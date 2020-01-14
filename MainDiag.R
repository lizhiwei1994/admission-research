library(feather)
library(readxl)
library(tidyverse)
library(lubridate)



path <- list.files('A:\\admission data\\original_data\\主要诊断', full.names = T)
dat <- lapply(path, read_xlsx) %>% 
  do.call('bind_rows', .)


# 基本数据清洗 ------------------------------------------------------------------


# 提取入院日期和时间
dat2 <- dat %>% separate(DATE_INHOSPITAL, c('日期','时间'), sep = ' ') %>% 
  mutate(日期 = as.Date(日期))
          
# 排除不在 2014.01.01 —— 2018.12.31 时间内的患者
begin <- as.Date('2014-1-1')
end <- as.Date('2018-12-31')
dat3 <- dat2 %>% filter(between(日期, begin, end))
e1.date <- nrow(dat) - nrow(dat3)

# 排除无性别患者 0-未知性别  1-男 2-女 9-未说明性别
dat4 <- dat3 %>% filter(GENDER %in% c(1,2))
e2.sex <- nrow(dat3) - nrow(dat4)

# 排除18岁以下患者
dat5 <- dat4 %>% filter(AGE > 18)
e3.age <- nrow(dat4) - nrow(dat5)

# 排除非京籍入院患者 (ADDR_ID 开头为110的是北京地区患者)
dat6 <- dat5 %>% filter(str_sub(ADDR_ID, 1, 3) == '110')
e4.addr <- nrow(dat5) - nrow(dat6)

# 排除无住院诊断的患者 (DISEASE_CODE1 是主要诊断的编码)
dat7 <- dat6 %>% filter(!is.na(DISEASE_CODE1))
e5.nodiag <- nrow(dat6) - nrow(dat7)

# 排除不是因为CVD入院的患者(DISEASE_CODE1 的开头字母为I,则是因为CVD入院)
dat8 <- dat7 %>% filter(str_sub(DISEASE_CODE1, 1, 1) == 'I') 
e6.notcvd <- nrow(dat7) - nrow(dat8)
  # 下边的代码可以查看各种疾病
  # dat7 %>% mutate(ICD = str_sub(DISEASE_CODE1, 1, 1)) %>% count(ICD) %>% arrange(desc(n))
text <- paste0('本研究初始收集到入院患者', nrow(dat), '人，',
               '排除不在研究时间范围内的患者（2014-01-01至2018-12-31）', e1.date, '人，', 
               '排除没有记录性别的患者', e2.sex,'人，',
               '排除18岁以下患者', e3.age, '人，',
               '排除无户籍以及非京籍就诊患者', e4.addr, '人，',
               '排除无主要诊断患者', e5.nodiag, '人，',
               '排除不是因心血管疾病入院患者', e6.notcvd,'人，',
               '最终本研究纳入' ,nrow(dat8), '名研究对象。')

dat8 <- dat8 %>% mutate(age65 = ifelse(AGE >= 65, 'up65', 'low65'),
                        GENDER = recode(GENDER, '1' = 'man', '2' = 'woman'),
                        日期 = as.character(日期),
                        ICD10 = str_sub(DISEASE_CODE1, 1, 3))


# 生成dm 和 nodm 数据库 ---------------------------------------------------------
# 导入指示变量数据库
ind.dm <- read_feather('A:\\admission data\\index.dm.feather') %>% select(2) %>% 
ind.nodm <- read_feather('A:\\admission data\\index.nodm.feather') %>% select(2)
ind.dm2 <- ind.dm %>% unlist()
ind.nodm2 <- ind.nodm %>% unlist()

dm <- dat8 %>% filter(UNICODE %in% ind.dm2)
nodm <- dat8 %>% filter(UNICODE %in% ind.nodm2)

 
# 个体数据到时间序列数据 -------------------------------------------------------------

# function 1 将个体数据转换为时间序列数据，包括总入院和分年龄性别入院
tsdata <- function(df) {
  # total
  test.1 <- df %>% mutate(日期 = as.character(日期)) %>% count(日期)
  
  # sex
  test.1.sex <- df %>% group_by(GENDER) %>% count(日期) %>% spread(GENDER, n)
  
  # age
  test.1.age <- df %>% group_by(age65) %>% count(日期) %>% spread(age65, n)
  
  # date sequence
  seq <- seq.Date(as.Date('2014-01-01'), as.Date('2018-12-31'), by = 1) %>% as.character()
  date.col <- data.frame(日期 = seq, stringsAsFactors = F)
  
  # merge them
  whole <- Reduce(left_join, list(date.col, test.1, test.1.age, test.1.sex)) %>% as_tibble()
}

# function 2 输入个体数
tsdata2.list.icd <- function(df){
  # 生成用于Merge 数据的时间序列数据
  seq <- seq.Date(as.Date('2014-01-01'), as.Date('2018-12-31'), by = 1) %>% as.character()
  # 将原始df根据ICD10编码分割为不同的小df，并储存在一个list里
 data.list <-  split(df, df$ICD10)
 
 data.merge <- data.list %>% 
   map(tsdata) %>% #将每个小df中的个体数据生成时间序列数据，包括总入院，分年龄性别入院
   do.call('cbind', .) %>%  # 将每个小df的数据横向合并为一个大的df
   mutate(日期 = seq) %>% # 生成一个新变量“日期”
   select(-contains('.日期')) %>% # 删除之前合并产生的多余日期变量
   select(one_of('日期'), everything()) # 将日期变量放在第一个
   
}

# function 3 : 输入个体数据,输出各个区的时间序列数据
tsdata3.list.icd.addr <- function(df){
  
  # 按照地区分割数据
  d1 <- dat8 %>% split(.$ADDR_ID)
  
  # 先按照ICD分割生成时间序列数据
  d2 <- map(d1, tsdata2.list.icd) 
  
  # 在时间序列后边添加一个from变量，表示这个时间序列数据来自哪个区
  d3 <- Map(cbind, d2, from = names(d2))
  
  # 将上边的list纵向合并为一个大df，bind_rows可以自动为不能匹配的变量生成NA
  d4 <- do.call('bind_rows', d3)
  
  #调整一下变量顺序
  num <- sprintf('%02d',0:99)
  index <- c('n', 'man', 'woman', 'low65', 'up65')
  vars <- with(expand.grid(index=index, num=num), paste0('I', num, '.', index))
  vars2 <- c('from', '日期', vars)
  
  d5 <- d4 %>% select(one_of(vars2)) %>% 
    replace(., is.na(.), 0) # 入院缺失改为0
}


dm.ts <- tsdata3.list.icd.addr(dm)
nodm.ts <- tsdata3.list.icd.addr(nodm)

write.csv(dm.ts, 'A:\\admission data\\dm.ts.csv', row.names = F)
write.csv(nodm.ts, 'A:\\admission data\\nodm.ts.csv', row.names = F)
