library(feather)
library(readxl)
library(tidyverse)
library(lubridate)



path <- list.files('A:\\admission data\\original_data\\��Ҫ���', full.names = T)
dat <- lapply(path, read_xlsx) %>% 
  do.call('bind_rows', .)


# ����������ϴ ------------------------------------------------------------------


# ��ȡ��Ժ���ں�ʱ��
dat2 <- dat %>% separate(DATE_INHOSPITAL, c('����','ʱ��'), sep = ' ') %>% 
  mutate(���� = as.Date(����))
          
# �ų����� 2014.01.01 ���� 2018.12.31 ʱ���ڵĻ���
begin <- as.Date('2014-1-1')
end <- as.Date('2018-12-31')
dat3 <- dat2 %>% filter(between(����, begin, end))
e1.date <- nrow(dat) - nrow(dat3)

# �ų����Ա��� 0-δ֪�Ա�  1-�� 2-Ů 9-δ˵���Ա�
dat4 <- dat3 %>% filter(GENDER %in% c(1,2))
e2.sex <- nrow(dat3) - nrow(dat4)

# �ų�18�����»���
dat5 <- dat4 %>% filter(AGE > 18)
e3.age <- nrow(dat4) - nrow(dat5)

# �ų��Ǿ�����Ժ���� (ADDR_ID ��ͷΪ110���Ǳ�����������)
dat6 <- dat5 %>% filter(str_sub(ADDR_ID, 1, 3) == '110')
e4.addr <- nrow(dat5) - nrow(dat6)

# �ų���סԺ��ϵĻ��� (DISEASE_CODE1 ����Ҫ��ϵı���)
dat7 <- dat6 %>% filter(!is.na(DISEASE_CODE1))
e5.nodiag <- nrow(dat6) - nrow(dat7)

# �ų�������ΪCVD��Ժ�Ļ���(DISEASE_CODE1 �Ŀ�ͷ��ĸΪI,������ΪCVD��Ժ)
dat8 <- dat7 %>% filter(str_sub(DISEASE_CODE1, 1, 1) == 'I') 
e6.notcvd <- nrow(dat7) - nrow(dat8)
  # �±ߵĴ�����Բ鿴���ּ���
  # dat7 %>% mutate(ICD = str_sub(DISEASE_CODE1, 1, 1)) %>% count(ICD) %>% arrange(desc(n))
text <- paste0('���о���ʼ�ռ�����Ժ����', nrow(dat), '�ˣ�',
               '�ų������о�ʱ�䷶Χ�ڵĻ��ߣ�2014-01-01��2018-12-31��', e1.date, '�ˣ�', 
               '�ų�û�м�¼�Ա�Ļ���', e2.sex,'�ˣ�',
               '�ų�18�����»���', e3.age, '�ˣ�',
               '�ų��޻����Լ��Ǿ������ﻼ��', e4.addr, '�ˣ�',
               '�ų�����Ҫ��ϻ���', e5.nodiag, '�ˣ�',
               '�ų���������Ѫ�ܼ�����Ժ����', e6.notcvd,'�ˣ�',
               '���ձ��о�����' ,nrow(dat8), '���о�����')

dat8 <- dat8 %>% mutate(age65 = ifelse(AGE >= 65, 'up65', 'low65'),
                        GENDER = recode(GENDER, '1' = 'man', '2' = 'woman'),
                        ���� = as.character(����),
                        ICD10 = str_sub(DISEASE_CODE1, 1, 3))


# ����dm �� nodm ���ݿ� ---------------------------------------------------------
# ����ָʾ�������ݿ�
ind.dm <- read_feather('A:\\admission data\\index.dm.feather') %>% select(2) %>% 
ind.nodm <- read_feather('A:\\admission data\\index.nodm.feather') %>% select(2)
ind.dm2 <- ind.dm %>% unlist()
ind.nodm2 <- ind.nodm %>% unlist()

dm <- dat8 %>% filter(UNICODE %in% ind.dm2)
nodm <- dat8 %>% filter(UNICODE %in% ind.nodm2)

 
# �������ݵ�ʱ���������� -------------------------------------------------------------

# function 1 ����������ת��Ϊʱ���������ݣ���������Ժ�ͷ������Ա���Ժ
tsdata <- function(df) {
  # total
  test.1 <- df %>% mutate(���� = as.character(����)) %>% count(����)
  
  # sex
  test.1.sex <- df %>% group_by(GENDER) %>% count(����) %>% spread(GENDER, n)
  
  # age
  test.1.age <- df %>% group_by(age65) %>% count(����) %>% spread(age65, n)
  
  # date sequence
  seq <- seq.Date(as.Date('2014-01-01'), as.Date('2018-12-31'), by = 1) %>% as.character()
  date.col <- data.frame(���� = seq, stringsAsFactors = F)
  
  # merge them
  whole <- Reduce(left_join, list(date.col, test.1, test.1.age, test.1.sex)) %>% as_tibble()
}

# function 2 ���������
tsdata2.list.icd <- function(df){
  # ��������Merge ���ݵ�ʱ����������
  seq <- seq.Date(as.Date('2014-01-01'), as.Date('2018-12-31'), by = 1) %>% as.character()
  # ��ԭʼdf����ICD10����ָ�Ϊ��ͬ��Сdf����������һ��list��
 data.list <-  split(df, df$ICD10)
 
 data.merge <- data.list %>% 
   map(tsdata) %>% #��ÿ��Сdf�еĸ�����������ʱ���������ݣ���������Ժ���������Ա���Ժ
   do.call('cbind', .) %>%  # ��ÿ��Сdf�����ݺ���ϲ�Ϊһ�����df
   mutate(���� = seq) %>% # ����һ���±��������ڡ�
   select(-contains('.����')) %>% # ɾ��֮ǰ�ϲ������Ķ������ڱ���
   select(one_of('����'), everything()) # �����ڱ������ڵ�һ��
   
}

# function 3 : �����������,�����������ʱ����������
tsdata3.list.icd.addr <- function(df){
  
  # ���յ����ָ�����
  d1 <- dat8 %>% split(.$ADDR_ID)
  
  # �Ȱ���ICD�ָ�����ʱ����������
  d2 <- map(d1, tsdata2.list.icd) 
  
  # ��ʱ�����к������һ��from��������ʾ���ʱ���������������ĸ���
  d3 <- Map(cbind, d2, from = names(d2))
  
  # ���ϱߵ�list����ϲ�Ϊһ����df��bind_rows�����Զ�Ϊ����ƥ��ı�������NA
  d4 <- do.call('bind_rows', d3)
  
  #����һ�±���˳��
  num <- sprintf('%02d',0:99)
  index <- c('n', 'man', 'woman', 'low65', 'up65')
  vars <- with(expand.grid(index=index, num=num), paste0('I', num, '.', index))
  vars2 <- c('from', '����', vars)
  
  d5 <- d4 %>% select(one_of(vars2)) %>% 
    replace(., is.na(.), 0) # ��Ժȱʧ��Ϊ0
}


dm.ts <- tsdata3.list.icd.addr(dm)
nodm.ts <- tsdata3.list.icd.addr(nodm)

write.csv(dm.ts, 'A:\\admission data\\dm.ts.csv', row.names = F)
write.csv(nodm.ts, 'A:\\admission data\\nodm.ts.csv', row.names = F)