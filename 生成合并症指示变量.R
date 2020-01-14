# import data
# creat blank df
library(readxl)
library(tidyverse)
library(feather)
# 测试函数 成功运行
# test <- read_xlsx('A:\\admission data\\original_data\\其他诊断/2018其他诊断.xlsx')
# index <- (ncol(test) + 1)/6 - 1
# data0 <- matrix(nrow = 0, ncol = 5)
# test <- as.matrix(test)
# for (i in 0:index) {
#   data1 <- test[, (1:5) + i*6]
#   data0 <- rbind(data1,data0)
# }
# test1 <- test[1:5, ]
# test2 <- test[6:10, ]
# test3 <- test[11:15, ]
# test.list <- list(test1, test2, test3)
# d <- qitazhenduan(test1)
# d2 <- map(test.list, qitazhenduan)
# names <- c('number', 'diag', 'chinese', 'status', 'UNICODE')
# d3 <- do.call('rbind',d2) %>% as.data.frame() %>% set_names(names) %>% select(one_of('diag', 'UNICODE'))



qitazhenduan <- function(df) {
  data0 <- matrix(nrow = 0, ncol = 5)
  index <- (ncol(df) + 1)/6 - 1
  
  df <- as.matrix(df)
  for (i in 0:index) {
    data1 <- df[, (1:5) + i*6]
    data0 <- rbind(data1,data0)
  }
  data00 <- data0
}

path.qita <- list.files('A:\\admission data\\original_data\\其他诊断', full.names = T)
data <- lapply(path.qita, read_xlsx)

names <- c('number', 'diag', 'chinese', 'status', 'UNICODE')

data.qita <- map(data,qitazhenduan) %>% asdasdasda
                          do.call('rbind', .) %>% 
                          as.data.frame() %>% 
                          set_names(names) %>% 
                          select(one_of('diag', 'UNICODE'))
# write_feather(data.qita, 'A:\\admission data/index.feather') # 输出用于判断患者是否合并糖尿病的数据库


# 区分合不合并糖尿病人群 -------------------------------------------------------------

data.qita <- read_feather('A:\\admission data/index.feather')
data.qita <- mutate_at(data.qita, vars(1:2), as.character) # 将变量类型转换为字符型，便于后续筛选
index.dm <- data.qita %>% 
            group_by(UNICODE) %>% 
            filter(str_sub(diag,1,3) == 'E11') %>%  # 提取诊断编码前三位，如果诊断编码前三位为E11,则定义为合并dm
            distinct(UNICODE, .keep_all = T)

d1 <- data.qita %>% distinct(UNICODE, .keep_all = T) #去重数据库

index.nodm <- d1 %>% anti_join(index.dm,by = 'UNICODE')

# 检验结果是否正确
# p1 <- index.dm %>% mutate(letter = str_sub(diag, 1, 3)) # 看一下dm中是否全是E11
# table(p1$letter)
# p2 <- index.nodm %>% mutate(letter = str_sub(diag, 1, 1)) # 看一下nodm中是否全不是E11
# table(p2$letter)
# p22 <- p2 %>% filter(str_sub(diag, 1, 1) == 'E') %>% arrange(diag)
# head(p22)
# tail(p22) # 结果正确


# 其他方法计算不合并糖尿病人群
# index.nodm2 <- data.qita %>% group_by(UNICODE) %>% 
#   filter(!any(grepl('E11', diag))) %>% 
#   distinct(UNICODE, .keep_all = T)
# index.nodm3 <- data.qita %>% group_by(UNICODE) %>% 
#   mutate(diag2 = str_sub(diag, 1, 3)) %>% 
#   summarise(dm = any(grepl('E11', diag2))) %>% 
#   filter(dm == F)


# write_feather(index.dm, 'A:\\admission data/index.dm.feather')
# write_feather(index.nodm, 'A:\\admission data/index.nodm.feather')
# 原始数据库中合并dm的有 206,2452 人
#           不合并dm的有 30,9208  人