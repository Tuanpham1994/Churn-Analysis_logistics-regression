library(tidyverse)
library(data.table)
require(vcd)

# import data----
import_data <- fread("C:/Users/mark9/Desktop/2_Strategic Marketing Decision Making/0_Group Assignment/assignment 2/UVA-QA-0806X.csv")

import_data[import_data == '-'] <- 0

import_data <- 
  import_data %>% 
  mutate_if(is.character, as.numeric) %>% 
  arrange(ID)

# Q1 - a----
import_data_Q1 <- 
  import_data %>% 
  select(`Customer Age`, Churn) %>% 
  group_by(`Customer Age`, Churn) %>% 
  mutate(Churn_cnt = n()) %>% 
  ungroup() %>% 
  group_by(`Customer Age`) %>% 
  mutate(age_cnt = n()) %>% 
  ungroup() %>% 
  unique() %>% 
  mutate(percent = Churn_cnt / age_cnt) %>% 
  arrange(desc(`Customer Age`,Churn))
  
plotTenure_a <- 
  import_data_Q1 %>%
  mutate(Churn = Churn %>% factor(labels = c("No", "Yes"))) %>%
  ggplot(aes(y = percent, x = `Customer Age`,fill = factor(Churn))) +
  geom_bar(stat = "identity") +
  facet_grid( ~ Churn) +
  theme(legend.position = "none")
plotTenure_a

# Q1 - b----
import_data_b1 <- 
  import_data %>% 
  mutate(Age_level = ifelse(`Customer Age` <= 3, '0~3', 
                            ifelse(`Customer Age` >=4 & `Customer Age` <= 7, '4~7',
                                   ifelse(`Customer Age` >=8 & `Customer Age` <= 11, '8~11',
                                          ifelse(`Customer Age` >=12 & `Customer Age` <= 15, '12~15',
                                                 ifelse(`Customer Age` >= 16 & `Customer Age` <= 19, '16~19',
                                                        ifelse(`Customer Age` >= 20 & `Customer Age` <= 23, '20~23', 
                     ifelse(`Customer Age` >= 24 & `Customer Age` <= 27, '24~27',
                            ifelse(`Customer Age` >= 28 & `Customer Age` <= 31, '28~31',
                                   ifelse(`Customer Age` >= 32 & `Customer Age` <= 35, '32~35',
                                          ifelse(`Customer Age` >= 36 & `Customer Age` <= 39, '36~39',
                     ifelse(`Customer Age` >= 40 & `Customer Age` <= 43, '40~43',
                            ifelse(`Customer Age` >= 44 & `Customer Age` <= 47, '44~47',
                                   ifelse(`Customer Age` >= 48 & `Customer Age` <= 51, '48~51',
                                          ifelse(`Customer Age` >= 52 & `Customer Age` <= 55, '52~55',
                     ifelse(`Customer Age` >= 56 & `Customer Age` <= 59, '56~59','>=60')))))))))))))))
  )

import_data_b1$Age_level <- 
  factor(import_data_b1$Age_level, 
         levels = c('0~3','4~7','8~11','12~15','16~19','20~23',
                    '24~27','28~31','32~35','36~39','40~43',
                    '44~47','48~51','52~55','56~59','>=60'), order = T)

import_data_b2 <- 
  import_data %>% 
  mutate(Age_level = ifelse(`Customer Age` < 6, '<6', 
                            ifelse(`Customer Age` >=6 & `Customer Age` <= 14, '6~14', '>14')
  ))

import_data_b2$Age_level <- 
  factor(import_data_b2$Age_level, levels = c("<6","6~14",">14"), order = T)

doubledecker(Churn ~ ., data = import_data_b1 %>% select(Age_level, Churn))
doubledecker(Churn ~ ., data = import_data_b2 %>% select(Age_level, Churn))

# Q1 - c----
data_Q1_c1 <- 
  import_data_Q1 %>%
  mutate(Churn = Churn %>% factor(labels = c("No", "Yes"))) %>%
  filter(Churn == "No")

data_Q1_c2 <- 
  import_data_Q1 %>%
  mutate(Churn = Churn %>% factor(labels = c("No", "Yes"))) %>%
  filter(Churn == "Yes")

plotTenure_c1 <- 
  data_Q1_c1 %>% 
  ggplot(aes(y = Churn_cnt, x = `Customer Age`,fill = factor(Churn))) +
  geom_point() +
  geom_smooth(data = data_Q1_c1, size = 1, method = 'loess')

plotTenure_c2 <- 
  data_Q1_c2 %>% 
  ggplot(aes(y = Churn_cnt, x = `Customer Age`,fill = factor(Churn))) +
  geom_point() +
  geom_smooth(data = data_Q1_c2, size = 1, method = 'loess')

# plotTenure_d1 <- 
#   data_Q1_c1 %>% 
#   ggplot(aes(y = percent, x = `Customer Age`,fill = factor(Churn))) +
#   geom_point() +
#   geom_smooth(data = data_Q1_c1, size = 1, method = 'loess') +
#   scale_x_continuous(limits = c(0,70))
# 
# plotTenure_d2 <- 
#   data_Q1_c2 %>% 
#   ggplot(aes(y = percent, x = `Customer Age`,fill = factor(Churn))) +
#   geom_point() +
#   geom_smooth(data = data_Q1_c2, size = 1, method = 'loess') +
#   scale_x_continuous(limits = c(0,70))

data_Q1_d <- import_data_Q1 %>% mutate(Churn = Churn %>% factor(labels = c("No", "Yes")))

plotTenure_d1 <- 
  data_Q1_d %>% 
  ggplot(aes(y = percent, x = `Customer Age`, fill = factor(Churn))) +
  geom_point() +
  geom_smooth(data = data_Q1_d, size = 1, method = 'loess') +
  facet_grid(Churn ~., scales='free')
plotTenure_d1

plotTenure_d2 <- 
  data_Q1_d %>% 
  ggplot(aes(y = Churn_cnt, x = `Customer Age`, fill = factor(Churn))) +
  geom_point() +
  geom_smooth(data = data_Q1_d, size = 1, method = 'loess') +
  facet_grid(Churn ~., scales='free')
plotTenure_d2

# Q2----
glm(Churn ~ `Customer Age`, data = import_data, family = "binomial")

mylogit <- glm(Churn ~ .-ID, data = import_data, family = "binomial")
summary(mylogit)

# predict(mylogit, newdata = import_data[672,], type = "response")
mylogit$fitted.values[672]
import_data[672]

mylogit$fitted.values[354]
import_data[354]

mylogit$fitted.values[5203]
import_data[5203]

v.prd <- mylogit$fitted.values
v.prd <- sort(v.prd, decreasing = TRUE)
v.TopChurn <- v.prd[1:100]

# Q3----
typeof(names(v.TopChurn))
v.slct <- as.numeric(names(v.TopChurn))

df.slct <- import_data[v.slct,]
df.slct <- df.slct[,-c(1,3)]

# tmpr.u <- abs(df.slct[i,]*mylogit$coefficients[2:12]) # no.8 customer
# df[i,] <- names(sort(tmpr.u, decreasing = T))[1:3]

z <- dim(df.slct)[1]
df <- data.frame('1st' = rep(0, z), '2nd' = rep(0, z), '3rd' = rep(0, z))

for(i in 1:100){
  tmpr.u <- abs(df.slct[i,]*mylogit$coefficients[2:12])
  df[i,] <- names(sort(tmpr.u, decreasing = TRUE))[1:3]
}

df

table(df[,1])
table(c(df[,1], df[,2], df[,3]))












