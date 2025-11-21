library(tidyverse)
library(data.table)
require(vcd)

# import data----
import_data <- fread("C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Strategic Marketing for Decision Making/Coursepack/UVA-QA-0806X.csv")

import_data[import_data == '-'] <- 0

import_data <- 
  import_data %>% 
  mutate_if(is.character, as.numeric)

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
  mutate(Age_level = ifelse(`Customer Age` <= 5, '0~5', 
                            ifelse(`Customer Age` >=6 & `Customer Age` <= 10, '6~10',
                                   ifelse(`Customer Age` >=11 & `Customer Age` <= 15, '11~15',
                                          ifelse(`Customer Age` >=16 & `Customer Age` <= 20, '16~20',
                                                 ifelse(`Customer Age` >= 21 & `Customer Age` <= 25, '21~25',
                                                        ifelse(`Customer Age` >= 26 & `Customer Age` <= 30, '26~30', '>30'))))))
  )

import_data_b1$Age_level <- 
  factor(import_data_b1$Age_level, levels = c('0~5','6~10','11~15','16~20','21~25','26~30','>30'), order = T)

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

predict(mylogit, newdata = import_data[672,], type = "response")
mylogit$fitted.values[672]

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












