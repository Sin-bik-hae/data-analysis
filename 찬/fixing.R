#data
library(tidyverse)
bank <- read_csv("C:/Users/alllh/Documents/대학/공모전-신.빅.해/data_038_1.csv")
head(bank)

library(openxlsx)
library(gdata)
bank_name <- read.xlsx("C:/Users/alllh/Documents/대학/공모전-신.빅.해/schema.xlsx", startRow = 11)
bank_name
head(bank_name)

bank_names <- bank_name[,6]
bank_names

library(MASS)
names(bank)
bank_ <- bank[,c(1:181)]
names(bank_) <- c(bank_names)
names(bank_)

#regression P3~B.
new_bank <- bank_[,c(3,8:173)]
head(new_bank)

glm_bank <- glm(은행활동고객TF ~ ., data = new_bank, family = binomial)
summary(glm_bank)

#deleting
colSums(new_bank) == 0

bank_del <- subset(new_bank, select = -금액_혼수전문점)
ncol(bank_del)

#regression using deleting data
glm_bank_del <- glm(은행활동고객TF ~., data = bank_del, family = binomial)
summary(glm_bank_del)

n_variable = rep(0, 166)
names(n_variable) = names(bank_del)
n_variable

#sample ver.
sample_bank_del <- bank_del[sample(nrow(bank_del),1000),]
sample_bank_del
View(sample_bank_del)
sum(colSums(sample_bank_del) == 0)

sample_bank_del <- sample_bank_del[colSums(sample_bank_del) != 0]
sample_bank_del_name <- names(sample_bank_del)
ncol(sample_bank_del)

glm_sample_bank_del <- glm(은행활동고객TF ~ ., data = sample_bank_del, family = binomial)
summary(glm_sample_bank_del)

#multicolinearity
# install.packages("car")
library(car)
sample_bank_del_ <- cbind(sample_bank_del["은행활동고객TF"], sample_bank_del[-c(T, vif(glm_sample_bank_del) > 10)])

glm_sample_bank_del <- glm(은행활동고객TF ~ ., data = sample_bank_del_, family = binomial)

#regression``
glm_sample_bank_del_ <- glm(은행활동고객TF ~ ., data = sample_bank_del_, family = binomial)
summary(glm_sample_bank_del_)

step_bank <- step(glm_sample_bank_del, direction = "both")
