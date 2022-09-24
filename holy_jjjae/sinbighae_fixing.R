#data
library(tidyverse)
bank <- read_csv("C:/Users/LimJaeSung/bank_data/data_038_1.csv")
head(bank)

library(openxlsx)
library(gdata)
bank_name <- read.xlsx("C:/Users/LimJaeSung/bank_data/schema.xlsx", startRow = 11)
bank_name
head(bank_name)

bank_names <- bank_name[,6]
bank_names

library(MASS)
names(bank)
bank_ <- bank[,c(1:181)]
names(bank_) <- c(bank_names)
names(bank_)
View(bank_)

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
install.packages("car")
library(car)
vif(glm_sample_bank_del) > 10
sample_bank_del_ <- subset(sample_bank_del, (vif(glm_sample_bank_del) > 10) == T)

if((vif(glm_sample_bank_del) > 10) == T){
  print(names(glm_sample_bank_del))
}

#regression``
glm_sample_bank_del_ <- glm(은행활동고객TF ~ ., data = sample_bank_del_, family = binomial)
summary(glm_sample_bank_del_)

step_bank <- step(glm_sample_bank_del, direction = "both")


#####일단 보류#########
#use fiited model to predict response values
sample_bank_del$y_pred <- predict(glm_sample_bank_del, sample_bank_del, type = "response")
sample_bank_del$y_pred
View(sample_bank_del)
names(sample_bank_del)

sample_bank_del_ <- subset(sample_bank_del, y_pred <0.999999999)
View(sample_bank_del_)
nrow(sample_bank_del_) 

corr <- cor(sample_bank_del)
View(corr)
