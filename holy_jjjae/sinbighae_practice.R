install.packages("tidyverse")
library(tidyverse)
bank <- read_csv("C:/Users/LimJaeSung/bank_data/data_038_1.csv")
nrow(bank)
ncol(bank)
head(bank)

summary(bank)

#logistic regression
new_bank <- bank[,c(3,8:174)]
head(new_bank)

glm_bank <- glm(P3 ~ ., data = new_bank, family = binomial)
summary(glm_bank) 

#selecting
step_bank <- step(glm_bank, direction = "backward")

#ÀÌÅ»µµ È®ÀÎ
anova(glm_bank, test = "Chisq")