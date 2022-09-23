install.packages("tidyverse")
install.packages("xlsx")
install.packages("MASS")

#infile
library(tidyverse)
bank <- read_csv("C:/Users/LimJaeSung/bank_data/data_038_1.csv")
nrow(bank)
ncol(bank)
head(bank)
summary(bank)

#schema infile
library(openxlsx)
library(gdata)
bank_name <- read.xlsx("C:/Users/LimJaeSung/bank_data/schema.xlsx", startRow = 11)
bank_name
head(bank_name)

bank_names <- bank_name[,6]
bank_names

#rename
library(MASS)
names(bank)
bank_ <- bank[,c(1:181)]
names(bank_) <- c(bank_names)
names(bank_)
View(bank_)


#logistic regression
new_bank <- bank_[,c(3,8:173)]
head(new_bank)

glm_bank <- glm(은행활동고객TF ~ ., data = new_bank, family = binomial)
summary(glm_bank)

#checking correlation matrix
corr <- cor(new_bank)
View(corr)

#checking colSums = 0
new_bank %>% summarize(혼수 = sum(금액_혼수전문점))
colSums(new_bank) == 0


#delete 혼수전문점
bank_del <- subset(new_bank, select = -금액_혼수전문점)
head(bank_del)
bank_del

#regression
glm_bank_del <- glm(은행활동고객TF ~ ., data = bank_del, family = binomial)
summary(glm_bank_del)

#checking correlation matrix
corr_del <- cor(bank_del)
View(corr_del)


#sample ver.
sample_bank_del <- bank_del[sample(nrow(bank_del),1000),]
sample_bank_del

nrow(sample_bank_del)
ncol(sample_bank_del)

#0 -> 1로 바꾸기
for(j in 2:ncol(sample_bank_del)){ 
  for(i in 1:nrow(sample_bank_del)){
    if(sample_bank_del[i,j] == 0){
      sample_bank_del[i,j] = 1
    }
  }
}  


head(sample_bank_del)
sample_bank_del
View(sample_bank_del)

colSums(sample_bank_del) == 0

glm_sample_bank_del <- glm(은행활동고객TF ~ ., data = sample_bank_del, family = binomial)
summary(glm_sample_bank_del)

sample_corr_del <- cor(sample_bank_del)
View(sample_corr_del)










#selecting
step_bank <- step(glm_bank, direction = "backward")

#이탈도 확인
anova(glm_bank, test = "Chisq")


step_bank <- step(glm_sample_bank, direction = "backward")

anova(glm_sample_bank, test = 'Chisq')

warnings()
