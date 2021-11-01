######## NICHOLAS EAMES ###############

library(lmtest)
library(sandwich)
library(parameters)
library("readxl")

install.packages("tidyverse")

library(tidyverse)

The_DF <- data.frame(H1B_Disclosure_Data_FY2019)

DF1 <- The_DF %>%
  filter(CASE_STATUS %in% c("CERTIFIED", "DENIED"))

unique(DF1$CASE_STATUS)

class(DF1$CASE_STATUS)

DF2 <- DF1 %>%
  mutate(test = ifelse(CASE_STATUS=='CERTIFIED',0,1))
head(DF2)

Reg1 <- lm(test~WAGE_RATE_OF_PAY_FROM_1, DF3)
summary(Reg1)

names(DF1)

memory.limit()

names(DF2)

DF3 <- DF2[c(1:300),]

memory.limit(50000)

memory.limit()

memory.limit(75000)

memory.limit()

DF4 <- DF2[c(1:349515),]
DF5 <- DF2[c(1:50000),]
DF6 <- DF2[c(1:300),]



Reg2 <- lm(test~WAGE_RATE_OF_PAY_FROM_1, DF6)
summary(Reg2)

######################################################

DF_01 <- DF2 %>%
  mutate(test2 = ifelse(H.1B_DEPENDENT=='Y',0,1))
head(DF_01)

DF_01_1 <- DF_01[c(1:300),]

Reg3 <- lm(test~WAGE_RATE_OF_PAY_FROM_1 + test2, DF_01_1)
summary(Reg3)

Reg4 <- lm(test~test2, DF6)

Reg5 <- lm(test~WAGE_RATE_OF_PAY_FROM_1 + EMPLOYER_POSTAL_CODE, DF6)
summary(Reg5)

######## NICHOLAS EAMES ###########