#
rm(list = ls())
library(tidyverse)
library(psych)

mexico2 <- read_csv("mexico2.csv")
mexico2 <- mexico2 %>%
  select(-(1:3))
mexico3 <- read_csv("mexico3.csv")
mexico3 <- mexico3 %>%
  select(-(1:3))
mexico4 <- read_csv("mexico4.csv")
mexico4 <- mexico4 %>%
  select(-(1:3))
mexico5<- read_csv("mexico5.csv")
mexico5 <- mexico5 %>%
  select(-(1:3))
mexico6<- read_csv("mexico6.csv")
mexico6 <- mexico6 %>%
  select(-(1:3))
mexico7 <- read_csv("mexico7.csv")
mexico7 <- mexico7 %>%
  select(-(1:3))
summary(mexico2)
# Substitua todos os valores menores que 0 por NA
mexico2 <- mexico2 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(mexico2)#verifique NANS
mexico2 <- subset(mexico2, select=-c(B008))

mexico3 <- mexico3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(mexico3)#verifique NANS
#mexico2 <- subset(mexico2, select=-c(B008,E069_12))

mexico4 <- mexico4 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(mexico4)#verifique NANS
mexico4 <- subset(mexico4, select=-c(E069_17))

mexico5<- mexico5 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(mexico5)#verifique NANS
mexico5<- subset(mexico5, select=-c(E034))

mexico6<- mexico6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(mexico6)#verifique NANS
mexico6<- subset(mexico6, select=-c(E034))


mexico7<- mexico7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(mexico7)#verifique NANS
#mexico5<- subset(mexico5, select=-c(E034))



mirtmexico2 <- mirt(mexico2,1)
summary(mirtmexico2, rotate="varimax", suppress=.31)
mirtmexico3 <- mirt(mexico3,1)
summary(mirtmexico3, rotate="varimax", suppress=.31)
mirtmexico4 <- mirt(mexico4,1)
summary(mirtmexico4, rotate="varimax", suppress=.31)
mirtmexico5 <- mirt(mexico5,1)
summary(mirtmexico5, rotate="varimax", suppress=.31)
mirtmexico6 <- mirt(mexico6,1)
summary(mirtmexico6, rotate="varimax", suppress=.31)
mirtmexico7 <- mirt(mexico7,1)
summary(mirtmexico7, rotate="varimax", suppress=.31)
