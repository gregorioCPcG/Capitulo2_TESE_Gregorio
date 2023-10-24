#chile
rm(list = ls())
library(tidyverse)
library(psych)
library(mirt)
mirtCluster()
chile2 <- read_csv("chile2.csv")
chile2 <- chile2 %>%
  select(-(1:3))
chile3 <- read_csv("chile3.csv")
chile3 <- chile3 %>%
  select(-(1:3))
chile4 <- read_csv("chile4.csv")
chile4 <- chile4 %>%
  select(-(1:3))
chile5<- read_csv("chile5.csv")
chile5 <- chile5 %>%
  select(-(1:3))
chile6<- read_csv("chile6.csv")
chile6 <- chile6 %>%
  select(-(1:3))
chile7 <- read_csv("chile7.csv")
chile7 <- chile7 %>%
  select(-(1:3))
summary(chile2)
# Substitua todos os valores menores que 0 por NA
chile2 <- chile2 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(chile2)#verifique NANS
chile2 <- subset(chile2, select=-c(B008))

chile3 <- chile3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(chile3)#verifique NANS
#chile2 <- subset(chile2, select=-c(B008,E069_12))

chile4 <- chile4 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(chile4)#verifique NANS
chile4 <- subset(chile4, select=-c(E069_17))

chile5<- chile5 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(chile5)#verifique NANS
chile5<- subset(chile5, select=-c(E034))

chile6<- chile6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(chile6)#verifique NANS
chile6<- subset(chile6, select=-c(E034))


chile7<- chile7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(chile7)#verifique NANS
#chile5<- subset(chile5, select=-c(E034))



mirtchile2 <- mirt(chile2,1)
summary(mirtchile2, rotate="varimax", suppress=.31)


mirtchile3 <- mirt(chile3,1)
summary(mirtchile3, rotate="varimax", suppress=.31)


mirtchile4 <- mirt(chile4,1)
summary(mirtchile4, rotate="varimax", suppress=.31)


mirtchile5 <- mirt(chile5,1)
summary(mirtchile5, rotate="varimax", suppress=.31)



mirtchile6 <- mirt(chile6,1)
summary(mirtchile6, rotate="varimax", suppress=.31)


mirtchile7 <- mirt(chile7,1)
summary(mirtchile7, rotate="varimax", suppress=.31)
