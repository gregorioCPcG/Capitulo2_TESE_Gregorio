#
rm(list = ls())
library(tidyverse)
library(psych)

ven3 <- read_csv("ven3.csv")
ven3 <- ven3 %>%
  select(-(1:3))
ven4 <- read_csv("ven4.csv")
ven4 <- ven4 %>%
  select(-(1:3))

ven7 <- read_csv("ven7.csv")
ven7 <- ven7 %>%
  select(-(1:3))

# Substitua todos os valores menores que 0 por NA


ven3 <- ven3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(ven3)#verifique NANS
#ven2 <- subset(ven2, select=-c(B008,E069_12))

ven4 <- ven4 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(ven4)#verifique NANS
ven4 <- subset(ven4, select=-c(E069_17))




ven7<- ven7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(ven7)#verifique NANS
#ven5<- subset(ven5, select=-c(E034))


mirtven3 <- mirt(ven3,1)
summary(mirtven3, rotate="varimax", suppress=.31)

mirtven4 <- mirt(ven4,1)
summary(mirtven4, rotate="varimax", suppress=.31)

mirtven7 <- mirt(ven7,1)
summary(mirtven7, rotate="varimax", suppress=.31)
