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





faven3 <- fa(ven3,nfactors = 3,rotate = "varimax")
faven3 $loadings



faven4 <- fa(ven4,nfactors = 3,rotate = "varimax")
faven4 $loadings






faven7 <- fa(ven7,nfactors = 3,rotate = "varimax")
faven7 $loadings

