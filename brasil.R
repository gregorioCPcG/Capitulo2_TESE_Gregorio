#
rm(list = ls())
library(tidyverse)
library(psych)

brasil2 <- read_csv("brasil2.csv")
brasil2 <- brasil2 %>%
  select(-(1:3))
brasil3 <- read_csv("brasil3.csv")
brasil3 <- brasil3 %>%
  select(-(1:3))
brasil5<- read_csv("brasil5.csv")
brasil5 <- brasil5 %>%
  select(-(1:3))
brasil6<- read_csv("brasil6.csv")
brasil6 <- brasil6 %>%
  select(-(1:3))
brasil7 <- read_csv("brasil7.csv")
brasil7 <- brasil7 %>%
  select(-(1:3))
summary(brasil2)
# Substitua todos os valores menores que 0 por NA
brasil2 <- brasil2 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil2)#verifique NANS
brasil2 <- subset(brasil2, select=-c(B008,E069_12))

brasil3 <- brasil3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil3)#verifique NANS
#brasil2 <- subset(brasil2, select=-c(B008,E069_12))


brasil5<- brasil5 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil5)#verifique NANS
brasil5<- subset(brasil5, select=-c(E034))

brasil6<- brasil6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil6)#verifique NANS
brasil6<- subset(brasil6, select=-c(E034))


brasil7<- brasil7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil7)#verifique NANS
#brasil5<- subset(brasil5, select=-c(E034))



fabrasil2 <- fa(brasil2,nfactors = 3,rotate = "varimax")
fabrasil2 $loadings


fabrasil3 <- fa(brasil3,nfactors = 3,rotate = "varimax")
fabrasil3 $loadings



fabrasil5 <- fa(brasil5,nfactors = 3,rotate = "varimax")
fabrasil5 $loadings



fabrasil6 <- fa(brasil6,nfactors = 3,rotate = "varimax")
fabrasil6 $loadings


fabrasil7 <- fa(brasil7,nfactors = 3,rotate = "varimax")
fabrasil7 $loadings

