#
rm(list = ls())
library(tidyverse)
library(psych)


equad6<- read_csv("equad6.csv")
equad6 <- equad6 %>%
  select(-(1:3))
equad7 <- read_csv("equad7.csv")
equad7 <- equad7 %>%
  select(-(1:3))

equad6<- equad6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(equad6)#verifique NANS
equad6<- subset(equad6, select=-c(E034))


equad7<- equad7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(equad7)#verifique NANS
#equad5<- subset(equad5, select=-c(E034))





faequad6 <- fa(equad6,nfactors = 3,rotate = "varimax")
faequad6 $loadings


faequad7 <- fa(equad7,nfactors = 3,rotate = "varimax")
faequad7 $loadings

