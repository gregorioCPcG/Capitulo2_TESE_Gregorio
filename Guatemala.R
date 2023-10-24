#
rm(list = ls())
library(tidyverse)
library(psych)


guat5<- read_csv("guat5.csv")
guat5 <- guat5 %>%
  select(-(1:3))
guat7 <- read_csv("guat7.csv")
guat7 <- guat7 %>%
  select(-(1:3))

guat5<- guat5 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(guat5)#verifique NANS
guat5<- subset(guat5, select=-c(E034,E069_17))


guat7<- guat7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(guat7)#verifique NANS
#guat5<- subset(guat5, select=-c(E034))





faguat5 <- fa(guat5,nfactors = 3,rotate = "varimax")
faguat5 $loadings


faguat7 <- fa(guat7,nfactors = 3,rotate = "varimax")
faguat7 $loadings
