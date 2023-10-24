
rm(list = ls())
library(tidyverse)
library(psych)
repdom3 <- read_csv("repdom3.csv")
repdom3 <- repdom3 %>%
  select(-(1:3))
repdom3<- repdom3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(repdom3)#verifique NANS
#repdom3<- subset(repdom3, select=-c(???))

farepdom3 <- fa(repdom3,nfactors = 3,rotate = "varimax")
farepdom3 $loadings
