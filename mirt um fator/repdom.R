
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

mirtrepdom3 <- mirt(repdom3,1)
summary(mirtrepdom3, rotate="varimax", suppress=.31)
