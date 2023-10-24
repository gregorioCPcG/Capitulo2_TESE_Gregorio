
rm(list = ls())
library(tidyverse)
library(psych)
bolivia7 <- read_csv("bolivia7.csv")
bolivia7 <- bolivia7 %>%
  select(-(1:3))
bolivia7<- bolivia7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(bolivia7)#verifique NANS
#bolivia7<- subset(bolivia7, select=-c(???))

mirtbolivia7 <- mirt(bolivia7,1)
summary(mirtbolivia7, rotate="varimax", suppress=.31)
