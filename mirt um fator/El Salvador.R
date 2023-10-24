rm(list = ls())
library(tidyverse)
library(psych)
elSalvador3 <- read_csv("elSalvador3.csv")
elSalvador3 <- elSalvador3 %>%
  select(-(1:3))
elSalvador3<- elSalvador3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(elSalvador3)#verifique NANS
elSalvador3<- subset(elSalvador3, select=-c(C002))

mirtelSalvador3 <- mirt(elSalvador3,1)
summary(mirtelSalvador3, rotate="varimax", suppress=.31)

