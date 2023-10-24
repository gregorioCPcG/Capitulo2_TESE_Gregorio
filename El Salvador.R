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

faelSalvador3 <- fa(elSalvador3,nfactors = 3,rotate = "varimax")
faelSalvador3 $loadings

faelSalvador3 $loadings