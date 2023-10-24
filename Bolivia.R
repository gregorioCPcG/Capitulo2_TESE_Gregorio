
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

fabolivia7 <- fa(bolivia7,nfactors = 3,rotate = "varimax")
fabolivia7 $loadings
