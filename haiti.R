#Haiti
rm(list = ls())
library(tidyverse)
library(psych)
haiti6 <- read_csv("haiti6.csv")
haiti6 <- haiti6 %>%
  select(-(1:3))
haiti6<- haiti6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(haiti6)#verifique NANS
haiti6<- subset(haiti6, select=-c(E034))

fahaiti6 <- fa(haiti6,nfactors = 3,rotate = "varimax")
fahaiti6 $loadings
