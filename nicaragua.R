
rm(list = ls())
library(tidyverse)
library(psych)
nic7 <- read_csv("nic7.csv")
nic7 <- nic7 %>%
  select(-(1:3))
nic7<- nic7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(nic7)#verifique NANS
#nic7<- subset(nic7, select=-c(???))

fanic7 <- fa(nic7,nfactors = 3,rotate = "varimax")
fanic7 $loadings
