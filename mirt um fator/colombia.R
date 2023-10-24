#colomb
rm(list = ls())
library(tidyverse)
library(psych)
colomb7 <- read_csv("colomb7.csv")
colomb7 <- colomb7 %>%
  select(-(1:3))
colomb7<- colomb7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(colomb7)#verifique NANS
#colomb5<- subset(colomb5, select=-c(E034))

mirtcolomb7 <- mirt(colomb7,1)
summary(mirtcolomb7, rotate="varimax", suppress=.31)
