#Porto Rico

#portoRico

#
rm(list = ls())
library(tidyverse)
library(psych)


portoRico3 <- read_csv("portoRico3.csv")
portoRico3 <- portoRico3 %>%
  select(-(1:3))
portoRico4 <- read_csv("portoRico4.csv")
portoRico4 <- portoRico4 %>%
  select(-(1:3))


portoRico7 <- read_csv("portoRico7.csv")
portoRico7 <- portoRico7 %>%
  select(-(1:3))
# Substitua todos os valores menores que 0 por NA

portoRico3 <- portoRico3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(portoRico3)#verifique NANS

portoRico4 <- portoRico4 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(portoRico4)#verifique NANS
portoRico4 <- subset(portoRico4, select=-c(E069_17))




portoRico7<- portoRico7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(portoRico7)#verifique NANS



mirtportoRico3 <- mirt(portoRico3,1)
summary(mirtportoRico3, rotate="varimax", suppress=.31)

mirtportoRico4 <- mirt(portoRico4,1)
summary(mirtportoRico4, rotate="varimax", suppress=.31)

mirtportoRico7 <- mirt(portoRico7,1)
summary(mirtportoRico7, rotate="varimax", suppress=.31)


