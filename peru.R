#Peru

#
rm(list = ls())
library(tidyverse)
library(psych)


peru3 <- read_csv("peru3.csv")
peru3 <- peru3 %>%
  select(-(1:3))
peru4 <- read_csv("peru4.csv")
peru4 <- peru4 %>%
  select(-(1:3))

peru6<- read_csv("peru6.csv")
peru6 <- peru6 %>%
  select(-(1:3))
peru7 <- read_csv("peru7.csv")
peru7 <- peru7 %>%
  select(-(1:3))
# Substitua todos os valores menores que 0 por NA

peru3 <- peru3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(peru3)#verifique NANS

peru4 <- peru4 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(peru4)#verifique NANS
peru4 <- subset(peru4, select=-c(E069_17))

peru6<- peru6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(peru6)#verifique NANS
peru6<- subset(peru6, select=-c(E034))


peru7<- peru7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(peru7)#verifique NANS




faperu3 <- fa(peru3,nfactors = 3,rotate = "varimax")
faperu3 $loadings



faperu4 <- fa(peru4,nfactors = 3,rotate = "varimax")
faperu4 $loadings



faperu6 <- fa(peru6,nfactors = 3,rotate = "varimax")
faperu6 $loadings


faperu7 <- fa(peru7,nfactors = 3,rotate = "varimax")
faperu7 $loadings

