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




faportoRico3 <- fa(portoRico3,nfactors = 3,rotate = "varimax")
faportoRico3 $loadings



faportoRico4 <- fa(portoRico4,nfactors = 3,rotate = "varimax")
faportoRico4 $loadings



faportoRico7 <- fa(portoRico7,nfactors = 3,rotate = "varimax")
faportoRico7 $loadings

