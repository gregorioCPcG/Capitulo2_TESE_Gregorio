#Uru
#
rm(list = ls())
library(tidyverse)
library(psych)


uru3 <- read_csv("uru3.csv")
uru3 <- uru3 %>%
  select(-(1:3))

uru5<- read_csv("uru5.csv")
uru5 <- uru5 %>%
  select(-(1:3))
uru6<- read_csv("uru6.csv")
uru6 <- uru6 %>%
  select(-(1:3))
uru7 <- read_csv("uru7.csv")
uru7 <- uru7 %>%
  select(-(1:3))
# Substitua todos os valores menores que 0 por NA


uru3 <- uru3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(uru3)#verifique NANS
#uru2 <- subset(uru2, select=-c(B008,E069_12))


uru5<- uru5 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(uru5)#verifique NANS
uru5<- subset(uru5, select=-c(E034))

uru6<- uru6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(uru6)#verifique NANS
uru6<- subset(uru6, select=-c(E034))


uru7<- uru7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(uru7)#verifique NANS
#uru5<- subset(uru5, select=-c(E034))


mirturu3 <- mirt(uru3,1)
summary(mirturu3, rotate="varimax", suppress=.31)


mirturu5 <- mirt(uru5,1)
summary(mirturu5, rotate="varimax", suppress=.31)


mirturu6 <- mirt(uru6,1)
summary(mirturu6, rotate="varimax", suppress=.31)

mirturu7 <- mirt(uru7,1)
summary(mirturu7, rotate="varimax", suppress=.31)

