#
rm(list = ls())
library(tidyverse)
library(mirt)

arg2 <- read_csv("arg2.csv")
arg2 <- arg2 %>%
  select(-(1:3))
arg3 <- read_csv("arg3.csv")
arg3 <- arg3 %>%
  select(-(1:3))
arg4 <- read_csv("arg4.csv")
arg4 <- arg4 %>%
  select(-(1:3))
arg5<- read_csv("arg5.csv")
arg5 <- arg5 %>%
  select(-(1:3))
arg6<- read_csv("arg6.csv")
arg6 <- arg6 %>%
  select(-(1:3))
arg7 <- read_csv("arg7.csv")
arg7 <- arg7 %>%
  select(-(1:3))
summary(arg2)
# Substitua todos os valores menores que 0 por NA
arg2 <- arg2 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(arg2)#verifique NANS
arg2 <- subset(arg2, select=-c(B008,E069_12))

arg3 <- arg3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(arg3)#verifique NANS
#arg2 <- subset(arg2, select=-c(B008,E069_12))

arg4 <- arg4 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(arg4)#verifique NANS
arg4 <- subset(arg4, select=-c(E069_17))

arg5<- arg5 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(arg5)#verifique NANS
arg5<- subset(arg5, select=-c(E034))

arg6<- arg6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(arg6)#verifique NANS
arg6<- subset(arg6, select=-c(E034))


arg7<- arg7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(arg7)#verifique NANS
#arg5<- subset(arg5, select=-c(E034))



mirtarg2 <- mirt(arg2,1)
summary(mirtarg2, rotate="varimax", suppress=.31)


mirtarg3 <- mirt(arg3,1)
summary(mirtarg3, rotate="varimax", suppress=.31)



mirtarg4 <- mirt(arg4,1)
summary(mirtarg4, rotate="varimax", suppress=.31)



mirtarg5 <- mirt(arg5,1)
summary(mirtarg5, rotate="varimax", suppress=.31)




mirtarg6 <- mirt(arg6,1)
summary(mirtarg6, rotate="varimax", suppress=.31)



mirtarg7 <- mirt(arg7,1)
summary(mirtarg7, rotate="varimax", suppress=.31)


