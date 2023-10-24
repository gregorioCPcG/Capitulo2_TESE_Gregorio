#
rm(list = ls())
library(tidyverse)
library(psych)

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



faarg2 <- fa(arg2,nfactors = 3,rotate = "varimax")
faarg2 $loadings


faarg3 <- fa(arg3,nfactors = 3,rotate = "varimax")
faarg3 $loadings



faarg4 <- fa(arg4,nfactors = 3,rotate = "varimax")
faarg4 $loadings


faarg5 <- fa(arg5,nfactors = 3,rotate = "varimax")
faarg5 $loadings



faarg6 <- fa(arg6,nfactors = 3,rotate = "varimax")
faarg6 $loadings


faarg7 <- fa(arg7,nfactors = 3,rotate = "varimax")
faarg7 $loadings

