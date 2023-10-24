#
rm(list = ls())
library(tidyverse)
library(psych)

brasil2 <- read_csv("brasil2.csv")
brasil2 <- brasil2 %>%
  select(-(1:3))
brasil3 <- read_csv("brasil3.csv")
brasil3 <- brasil3 %>%
  select(-(1:3))
brasil5<- read_csv("brasil5.csv")
brasil5 <- brasil5 %>%
  select(-(1:3))
brasil6<- read_csv("brasil6.csv")
brasil6 <- brasil6 %>%
  select(-(1:3))
brasil7 <- read_csv("brasil7.csv")
brasil7 <- brasil7 %>%
  select(-(1:3))
summary(brasil2)
# Substitua todos os valores menores que 0 por NA
brasil2 <- brasil2 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil2)#verifique NANS
brasil2 <- subset(brasil2, select=-c(B008,E069_12))

brasil3 <- brasil3 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil3)#verifique NANS
#brasil2 <- subset(brasil2, select=-c(B008,E069_12))


brasil5<- brasil5 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil5)#verifique NANS
brasil5<- subset(brasil5, select=-c(E034))

brasil6<- brasil6 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil6)#verifique NANS
brasil6<- subset(brasil6, select=-c(E034))


brasil7<- brasil7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(brasil7)#verifique NANS
#brasil5<- subset(brasil5, select=-c(E034))



mirtbrasil2 <- mirt(brasil2,1)
summary(mirtbrasil2, rotate="varimax", suppress=.31)


mirtbrasil3 <- mirt(brasil3,1)
summary(mirtbrasil3, rotate="varimax", suppress=.31)


mirtbrasil5 <- mirt(brasil5,1)
summary(mirtbrasil5, rotate="varimax", suppress=.31)



mirtbrasil6 <- mirt(brasil6,1)
summary(mirtbrasil6, rotate="varimax", suppress=.31)


mirtbrasil7 <- mirt(brasil7,1)
summary(mirtbrasil7, rotate="varimax", suppress=.31)


