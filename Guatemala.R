#Guatemala
rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
guatemala <- read_csv("novapasta/guatemala.csv")
#guat5

guat05 <- guatemala %>%
  filter(S002VS == 5)
guat05d <- guat05[,26:44]
summary(guat05d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
guat05m <- subset(guat05d, select=-c(B006,F141,F144_02, E034))#PERGUNTAS NAO FEITAS
summary(guat05m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
guat05d <- subset(guat05d, select=-c(B006,F141, F144_02,E034))%>% na.omit()
summary(guat05d)
guat07 <- guatemala %>%
  filter(S002VS == 7)
guat07d <- guat07[,26:44]
summary(guat07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
guat07m <- subset(guat07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(guat07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
guat07d <- subset(guat07d, select=-c(B006, F141))%>% na.omit()
summary(guat07d)

#
#guat05d
KMO(guat05d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(guat05d) #
nfactors(guat05d, rotate = "varimax")
guat5fact1 <- fa(guat05d,nfactors=1,rotate = "varimax")
guat5fact1$loadings
guat5fact2 <- fa(guat05d,nfactors=2,rotate = "varimax")
guat5fact2$loadings
guat5fact3 <- fa(guat05d,nfactors=3,rotate = "varimax")
guat5fact3$loadings
guat5fact4 <- fa(guat05d,nfactors=4,rotate = "varimax")
guat5fact4$loadings


#guat07d
KMO(guat07d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(guat07d) #
nfactors(guat07d, rotate = "varimax")
guat7fact1 <- fa(guat07d,nfactors=1,rotate = "varimax")
guat7fact1$loadings
guat7fact2 <- fa(guat07d,nfactors=2,rotate = "varimax")
guat7fact2$loadings
guat7fact3 <- fa(guat07d,nfactors=3,rotate = "varimax")
guat7fact3$loadings
guat7fact4 <- fa(guat07d,nfactors=4,rotate = "varimax")
guat7fact4$loadings

