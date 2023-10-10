#Nicaragua
rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
nicaragua <- read_csv("novapasta/nicaragua.csv")
table(nicaragua$S002VS)#seven


#onda 7

nicaraguad <- nicaragua[,26:44]
summary(nicaraguad)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
nicaraguam <- subset(nicaraguad, select=-c(B006,F141))#PERGUNTAS NAO FEITAS
summary(nicaraguam)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
nicaraguad <- subset(nicaraguad, select=-c(B006,F141))%>% na.omit()
summary(nicaraguad)

KMO(nicaraguad)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(nicaraguad) #
nfactors(nicaraguad, rotate = "varimax")
nic7fact1 <- fa(nicaraguad,nfactors=1,rotate = "varimax")
nic7fact1$loadings
nic7fact2 <- fa(nicaraguad,nfactors=2,rotate = "varimax")
nic7fact2$loadings
nic7fact3 <- fa(nicaraguad,nfactors=3,rotate = "varimax")
nic7fact3$loadings
nic7fact4 <- fa(nicaraguad,nfactors=4,rotate = "varimax")
nic7fact4$loadings

