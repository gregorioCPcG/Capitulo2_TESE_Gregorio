#
rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
#
#EQUADOR
equador <- read_csv("novapasta/equador.csv")
equ06 <- equador %>%
  filter(S002VS == 6)
equ06d <- equ06[,26:44]
summary(equ06d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
equ06m <- subset(equ06d, select=-c(B006, F141, F144_02, E034))#PERGUNTAS NAO FEITAS
summary(equ06m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
equ06d <- subset(equ06d, select=-c(B006, F141, F144_02, E034))%>% na.omit()
summary(equ06d)

equ07 <- equador %>%
  filter(S002VS == 7)
equ07d <- equ07[,26:44]
summary(equ07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
equ07m <- subset(equ07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(equ07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
equ07d <- subset(equ07d, select=-c(B006, F141))%>% na.omit()
summary(equ07d)
#


##equ06d
KMO(equ06d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(equ06d) #
nfactors(equ06d, rotate = "varimax")
equ6fact1 <- fa(equ06d,nfactors=1,rotate = "varimax")
equ6fact1$loadings
equ6fact2 <- fa(equ06d,nfactors=2,rotate = "varimax")
equ6fact2$loadings
equ6fact3 <- fa(equ06d,nfactors=3,rotate = "varimax")
equ6fact3$loadings
equ6fact4 <- fa(equ06d,nfactors=4,rotate = "varimax")
equ6fact4$loadings

#equ07d
KMO(equ07d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(equ07d) #
nfactors(equ07d, rotate = "varimax")
equ7fact1 <- fa(equ07d,nfactors=1,rotate = "varimax")
equ7fact1$loadings
equ7fact2 <- fa(equ07d,nfactors=2,rotate = "varimax")
equ7fact2$loadings
equ7fact3 <- fa(equ07d,nfactors=3,rotate = "varimax")
equ7fact3$loadings
equ7fact4 <- fa(equ07d,nfactors=4,rotate = "varimax")
equ7fact4$loadings

