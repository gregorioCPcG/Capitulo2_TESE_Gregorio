library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

rm(list = ls())
haiti6 <- read_csv("novapasta/haiti.csv")


#onda 6

haiti6d <- haiti6[,26:44]
summary(haiti6d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
haiti6m <- subset(haiti6d, select=-c(B006,F141,F144_02, E034))#PERGUNTAS NAO FEITAS
summary(haiti6m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
haiti6d <- subset(haiti6d, select=-c(B006,F141, F144_02,E034))%>% na.omit()
summary(haiti6d)

#
##haiti6d
KMO(haiti6d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(haiti6d) #
nfactors(haiti6d, rotate = "varimax")
haiti6fact1 <- fa(haiti6d,nfactors=1,rotate = "varimax")
haiti6fact1$loadings
haiti6fact2 <- fa(haiti6d,nfactors=2,rotate = "varimax")
haiti6fact2$loadings
haiti6fact3 <- fa(haiti6d,nfactors=3,rotate = "varimax")
haiti6fact3$loadings
haiti6fact4 <- fa(haiti6d,nfactors=4,rotate = "varimax")
haiti6fact4$loadings
