rm(list = ls())
#Bolivia
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")

Bolivia <- read_csv("novapasta/Bolivia.csv") # 
# primeiro listwise deletions
table(Bolivia$S002VS)

summary(Bolivia[,26:44])
b07 <- Bolivia[,26:44]
b07 <- subset(b07, select=-c(B006, F141))
b07m <- b07
b07 <- Bolivia
b07d <- subset(b07m)%>% na.omit()

#quantos?
KMO(b07d)# todos acima de 0.5
cortest.bartlett(b07d) # nao roda
nfactors(b07d, rotate = "varimax")
fit<-princomp(b07d,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
b7fact1 <- fa(b07d,nfactors=1,rotate = "varimax")
b7fact1$loadings
b7fact2 <- fa(b07d,nfactors=2,rotate = "varimax")
b7fact2$loadings
b7fact3 <- fa(b07d,nfactors=3,rotate = "varimax")
b7fact3$loadings
b7fact4 <- fa(b07d,nfactors=4,rotate = "varimax")
b7fact4$loadings
