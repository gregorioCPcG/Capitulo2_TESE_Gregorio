rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

# PERU

Peru <- read_csv("novapasta/Peru.csv")


# peru 3

peru03 <- Peru %>%
  filter(S002VS == 3)
peru03d <- peru03[,26:44]
summary(peru03d)#pra conferir
peru03m <-subset(peru03d, select=-c(B006,F141,F144_02))
summary(peru03m)#pra conferir
peru03d <-subset(peru03d, select=-c(B006,F141,F144_02))%>% na.omit()

# peru 4
#

peru04 <- Peru %>%
  filter(S002VS == 4)
peru04d <- peru04[,26:44]
summary(peru04d)#pra conferir
peru04m <-subset(peru04d, select=-c(B006,F141,F144_02))
summary(peru04m)#pra conferir
peru04d <-subset(peru04d, select=-c(B006,F141,F144_02))%>% na.omit()


# Peru 5 - poucas issues foram feitas, abdiquei

#

#peru6

#

peru06 <- Peru %>%
  filter(S002VS == 6)
peru06d <- peru06[,26:44]
summary(peru06d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
peru06m <- subset(peru06d, select=-c(B006, F141, F144_02, E034))#PERGUNTAS NAO FEITAS
summary(peru06m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
peru06d <- subset(peru06d, select=-c(B006, F141, F144_02, E034))%>% na.omit()
summary(peru06d)



#peru7

#

peru07 <- Peru %>%
  filter(S002VS == 7)
peru07d <- peru07[,26:44]
summary(peru07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
peru07m <- subset(peru07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(peru07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
peru07d <- subset(peru07d, select=-c(B006, F141))%>% na.omit()
summary(peru07d)



#onda 3
KMO(peru03d)# todos acima de 0.5
cortest.bartlett(peru03d) # nao roda
nfactors(peru03d, rotate = "varimax")
fit<-princomp(peru03d,cor=TRUE)# AFE
summary(fit)
peru3fact1 <- fa(peru03d,nfactors=1,rotate = "varimax")
peru3fact1$loadings
peru3fact2 <- fa(peru03d,nfactors=2,rotate = "varimax")
peru3fact2$loadings
peru3fact3 <- fa(peru03d,nfactors=3,rotate = "varimax")
peru3fact3$loadings
peru3fact4 <- fa(peru03d,nfactors=4,rotate = "varimax")
peru3fact4$loadings


#peru04d
KMO(peru04d)# todos acima de 0.5 (menos B008)
cortest.bartlett(peru04d) # nao roda
nfactors(peru04d, rotate = "varimax")
peru4fact1 <- fa(peru04d,nfactors=1,rotate = "varimax")
peru4fact1$loadings
peru4fact2 <- fa(peru04d,nfactors=2,rotate = "varimax")
peru4fact2$loadings
peru4fact3 <- fa(peru04d,nfactors=3,rotate = "varimax")
peru4fact3$loadings
peru4fact4 <- fa(peru04d,nfactors=4,rotate = "varimax")
peru4fact4$loadings




##peru06d
KMO(peru06d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(peru06d) #
nfactors(peru06d, rotate = "varimax")
peru6fact1 <- fa(peru06d,nfactors=1,rotate = "varimax")
peru6fact1$loadings
peru6fact2 <- fa(peru06d,nfactors=2,rotate = "varimax")
peru6fact2$loadings
peru6fact3 <- fa(peru06d,nfactors=3,rotate = "varimax")
peru6fact3$loadings
peru6fact4 <- fa(peru06d,nfactors=4,rotate = "varimax")
peru6fact4$loadings

#peru07d
KMO(peru07d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(peru07d) #
nfactors(peru07d, rotate = "varimax")
peru7fact1 <- fa(peru07d,nfactors=1,rotate = "varimax")
peru7fact1$loadings
peru7fact2 <- fa(peru07d,nfactors=2,rotate = "varimax")
peru7fact2$loadings
peru7fact3 <- fa(peru07d,nfactors=3,rotate = "varimax")
peru7fact3$loadings
peru7fact4 <- fa(peru07d,nfactors=4,rotate = "varimax")
peru7fact4$loadings

