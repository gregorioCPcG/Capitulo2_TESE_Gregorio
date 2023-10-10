rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

mexico <- read_csv("novapasta/mexico.csv")
mex02 <- mexico %>%
  filter(S002VS == 2)
mex02d <- mex02[,26:44]
summary(mex02d)
mex02m <- subset(mex02d, select=-c(B008,F144_02))
summary(mex02m)
mex02d <- subset(mex02d, select=-c(B008,F144_02))%>% na.omit()
mex03 <- mexico %>%
  filter(S002VS == 3)
mex03d <- mex03[,26:44]
summary(mex03d)#pra conferir
mex03m <-subset(mex03d, select=-c(B006,F141,F144_02))
summary(mex03m)#pra conferir
mex03d <-subset(mex03d, select=-c(B006,F141,F144_02))%>% na.omit()
mex04 <- mexico %>%
  filter(S002VS == 4)
mex04d <- mex04[,26:44]
summary(mex04d)#pra conferir
mex04m <-subset(mex04d, select=-c(B006,F141,F144_02))
summary(mex04m)#pra conferir
mex04d <-subset(mex04d, select=-c(B006,F141,F144_02))%>% na.omit()
mex05 <- mexico %>%
  filter(S002VS == 5)
mex05d <- mex05[,26:44]
summary(mex05d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
mex05m <- subset(mex05d, select=-c(B006,F141
                                   ,F144_02, E034))#PERGUNTAS NAO FEITAS
summary(mex05m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
mex05d <- subset(mex05d, select=-c(B006,F141,
                                   F144_02,E034))%>% na.omit()
summary(mex05d)
mex06 <- mexico %>%
  filter(S002VS == 6)
mex06d <- mex06[,26:44]
summary(mex06d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
mex06m <- subset(mex06d, select=-c(B006, F141, F144_02, E034))#PERGUNTAS NAO FEITAS
summary(mex06m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
mex06d <- subset(mex06d, select=-c(B006, F141, F144_02, E034))%>% na.omit()
summary(mex06d)
mex07 <- mexico %>%
  filter(S002VS == 7)
mex07d <- mex07[,26:44]
summary(mex07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
mex07m <- subset(mex07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(mex07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
mex07d <- subset(mex07d, select=-c(B006, F141))%>% na.omit()
summary(mex07d)
#

# onda 2 factorial
KMO(mex02d)# todos acima de 0.5
cortest.bartlett(mex02d) # nao roda
nfactors(mex02d, rotate = "varimax")
fit<-princomp(mex02d,cor=TRUE)# AFE
summary(fit)
plot(fit,type="lines")
mex2fact1 <- fa(mex02d,nfactors=1,rotate = "varimax")
mex2fact1$loadings
mex2fact2 <- fa(mex02d,nfactors=2,rotate = "varimax")
mex2fact2$loadings
mex2fact3 <- fa(mex02d,nfactors=3,rotate = "varimax")
mex2fact3$loadings
mex2fact4 <- fa(mex02d,nfactors=4,rotate = "varimax")
mex2fact4$loadings


#onda 3
KMO(mex03d)# todos acima de 0.5
cortest.bartlett(mex03d) # nao roda
nfactors(mex03d, rotate = "varimax")
fit<-princomp(mex03d,cor=TRUE)# AFE
summary(fit)
mex3fact1 <- fa(mex03d,nfactors=1,rotate = "varimax")
mex3fact1$loadings
mex3fact2 <- fa(mex03d,nfactors=2,rotate = "varimax")
mex3fact2$loadings
mex3fact3 <- fa(mex03d,nfactors=3,rotate = "varimax")
mex3fact3$loadings
mex3fact4 <- fa(mex03d,nfactors=4,rotate = "varimax")
mex3fact4$loadings


#
KMO(mex04d)# todos acima de 0.5 (menos B008)
cortest.bartlett(mex04d) # nao roda
nfactors(mex04d, rotate = "varimax")
mex4fact1 <- fa(mex04d,nfactors=1,rotate = "varimax")
mex4fact1$loadings
mex4fact2 <- fa(mex04d,nfactors=2,rotate = "varimax")
mex4fact2$loadings
mex4fact3 <- fa(mex04d,nfactors=3,rotate = "varimax")
mex4fact3$loadings
mex4fact4 <- fa(mex04d,nfactors=4,rotate = "varimax")
mex4fact4$loadings


#mex05d
KMO(mex05d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(mex05d) #
nfactors(mex05d, rotate = "varimax")
mex5fact1 <- fa(mex05d,nfactors=1,rotate = "varimax")
mex5fact1$loadings
mex5fact2 <- fa(mex05d,nfactors=2,rotate = "varimax")
mex5fact2$loadings
mex5fact3 <- fa(mex05d,nfactors=3,rotate = "varimax")
mex5fact3$loadings
mex5fact4 <- fa(mex05d,nfactors=4,rotate = "varimax")
mex5fact4$loadings


##mex06d
KMO(mex06d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(mex06d) #
nfactors(mex06d, rotate = "varimax")
mex6fact1 <- fa(mex06d,nfactors=1,rotate = "varimax")
mex6fact1$loadings
mex6fact2 <- fa(mex06d,nfactors=2,rotate = "varimax")
mex6fact2$loadings
mex6fact3 <- fa(mex06d,nfactors=3,rotate = "varimax")
mex6fact3$loadings
mex6fact4 <- fa(mex06d,nfactors=4,rotate = "varimax")
mex6fact4$loadings

#mex07d
KMO(mex07d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(mex07d) #
nfactors(mex07d, rotate = "varimax")
mex7fact1 <- fa(mex07d,nfactors=1,rotate = "varimax")
mex7fact1$loadings
mex7fact2 <- fa(mex07d,nfactors=2,rotate = "varimax")
mex7fact2$loadings
mex7fact3 <- fa(mex07d,nfactors=3,rotate = "varimax")
mex7fact3$loadings
mex7fact4 <- fa(mex07d,nfactors=4,rotate = "varimax")
mex7fact4$loadings
