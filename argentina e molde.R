#argentina
rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

argentina <- read_csv("novapasta/argentina.csv") # 

#onda 2

arg02 <- argentina %>%
  filter(S002VS == 2)
arg02d <- arg02[,26:44]

arg02m <- subset(arg02d, select=-c(B008,F144_02))
arg02d <- subset(arg02d, select=-c(B008,F144_02))%>% na.omit()
summary(arg02d)

#onda 3 recod
# já tem

arg03 <- read_sav("onda 3 _ 94-98/moreno_clivagem_argentina1995.sav")
arg03 <- remove_labels(arg03)
arg03m <- arg03[,26:41]
arg03d <- arg03[,26:41]%>% na.omit()
summary(arg03d)

#onda 4

arg04 <- argentina %>%
  filter(S002VS == 4)
arg04d <- arg04[,26:44]
summary(arg04d)

arg04m <- subset(arg04d, select=-c(B006, F141,F144_02))
arg04d <- subset(arg04d, select=-c(B006, F141,F144_02))%>% na.omit()
summary(arg04d)

#onda 5
#já tem

arg05<- read_sav("onda 5 --- 2005-09/moreno_clivagem_argentina2006.sav")
arg05 <- remove_labels(arg05)
arg05m <- arg05[,26:40]
arg05d <- arg05[,26:40]%>% na.omit()
summary(arg05d)

#onda 6
#já tem
arg06 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_argentina2013.sav")
arg06 <- remove_labels(arg06)
arg06m <- arg06[,26:40]
arg06d <- arg06[,26:40]%>% na.omit()
summary(arg06d)

#onda 7
# já tem
arg07 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_argentina2017.sav")
arg07 <- remove_labels(arg07)
arg07m <- arg07[,26:42]
arg07d <- arg07[,26:42]%>% na.omit()
summary(arg07d)


# onda 2 factorial
KMO(arg02d)# todos acima de 0.5
cortest.bartlett(arg02d) # nao roda
nfactors(arg02d, rotate = "varimax")
fit<-princomp(arg02d,cor=TRUE)# AFE
summary(fit)
plot(fit,type="lines")
arg2fact1 <- fa(arg02d,nfactors=1,rotate = "varimax")
arg2fact1$loadings
arg2fact2 <- fa(arg02d,nfactors=2,rotate = "varimax")
arg2fact2$loadings
arg2fact3 <- fa(arg02d,nfactors=3,rotate = "varimax")
arg2fact3$loadings
arg2fact4 <- fa(arg02d,nfactors=4,rotate = "varimax")
arg2fact4$loadings


#onda 3
KMO(arg03d)# todos acima de 0.5
cortest.bartlett(arg03d) # nao roda
nfactors(arg03d, rotate = "varimax")
fit<-princomp(arg03d,cor=TRUE)# AFE
summary(fit)
arg3fact1 <- fa(arg03d,nfactors=1,rotate = "varimax")
arg3fact1$loadings
arg3fact2 <- fa(arg03d,nfactors=2,rotate = "varimax")
arg3fact2$loadings
arg3fact3 <- fa(arg03d,nfactors=3,rotate = "varimax")
arg3fact3$loadings
arg3fact4 <- fa(arg03d,nfactors=4,rotate = "varimax")
arg3fact4$loadings


#
KMO(arg04d)# todos acima de 0.5 (menos B008)
cortest.bartlett(arg04d) # nao roda
nfactors(arg04d, rotate = "varimax")
arg4fact1 <- fa(arg04d,nfactors=1,rotate = "varimax")
arg4fact1$loadings
arg4fact2 <- fa(arg04d,nfactors=2,rotate = "varimax")
arg4fact2$loadings
arg4fact3 <- fa(arg04d,nfactors=3,rotate = "varimax")
arg4fact3$loadings
arg4fact4 <- fa(arg04d,nfactors=4,rotate = "varimax")
arg4fact4$loadings


#arg05d
KMO(arg05d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(arg05d) #
nfactors(arg05d, rotate = "varimax")
arg5fact1 <- fa(arg05d,nfactors=1,rotate = "varimax")
arg5fact1$loadings
arg5fact2 <- fa(arg05d,nfactors=2,rotate = "varimax")
arg5fact2$loadings
arg5fact3 <- fa(arg05d,nfactors=3,rotate = "varimax")
arg5fact3$loadings
arg5fact4 <- fa(arg05d,nfactors=4,rotate = "varimax")
arg5fact4$loadings


##arg06d
KMO(arg06d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(arg06d) #
nfactors(arg06d, rotate = "varimax")
arg6fact1 <- fa(arg06d,nfactors=1,rotate = "varimax")
arg6fact1$loadings
arg6fact2 <- fa(arg06d,nfactors=2,rotate = "varimax")
arg6fact2$loadings
arg6fact3 <- fa(arg06d,nfactors=3,rotate = "varimax")
arg6fact3$loadings
arg6fact4 <- fa(arg06d,nfactors=4,rotate = "varimax")
arg6fact4$loadings

#arg07d
KMO(arg07d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(arg07d) #
nfactors(arg07d, rotate = "varimax")
arg7fact1 <- fa(arg07d,nfactors=1,rotate = "varimax")
arg7fact1$loadings
arg7fact2 <- fa(arg07d,nfactors=2,rotate = "varimax")
arg7fact2$loadings
arg7fact3 <- fa(arg07d,nfactors=3,rotate = "varimax")
arg7fact3$loadings
arg7fact4 <- fa(arg07d,nfactors=4,rotate = "varimax")
arg7fact4$loadings

