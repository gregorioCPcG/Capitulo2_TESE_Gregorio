#Brasil

rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

#baixar os bancos
#onda2
br2 <- read_sav("onda 2_89-93/moreno_clivagem_brasil1991.sav")
br2 <- remove_labels(br2)
br2m <- br2[,26:42]
summary(br2m)#verificar
br2d <- br2[,26:42]%>% na.omit()
summary(br2d)#verificar
#onda3
br3 <- read_sav("onda 3 _ 94-98/moreno_clivagem_brasil1997.sav")
br3 <- remove_labels(br3)
br3m <- br3[,26:41]
summary(br3m)#verificar
br3d <- br3[,26:41]%>% na.omit()
summary(br3d)#verificar

#onda5
br5 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_brasil2006.sav")
br5 <- remove_labels(br5)
br5m <- br5[,26:40]
summary(br5m)#verificar
br5d <- br5[,26:40]%>% na.omit()
summary(br5d)#verificar

#onda6
br6 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_brasil2014.sav")
br6 <- remove_labels(br6)
br6m <- br6[,26:40]
summary(br6m)#verificar
br6d <- br6[,26:40]%>% na.omit()
summary(br6d)#verificar

#onda7
br7 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_brasil2018.sav")
br7 <- remove_labels(br7)
br7m <- br7[,26:42]
summary(br7m)#verificar
br7d <- br7[,26:42]%>% na.omit()
summary(br7d)#verificar


# onda 2 factorial
KMO(br2d)# todos acima de 0.5
cortest.bartlett(br2d) # nao roda
nfactors(br2d, rotate = "varimax")
fit<-princomp(br2d,cor=TRUE)# AFE
summary(fit)
plot(fit,type="lines")
br2fact1 <- fa(br2d,nfactors=1,rotate = "varimax")
br2fact1$loadings
br2fact2 <- fa(br2d,nfactors=2,rotate = "varimax")
br2fact2$loadings
br2fact3 <- fa(br2d,nfactors=3,rotate = "varimax")
br2fact3$loadings
br2fact4 <- fa(br2d,nfactors=4,rotate = "varimax")
br2fact4$loadings


#onda 3
KMO(br3d)# todos acima de 0.5
cortest.bartlett(br3d) # nao roda
nfactors(br3d, rotate = "varimax")
fit<-princomp(br3d,cor=TRUE)# AFE
summary(fit)
br3fact1 <- fa(br3d,nfactors=1,rotate = "varimax")
br3fact1$loadings
br3fact2 <- fa(br3d,nfactors=2,rotate = "varimax")
br3fact2$loadings
br3fact3 <- fa(br3d,nfactors=3,rotate = "varimax")
br3fact3$loadings
br3fact4 <- fa(br3d,nfactors=4,rotate = "varimax")
br3fact4$loadings




#br5d
KMO(br5d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(br5d) #
nfactors(br5d, rotate = "varimax")
br5fact1 <- fa(br5d,nfactors=1,rotate = "varimax")
br5fact1$loadings
br5fact2 <- fa(br5d,nfactors=2,rotate = "varimax")
br5fact2$loadings
br5fact3 <- fa(br5d,nfactors=3,rotate = "varimax")
br5fact3$loadings
br5fact4 <- fa(br5d,nfactors=4,rotate = "varimax")
br5fact4$loadings


##br6d
KMO(br6d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(br6d) #
nfactors(br6d, rotate = "varimax")
br6fact1 <- fa(br6d,nfactors=1,rotate = "varimax")
br6fact1$loadings
br6fact2 <- fa(br6d,nfactors=2,rotate = "varimax")
br6fact2$loadings
br6fact3 <- fa(br6d,nfactors=3,rotate = "varimax")
br6fact3$loadings
br6fact4 <- fa(br6d,nfactors=4,rotate = "varimax")
br6fact4$loadings

#br7d
KMO(br7d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(br7d) #
nfactors(br7d, rotate = "varimax")
br7fact1 <- fa(br7d,nfactors=1,rotate = "varimax")
br7fact1$loadings
br7fact2 <- fa(br7d,nfactors=2,rotate = "varimax")
br7fact2$loadings
br7fact3 <- fa(br7d,nfactors=3,rotate = "varimax")
br7fact3$loadings
br7fact4 <- fa(br7d,nfactors=4,rotate = "varimax")
br7fact4$loadings

