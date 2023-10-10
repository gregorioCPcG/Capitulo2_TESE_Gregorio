#Chile

rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
merge <- read_sav("moreno_clivagem_merge.sav")
chi <- merge %>%
  filter(COUNTRY_ALPHA == "CHL")
CHILEobj <- summary(as.factor(chi$S002VS))
CHILEobj# todos o Lucas fez, menos o 4(terei q fazê-lo)
rm(chi, merge)

# #onda 2
ch2 <- read_sav("onda 2_89-93/moreno_clivagem_chile1990.sav")
ch2 <- remove_labels(ch2)
ch2m <- ch2[,26:42]
summary(ch2m)#verificar
ch2d <- ch2[,26:42]%>% na.omit()
summary(ch2d)#verificar

#onda3
ch3 <- read_sav("onda 3 _ 94-98/moreno_clivagem_chile1996.sav")
ch3 <- remove_labels(ch3)
ch3m <- ch3[,26:41]
summary(ch3m)#verificar
ch3d <- ch3[,26:41]%>% na.omit()
summary(ch3d)#verificar

#onda4 - Lucas nao fez, tive que fazer do início
merge <- read_sav("moreno_clivagem_merge.sav")
ch4 <- merge %>%
  filter(COUNTRY_ALPHA == "CHL")
ch4 <- ch4 %>%
  filter(S002VS == 4)
library(labelled)
ch4 <- remove_labels(ch4)
rm(merge)
ch4m <- ch4[,26:44]
summary(ch4m)#pra testar - ver quais são NAN
ch4m <- subset(ch4m, select=-c(B006, F141, F144_02))
summary(ch4m)#pra testar de novo
ch4d <-ch4m[,1:16]%>% na.omit()
summary(ch4d)#pra ver se deu certo

# onda 5
ch5 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_chile2006.sav")
ch5 <- remove_labels(ch5)
ch5m <- ch5[,26:40]
summary(ch5m)#verificar
ch5d <- ch5[,26:40]%>% na.omit()
summary(ch5d)#verificar


# onda 6
ch6 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_chile2012.sav")
ch6 <- remove_labels(ch6)
ch6m <- ch6[,26:40]
summary(ch6m)#verificar
ch6d <- ch6[,26:40]%>% na.omit()
summary(ch6d)#verificar

# onda 7
ch7 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_chile2018.sav")
ch7 <- remove_labels(ch7)
ch7m <- ch7[,26:42]
summary(ch7m)#verificar
ch7d <- ch7[,26:42]%>% na.omit()
summary(ch7d)#verificar


#
# onda 2 factorial
KMO(ch2d)# todos acima de 0.5
cortest.bartlett(ch2d) # nao roda
nfactors(ch2d, rotate = "varimax")
fit<-princomp(ch2d,cor=TRUE)# AFE
summary(fit)
plot(fit,type="lines")
ch2fact1 <- fa(ch2d,nfactors=1,rotate = "varimax")
ch2fact1$loadings
ch2fact2 <- fa(ch2d,nfactors=2,rotate = "varimax")
ch2fact2$loadings
ch2fact3 <- fa(ch2d,nfactors=3,rotate = "varimax")
ch2fact3$loadings
ch2fact4 <- fa(ch2d,nfactors=4,rotate = "varimax")
ch2fact4$loadings


#onda 3
KMO(ch3d)# todos acima de 0.5
cortest.bartlett(ch3d) # nao roda
nfactors(ch3d, rotate = "varimax")
fit<-princomp(ch3d,cor=TRUE)# AFE
summary(fit)
ch3fact1 <- fa(ch3d,nfactors=1,rotate = "varimax")
ch3fact1$loadings
ch3fact2 <- fa(ch3d,nfactors=2,rotate = "varimax")
ch3fact2$loadings
ch3fact3 <- fa(ch3d,nfactors=3,rotate = "varimax")
ch3fact3$loadings
ch3fact4 <- fa(ch3d,nfactors=4,rotate = "varimax")
ch3fact4$loadings


#
KMO(ch4d)# todos acima de 0.5 (menos B008)
cortest.bartlett(ch4d) # nao roda
nfactors(ch4d, rotate = "varimax")
ch4fact1 <- fa(ch4d,nfactors=1,rotate = "varimax")
ch4fact1$loadings
ch4fact2 <- fa(ch4d,nfactors=2,rotate = "varimax")
ch4fact2$loadings
ch4fact3 <- fa(ch4d,nfactors=3,rotate = "varimax")
ch4fact3$loadings
ch4fact4 <- fa(ch4d,nfactors=4,rotate = "varimax")
ch4fact4$loadings


#ch5d
KMO(ch5d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(ch5d) #
nfactors(ch5d, rotate = "varimax")
ch5fact1 <- fa(ch5d,nfactors=1,rotate = "varimax")
ch5fact1$loadings
ch5fact2 <- fa(ch5d,nfactors=2,rotate = "varimax")
ch5fact2$loadings
ch5fact3 <- fa(ch5d,nfactors=3,rotate = "varimax")
ch5fact3$loadings
ch5fact4 <- fa(ch5d,nfactors=4,rotate = "varimax")
ch5fact4$loadings


##ch6d
KMO(ch6d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(ch6d) #
nfactors(ch6d, rotate = "varimax")
ch6fact1 <- fa(ch6d,nfactors=1,rotate = "varimax")
ch6fact1$loadings
ch6fact2 <- fa(ch6d,nfactors=2,rotate = "varimax")
ch6fact2$loadings
ch6fact3 <- fa(ch6d,nfactors=3,rotate = "varimax")
ch6fact3$loadings
ch6fact4 <- fa(ch6d,nfactors=4,rotate = "varimax")
ch6fact4$loadings

#ch7d
KMO(ch7d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(ch7d) #
nfactors(ch7d, rotate = "varimax")
ch7fact1 <- fa(ch7d,nfactors=1,rotate = "varimax")
ch7fact1$loadings
ch7fact2 <- fa(ch7d,nfactors=2,rotate = "varimax")
ch7fact2$loadings
ch7fact3 <- fa(ch7d,nfactors=3,rotate = "varimax")
ch7fact3$loadings
ch7fact4 <- fa(ch7d,nfactors=4,rotate = "varimax")
ch7fact4$loadings

#
