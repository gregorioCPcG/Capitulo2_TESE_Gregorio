#Uruguai

rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

#onda3
uru03 <- read_sav("onda 3 _ 94-98/moreno_clivagem_uruguai1996.sav")
uru03 <- remove_labels(uru03)
uru03m <- uru03[,26:41]
summary(uru03m)#verificar
uru03d <- uru03[,26:41]%>% na.omit()
summary(uru03d)#verificar
#onda 5
uru05 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_uruguai2006.sav")
uru05 <- remove_labels(uru05)
uru05m <- uru05[,26:40]
summary(uru05m)#verificar
uru05d <- uru05[,26:40]%>% na.omit()
summary(uru05d)#verificar

# onda 6
uru06 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_uruguai2011.sav")
uru06 <- remove_labels(uru06)
uru06m <- uru06[,26:40]
summary(uru06m)#verificar
uru06d <- uru06[,26:40]%>% na.omit()
summary(uru06d)#verificar


#onda 3
KMO(uru03d)# todos acima de 0.5
cortest.bartlett(uru03d) # nao roda
nfactors(uru03d, rotate = "varimax")
fit<-princomp(uru03d,cor=TRUE)# AFE
summary(fit)
uru3fact1 <- fa(uru03d,nfactors=1,rotate = "varimax")
uru3fact1$loadings
uru3fact2 <- fa(uru03d,nfactors=2,rotate = "varimax")
uru3fact2$loadings
uru3fact3 <- fa(uru03d,nfactors=3,rotate = "varimax")
uru3fact3$loadings
uru3fact4 <- fa(uru03d,nfactors=4,rotate = "varimax")
uru3fact4$loadings


#uru05d
KMO(uru05d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(uru05d) #
nfactors(uru05d, rotate = "varimax")
uru5fact1 <- fa(uru05d,nfactors=1,rotate = "varimax")
uru5fact1$loadings
uru5fact2 <- fa(uru05d,nfactors=2,rotate = "varimax")
uru5fact2$loadings
uru5fact3 <- fa(uru05d,nfactors=3,rotate = "varimax")
uru5fact3$loadings
uru5fact4 <- fa(uru05d,nfactors=4,rotate = "varimax")
uru5fact4$loadings


##uru06d
KMO(uru06d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(uru06d) #
nfactors(uru06d, rotate = "varimax")
uru6fact1 <- fa(uru06d,nfactors=1,rotate = "varimax")
uru6fact1$loadings
uru6fact2 <- fa(uru06d,nfactors=2,rotate = "varimax")
uru6fact2$loadings
uru6fact3 <- fa(uru06d,nfactors=3,rotate = "varimax")
uru6fact3$loadings
uru6fact4 <- fa(uru06d,nfactors=4,rotate = "varimax")
uru6fact4$loadings




#onda 7 do zero _ Bolsista não fez.
WVS_TimeSeries_4_0 <- read_csv("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/WVS_TimeSeries_4_0.csv")
table(WVS_TimeSeries_4_0$S002VS)
selecao <- WVS_TimeSeries_4_0$S002VS == 7
uru <- WVS_TimeSeries_4_0[selecao, ]
selecao <- uru$S003 == 858
uru <- uru[selecao, ]
uru -> uru7
uru7 <- subset(uru7, select=c(B008, C001,                     
                              C002,                     
                              E018,           
                              E034,                
                              E035,                 
                              E036,              
                              E039,                       
                              F028,               
                              F034,  
                              F063,    
                              F116,     
                              F118,          
                              F120,     
                              F121,                         
                              F144_02,        
                              G006))
summary(uru7)
# Substitua todos os valores menores que 0 por NA em uru7
uru7 <- uru7 %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(uru7)
#uru7
KMO(uru7)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(uru7) #
nfactors(uru7, rotate = "varimax")
uru7fact1 <- fa(uru7,nfactors=1,rotate = "varimax")
uru7fact1$loadings
uru7fact2 <- fa(uru7,nfactors=2,rotate = "varimax")
uru7fact2$loadings
uru7fact3 <- fa(uru7,nfactors=3,rotate = "varimax")
uru7fact3$loadings
uru7fact4 <- fa(uru7,nfactors=4,rotate = "varimax")
uru7fact4$loadings
