rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

Ven03d <- read_csv("onda 3 _ 94-98/Ven03d.csv")
Ven04d <- read_csv("onda 4 99-2004/Ven04d.csv")
summary(Ven03d)#os missings ja foram deletados pelo bolsista
summary(Ven04d)#os missings ja foram deletados pelo bolsista
#onda 7 do zero _ Bolsista não fez.
WVS_TimeSeries_4_0 <- read_csv("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/WVS_TimeSeries_4_0.csv")
table(WVS_TimeSeries_4_0$S002VS)
selecao <- WVS_TimeSeries_4_0$S002VS == 7
ven <- WVS_TimeSeries_4_0[selecao, ]
selecao <- ven$S003 == 862
ven <- ven[selecao, ]
ven -> Ven07d
Ven07d <- subset(Ven07d, select=c(B008, C001,                     
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
summary(Ven07d)
# Substitua todos os valores menores que 0 por NA em Ven07d
Ven07d <- Ven07d %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(Ven07d)
summary(Ven03d)#os missings ja foram deletados pelo bolsista
summary(Ven04d)#os missings ja foram deletados pelo bolsista


#factoriais
#onda 3
KMO(Ven03d)# todos acima de 0.5
cortest.bartlett(Ven03d) # nao roda
nfactors(Ven03d, rotate = "varimax")
fit<-princomp(Ven03d,cor=TRUE)# AFE
summary(fit)
Ven3fact1 <- fa(Ven03d,nfactors=1,rotate = "varimax")
Ven3fact1$loadings
Ven3fact2 <- fa(Ven03d,nfactors=2,rotate = "varimax")
Ven3fact2$loadings
Ven3fact3 <- fa(Ven03d,nfactors=3,rotate = "varimax")
Ven3fact3$loadings
Ven3fact4 <- fa(Ven03d,nfactors=4,rotate = "varimax")
Ven3fact4$loadings


#onda 4
KMO(Ven04d)# todos acima de 0.5 (menos B008)
cortest.bartlett(Ven04d) # nao roda
nfactors(Ven04d, rotate = "varimax")
Ven4fact1 <- fa(Ven04d,nfactors=1,rotate = "varimax")
Ven4fact1$loadings
Ven4fact2 <- fa(Ven04d,nfactors=2,rotate = "varimax")
Ven4fact2$loadings
Ven4fact3 <- fa(Ven04d,nfactors=3,rotate = "varimax")
Ven4fact3$loadings
Ven4fact4 <- fa(Ven04d,nfactors=4,rotate = "varimax")
Ven4fact4$loadings



#Ven07d
KMO(Ven07d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(Ven07d) #
nfactors(Ven07d, rotate = "varimax")
Ven7fact1 <- fa(Ven07d,nfactors=1,rotate = "varimax")
Ven7fact1$loadings
Ven7fact2 <- fa(Ven07d,nfactors=2,rotate = "varimax")
Ven7fact2$loadings
Ven7fact3 <- fa(Ven07d,nfactors=3,rotate = "varimax")
Ven7fact3$loadings
Ven7fact4 <- fa(Ven07d,nfactors=4,rotate = "varimax")
Ven7fact4$loadings



