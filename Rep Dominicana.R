#Rep Dominicana
library(tidyverse)
WVS_TimeSeries_4_0 <- read_csv("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/WVS_TimeSeries_4_0.csv")
selecao <- WVS_TimeSeries_4_0$S003 == 214
Rdomgeral <- WVS_TimeSeries_4_0[selecao, ]
table(Rdomgeral$S002VS)
selecao <- Rdomgeral$S002VS == 3
Rdom3d <- Rdomgeral[selecao, ]
Rdom3d <- subset(Rdom3d, select=c(B008, C001,                     
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
                                              G006))
summary(Rdom3d)
Rdom3d <- Rdom3d %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(Rdom3d)
#Rdom3d<-subset(Rdom3d, select=-c( C002))

library(psych)
KMO(Rdom3d)# todos acima de 0.5 (menos e3036 e e039)



cortest.bartlett(Rdom3d) #
nfactors(Rdom3d, rotate = "varimax")
Rdom3dfact1 <- fa(Rdom3d,nfactors=1,rotate = "varimax")
Rdom3dfact1$loadings
Rdom3dfact2 <- fa(Rdom3d,nfactors=2,rotate = "varimax")
Rdom3dfact2$loadings
Rdom3dfact3 <- fa(Rdom3d,nfactors=3,rotate = "varimax")
Rdom3dfact3$loadings
Rdom3dfact4 <- fa(Rdom3d,nfactors=4,rotate = "varimax")
Rdom3dfact4$loadings

