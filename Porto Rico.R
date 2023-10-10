#Porto Rico
library(tidyverse)
WVS_TimeSeries_4_0 <- read_csv("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/WVS_TimeSeries_4_0.csv")
selecao <- WVS_TimeSeries_4_0$S003 == 630
portoricogeral <- WVS_TimeSeries_4_0[selecao, ]
table(portoricogeral$S002VS)
selecao <- portoricogeral$S002VS == 3
portorico3d <- portoricogeral[selecao, ]
selecao <- portoricogeral$S002VS == 4
portorico4d <- portoricogeral[selecao, ]
selecao <- portoricogeral$S002VS == 7
portorico7d <- portoricogeral[selecao, ]
portorico3d <- subset(portorico3d, select=c(B008, C001,                     
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

portorico4d <- subset(portorico4d, select=c(B008, C001,                     
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

portorico7d <- subset(portorico7d, select=c(B008, C001,                     
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
portorico7d <- portorico7d %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(portorico7d)
portorico3d <- portorico3d %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(portorico3d)
portorico4d <- portorico4d %>%
  mutate_all(~ ifelse(. < 0, NA, .))
summary(portorico4d)
portorico4d <- subset(portorico4d, select=-c(F144_02))
portorico3d <- subset(portorico3d, select=-c(F144_02))

library(psych)
KMO(portorico3d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(portorico3d) #
nfactors(portorico3d, rotate = "varimax")
portorico3dfact1 <- fa(portorico3d,nfactors=1,rotate = "varimax")
portorico3dfact1$loadings
portorico3dfact2 <- fa(portorico3d,nfactors=2,rotate = "varimax")
portorico3dfact2$loadings
portorico3dfact3 <- fa(portorico3d,nfactors=3,rotate = "varimax")
portorico3dfact3$loadings
portorico3dfact4 <- fa(portorico3d,nfactors=4,rotate = "varimax")
portorico3dfact4$loadings

KMO(portorico4d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(portorico4d) #
nfactors(portorico4d, rotate = "varimax")
portorico4dfact1 <- fa(portorico4d,nfactors=1,rotate = "varimax")
portorico4dfact1$loadings
portorico4dfact2 <- fa(portorico4d,nfactors=2,rotate = "varimax")
portorico4dfact2$loadings
portorico4dfact3 <- fa(portorico4d,nfactors=3,rotate = "varimax")
portorico4dfact3$loadings
portorico4dfact4 <- fa(portorico4d,nfactors=4,rotate = "varimax")
portorico4dfact4$loadings

KMO(portorico7d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(portorico7d) #
nfactors(portorico7d, rotate = "varimax")
portorico7dfact1 <- fa(portorico7d,nfactors=1,rotate = "varimax")
portorico7dfact1$loadings
portorico7dfact2 <- fa(portorico7d,nfactors=2,rotate = "varimax")
portorico7dfact2$loadings
portorico7dfact3 <- fa(portorico7d,nfactors=3,rotate = "varimax")
portorico7dfact3$loadings
portorico7dfact4 <- fa(portorico7d,nfactors=4,rotate = "varimax")
portorico7dfact4$loadings
