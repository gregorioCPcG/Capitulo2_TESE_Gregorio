library(tidyverse)
WVS_TimeSeries_4_0 <- read_csv("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/WVS_TimeSeries_4_0.csv")
selecao <- WVS_TimeSeries_4_0$S003 == 222
elSalvadorgeral <- WVS_TimeSeries_4_0[selecao, ]
table(elSalvadorgeral$S002VS)
selecao <- elSalvadorgeral$S002VS == 3
elSalvador3d <- elSalvadorgeral[selecao, ]
elSalvador3d <- subset(elSalvador3d, select=c(B008, C001,                     
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
library(psych)
KMO(elSalvador3d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(elSalvador3d) #
nfactors(elSalvador3d, rotate = "varimax")
elSalvador3dfact1 <- fa(elSalvador3d,nfactors=1,rotate = "varimax")
elSalvador3dfact1$loadings
elSalvador3dfact2 <- fa(elSalvador3d,nfactors=2,rotate = "varimax")
elSalvador3dfact2$loadings
elSalvador3dfact3 <- fa(elSalvador3d,nfactors=3,rotate = "varimax")
elSalvador3dfact3$loadings
elSalvador3dfact4 <- fa(elSalvador3d,nfactors=4,rotate = "varimax")
elSalvador3dfact4$loadings
