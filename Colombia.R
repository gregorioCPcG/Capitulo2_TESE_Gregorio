rm(list = ls())
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
Colombia <- read_csv("novapasta/Colombia.csv")
col05 <- Colombia %>%
  filter(S002VS == 5)
col05d <- col05[,26:44]
col06 <- Colombia %>%
  filter(S002VS == 6)
col06d <- col06[,26:44]
col07 <- Colombia %>%
  filter(S002VS == 7)
col07d <- col07[,26:44]
col03 <- Colombia %>%
  filter(S002VS == 3)
col03d <- col03[,26:44]

col07d <- subset(col07d, select=-c(B006, F141))%>% na.omit()

#col05d
#x*não foi possível rodar em virtuda da ausência de issues naquele país/onda específico
##col06d
#x*não foi possível rodar em virtuda da ausência de issues naquele país/onda específico

#col07d
KMO(col07d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(col07d) #
nfactors(col07d, rotate = "varimax")
col7fact1 <- fa(col07d,nfactors=1,rotate = "varimax")
col7fact1$loadings
col7fact2 <- fa(col07d,nfactors=2,rotate = "varimax")
col7fact2$loadings
col7fact3 <- fa(col07d,nfactors=3,rotate = "varimax")
col7fact3$loadings
col7fact4 <- fa(col07d,nfactors=4,rotate = "varimax")
col7fact4$loadings

