#Apendice cap 2 tabela latino 2015
rm(list = ls())

setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/latinobarometro2020/Achados Latino Barometro 06,18, anos 1990/WVS desconfis/6 paises e além ou LatinoBarometro 2015/Os outros todos paises e America Latina toda")
load("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/latinobarometro2020/Achados Latino Barometro 06,18, anos 1990/WVS desconfis/6 paises e além ou LatinoBarometro 2015/Latinobarometro_2015_Esp.rdata")



library(tidyverse)

#Argentina

Argentina <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 32)
table(Argentina$P16TGB.B)#POLICIA
table(Argentina$P19ST.C)#partidos
table(Argentina$P16ST.F)#congresso
table(Argentina$P19N.H)#justiça eleitoral
table(Argentina$P46N.C)#antidireito
table(Argentina$P12TG.B)#insatisfeito com economia
Argentina$P16TGB.B[Argentina$P16TGB.B  == -1] <- NA # gerar missing
Argentina$P16TGB.B[Argentina$P16TGB.B  == -2] <- NA
Argentina$P19ST.C[Argentina$P19ST.C  == -1] <- NA # gerar missing
Argentina$P19ST.C[Argentina$P19ST.C  == -2] <- NA
Argentina$P16ST.F[Argentina$P16ST.F  == -1] <- NA # gerar missing
Argentina$P16ST.F[Argentina$P16ST.F  == -2] <- NA
Argentina$P19N.H[Argentina$P19N.H  == -1] <- NA # gerar missing
Argentina$P19N.H[Argentina$P19N.H  == -2] <- NA
Argentina$P12TG.B[Argentina$P12TG.B  == -1] <- NA # gerar missing
Argentina$P12TG.B[Argentina$P12TG.B  == -2] <- NA
Argentina$P46N.C[Argentina$P46N.C  == -1] <- NA # gerar missing
Argentina$P46N.C[Argentina$P46N.C  == -2] <- NA
table(Argentina$P69ST.C)#desaprova casamento gay
Argentina$P69ST.C[Argentina$P69ST.C  == -1] <- NA
Argentina$P69ST.C[Argentina$P69ST.C  == -2] <- NA


Brasil <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 76)
table(Brasil$P16TGB.B)#POLICIA
table(Brasil$P19ST.C)#partidos
table(Brasil$P16ST.F)#congresso
table(Brasil$P19N.H)#justiça eleitoral
table(Brasil$P46N.C)#antidireito
table(Brasil$P12TG.B)#insatisfeito com economia
Brasil$P16TGB.B[Brasil$P16TGB.B  == -1] <- NA # gerar missing
Brasil$P16TGB.B[Brasil$P16TGB.B  == -2] <- NA
Brasil$P19ST.C[Brasil$P19ST.C  == -1] <- NA # gerar missing
Brasil$P19ST.C[Brasil$P19ST.C  == -2] <- NA
Brasil$P16ST.F[Brasil$P16ST.F  == -1] <- NA # gerar missing
Brasil$P16ST.F[Brasil$P16ST.F  == -2] <- NA
Brasil$P19N.H[Brasil$P19N.H  == -1] <- NA # gerar missing
Brasil$P19N.H[Brasil$P19N.H  == -2] <- NA
Brasil$P12TG.B[Brasil$P12TG.B  == -1] <- NA # gerar missing
Brasil$P12TG.B[Brasil$P12TG.B  == -2] <- NA
Brasil$P46N.C[Brasil$P46N.C  == -1] <- NA # gerar missing
Brasil$P46N.C[Brasil$P46N.C  == -2] <- NA
table(Brasil$P69ST.C)#desaprova casamento gay
Brasil$P69ST.C[Brasil$P69ST.C  == -1] <- NA
Brasil$P69ST.C[Brasil$P69ST.C  == -2] <- NA


Bolivia <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 68)
table(Bolivia$P16TGB.B)#POLICIA
table(Bolivia$P19ST.C)#partidos
table(Bolivia$P16ST.F)#congresso
table(Bolivia$P19N.H)#justiça eleitoral
table(Bolivia$P46N.C)#antidireito
table(Bolivia$P12TG.B)#insatisfeito com economia
Bolivia$P16TGB.B[Bolivia$P16TGB.B  == -1] <- NA # gerar missing
Bolivia$P16TGB.B[Bolivia$P16TGB.B  == -2] <- NA
Bolivia$P19ST.C[Bolivia$P19ST.C  == -1] <- NA # gerar missing
Bolivia$P19ST.C[Bolivia$P19ST.C  == -2] <- NA
Bolivia$P16ST.F[Bolivia$P16ST.F  == -1] <- NA # gerar missing
Bolivia$P16ST.F[Bolivia$P16ST.F  == -2] <- NA
Bolivia$P19N.H[Bolivia$P19N.H  == -1] <- NA # gerar missing
Bolivia$P19N.H[Bolivia$P19N.H  == -2] <- NA
Bolivia$P12TG.B[Bolivia$P12TG.B  == -1] <- NA # gerar missing
Bolivia$P12TG.B[Bolivia$P12TG.B  == -2] <- NA
Bolivia$P46N.C[Bolivia$P46N.C  == -1] <- NA # gerar missing
Bolivia$P46N.C[Bolivia$P46N.C  == -2] <- NA
table(Bolivia$P69ST.C)#desaprova casamento gay
Bolivia$P69ST.C[Bolivia$P69ST.C  == -1] <- NA
Bolivia$P69ST.C[Bolivia$P69ST.C  == -2] <- NA

Chile <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 152)
table(Chile$P16TGB.B)#POLICIA
table(Chile$P19ST.C)#partidos
table(Chile$P16ST.F)#congresso
table(Chile$P19N.H)#justiça eleitoral
table(Chile$P46N.C)#antidireito
table(Chile$P12TG.B)#insatisfeito com economia
Chile$P16TGB.B[Chile$P16TGB.B  == -1] <- NA # gerar missing
Chile$P16TGB.B[Chile$P16TGB.B  == -2] <- NA
Chile$P19ST.C[Chile$P19ST.C  == -1] <- NA # gerar missing
Chile$P19ST.C[Chile$P19ST.C  == -2] <- NA
Chile$P16ST.F[Chile$P16ST.F  == -1] <- NA # gerar missing
Chile$P16ST.F[Chile$P16ST.F  == -2] <- NA
Chile$P19N.H[Chile$P19N.H  == -1] <- NA # gerar missing
Chile$P19N.H[Chile$P19N.H  == -2] <- NA
Chile$P12TG.B[Chile$P12TG.B  == -1] <- NA # gerar missing
Chile$P12TG.B[Chile$P12TG.B  == -2] <- NA
Chile$P46N.C[Chile$P46N.C  == -1] <- NA # gerar missing
Chile$P46N.C[Chile$P46N.C  == -2] <- NA
table(Chile$P69ST.C)#desaprova casamento gay
Chile$P69ST.C[Chile$P69ST.C  == -1] <- NA
Chile$P69ST.C[Chile$P69ST.C  == -2] <- NA

Colomb <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 170)
table(Colomb$P16TGB.B)#POLICIA
table(Colomb$P19ST.C)#partidos
table(Colomb$P16ST.F)#congresso
table(Colomb$P19N.H)#justiça eleitoral
table(Colomb$P46N.C)#antidireito
table(Colomb$P12TG.B)#insatisfeito com economia
Colomb$P16TGB.B[Colomb$P16TGB.B  == -1] <- NA # gerar missing
Colomb$P16TGB.B[Colomb$P16TGB.B  == -2] <- NA
Colomb$P19ST.C[Colomb$P19ST.C  == -1] <- NA # gerar missing
Colomb$P19ST.C[Colomb$P19ST.C  == -2] <- NA
Colomb$P16ST.F[Colomb$P16ST.F  == -1] <- NA # gerar missing
Colomb$P16ST.F[Colomb$P16ST.F  == -2] <- NA
Colomb$P19N.H[Colomb$P19N.H  == -1] <- NA # gerar missing
Colomb$P19N.H[Colomb$P19N.H  == -2] <- NA
Colomb$P12TG.B[Colomb$P12TG.B  == -1] <- NA # gerar missing
Colomb$P12TG.B[Colomb$P12TG.B  == -2] <- NA
Colomb$P46N.C[Colomb$P46N.C  == -1] <- NA # gerar missing
Colomb$P46N.C[Colomb$P46N.C  == -2] <- NA
table(Colomb$P69ST.C)#desaprova casamento gay
Colomb$P69ST.C[Colomb$P69ST.C  == -1] <- NA
Colomb$P69ST.C[Colomb$P69ST.C  == -2] <- NA


CostaRica <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 188)
table(CostaRica$P16TGB.B)#POLICIA
table(CostaRica$P19ST.C)#partidos
table(CostaRica$P16ST.F)#congresso
table(CostaRica$P19N.H)#justiça eleitoral
table(CostaRica$P46N.C)#antidireito
table(CostaRica$P12TG.B)#insatisfeito com economia
CostaRica$P16TGB.B[CostaRica$P16TGB.B  == -1] <- NA # gerar missing
CostaRica$P16TGB.B[CostaRica$P16TGB.B  == -2] <- NA
CostaRica$P19ST.C[CostaRica$P19ST.C  == -1] <- NA # gerar missing
CostaRica$P19ST.C[CostaRica$P19ST.C  == -2] <- NA
CostaRica$P16ST.F[CostaRica$P16ST.F  == -1] <- NA # gerar missing
CostaRica$P16ST.F[CostaRica$P16ST.F  == -2] <- NA
CostaRica$P19N.H[CostaRica$P19N.H  == -1] <- NA # gerar missing
CostaRica$P19N.H[CostaRica$P19N.H  == -2] <- NA
CostaRica$P12TG.B[CostaRica$P12TG.B  == -1] <- NA # gerar missing
CostaRica$P12TG.B[CostaRica$P12TG.B  == -2] <- NA
CostaRica$P46N.C[CostaRica$P46N.C  == -1] <- NA # gerar missing
CostaRica$P46N.C[CostaRica$P46N.C  == -2] <- NA
table(CostaRica$P69ST.C)#desaprova casamento gay
CostaRica$P69ST.C[CostaRica$P69ST.C  == -1] <- NA
CostaRica$P69ST.C[CostaRica$P69ST.C  == -2] <- NA


RepDom <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 214)
table(RepDom$P16TGB.B)#POLICIA
table(RepDom$P19ST.C)#partidos
table(RepDom$P16ST.F)#congresso
table(RepDom$P19N.H)#justiça eleitoral
table(RepDom$P46N.C)#antidireito
table(RepDom$P12TG.B)#insatisfeito com economia
RepDom$P16TGB.B[RepDom$P16TGB.B  == -1] <- NA # gerar missing
RepDom$P16TGB.B[RepDom$P16TGB.B  == -2] <- NA
RepDom$P19ST.C[RepDom$P19ST.C  == -1] <- NA # gerar missing
RepDom$P19ST.C[RepDom$P19ST.C  == -2] <- NA
RepDom$P16ST.F[RepDom$P16ST.F  == -1] <- NA # gerar missing
RepDom$P16ST.F[RepDom$P16ST.F  == -2] <- NA
RepDom$P19N.H[RepDom$P19N.H  == -1] <- NA # gerar missing
RepDom$P19N.H[RepDom$P19N.H  == -2] <- NA
RepDom$P12TG.B[RepDom$P12TG.B  == -1] <- NA # gerar missing
RepDom$P12TG.B[RepDom$P12TG.B  == -2] <- NA
RepDom$P46N.C[RepDom$P46N.C  == -1] <- NA # gerar missing
RepDom$P46N.C[RepDom$P46N.C  == -2] <- NA
table(RepDom$P69ST.C)#desaprova casamento gay
RepDom$P69ST.C[RepDom$P69ST.C  == -1] <- NA
RepDom$P69ST.C[RepDom$P69ST.C  == -2] <- NA

Ecuador <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 218)
table(Ecuador$P16TGB.B)#POLICIA
table(Ecuador$P19ST.C)#partidos
table(Ecuador$P16ST.F)#congresso
table(Ecuador$P19N.H)#justiça eleitoral
table(Ecuador$P46N.C)#antidireito
table(Ecuador$P12TG.B)#insatisfeito com economia
Ecuador$P16TGB.B[Ecuador$P16TGB.B  == -1] <- NA # gerar missing
Ecuador$P16TGB.B[Ecuador$P16TGB.B  == -2] <- NA
Ecuador$P19ST.C[Ecuador$P19ST.C  == -1] <- NA # gerar missing
Ecuador$P19ST.C[Ecuador$P19ST.C  == -2] <- NA
Ecuador$P16ST.F[Ecuador$P16ST.F  == -1] <- NA # gerar missing
Ecuador$P16ST.F[Ecuador$P16ST.F  == -2] <- NA
Ecuador$P19N.H[Ecuador$P19N.H  == -1] <- NA # gerar missing
Ecuador$P19N.H[Ecuador$P19N.H  == -2] <- NA
Ecuador$P12TG.B[Ecuador$P12TG.B  == -1] <- NA # gerar missing
Ecuador$P12TG.B[Ecuador$P12TG.B  == -2] <- NA
Ecuador$P46N.C[Ecuador$P46N.C  == -1] <- NA # gerar missing
Ecuador$P46N.C[Ecuador$P46N.C  == -2] <- NA
table(Ecuador$P69ST.C)#desaprova casamento gay
Ecuador$P69ST.C[Ecuador$P69ST.C  == -1] <- NA
Ecuador$P69ST.C[Ecuador$P69ST.C  == -2] <- NA


elSalv <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 222)
table(elSalv$P16TGB.B)#POLICIA
table(elSalv$P19ST.C)#partidos
table(elSalv$P16ST.F)#congresso
table(elSalv$P19N.H)#justiça eleitoral
table(elSalv$P46N.C)#antidireito
table(elSalv$P12TG.B)#insatisfeito com economia
elSalv$P16TGB.B[elSalv$P16TGB.B  == -1] <- NA # gerar missing
elSalv$P16TGB.B[elSalv$P16TGB.B  == -2] <- NA
elSalv$P19ST.C[elSalv$P19ST.C  == -1] <- NA # gerar missing
elSalv$P19ST.C[elSalv$P19ST.C  == -2] <- NA
elSalv$P16ST.F[elSalv$P16ST.F  == -1] <- NA # gerar missing
elSalv$P16ST.F[elSalv$P16ST.F  == -2] <- NA
elSalv$P19N.H[elSalv$P19N.H  == -1] <- NA # gerar missing
elSalv$P19N.H[elSalv$P19N.H  == -2] <- NA
elSalv$P12TG.B[elSalv$P12TG.B  == -1] <- NA # gerar missing
elSalv$P12TG.B[elSalv$P12TG.B  == -2] <- NA
elSalv$P46N.C[elSalv$P46N.C  == -1] <- NA # gerar missing
elSalv$P46N.C[elSalv$P46N.C  == -2] <- NA
table(elSalv$P69ST.C)#desaprova casamento gay
elSalv$P69ST.C[elSalv$P69ST.C  == -1] <- NA
elSalv$P69ST.C[elSalv$P69ST.C  == -2] <- NA


Guat <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 320)
table(Guat$P16TGB.B)#POLICIA
table(Guat$P19ST.C)#partidos
table(Guat$P16ST.F)#congresso
table(Guat$P19N.H)#justiça eleitoral
table(Guat$P46N.C)#antidireito
table(Guat$P12TG.B)#insatisfeito com economia
Guat$P16TGB.B[Guat$P16TGB.B  == -1] <- NA # gerar missing
Guat$P16TGB.B[Guat$P16TGB.B  == -2] <- NA
Guat$P19ST.C[Guat$P19ST.C  == -1] <- NA # gerar missing
Guat$P19ST.C[Guat$P19ST.C  == -2] <- NA
Guat$P16ST.F[Guat$P16ST.F  == -1] <- NA # gerar missing
Guat$P16ST.F[Guat$P16ST.F  == -2] <- NA
Guat$P19N.H[Guat$P19N.H  == -1] <- NA # gerar missing
Guat$P19N.H[Guat$P19N.H  == -2] <- NA
Guat$P12TG.B[Guat$P12TG.B  == -1] <- NA # gerar missing
Guat$P12TG.B[Guat$P12TG.B  == -2] <- NA
Guat$P46N.C[Guat$P46N.C  == -1] <- NA # gerar missing
Guat$P46N.C[Guat$P46N.C  == -2] <- NA
table(Guat$P69ST.C)#desaprova casamento gay
Guat$P69ST.C[Guat$P69ST.C  == -1] <- NA
Guat$P69ST.C[Guat$P69ST.C  == -2] <- NA

Hondu <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 340)
table(Hondu$P16TGB.B)#POLICIA
table(Hondu$P19ST.C)#partidos
table(Hondu$P16ST.F)#congresso
table(Hondu$P19N.H)#justiça eleitoral
table(Hondu$P46N.C)#antidireito
table(Hondu$P12TG.B)#insatisfeito com economia
Hondu$P16TGB.B[Hondu$P16TGB.B  == -1] <- NA # gerar missing
Hondu$P16TGB.B[Hondu$P16TGB.B  == -2] <- NA
Hondu$P19ST.C[Hondu$P19ST.C  == -1] <- NA # gerar missing
Hondu$P19ST.C[Hondu$P19ST.C  == -2] <- NA
Hondu$P16ST.F[Hondu$P16ST.F  == -1] <- NA # gerar missing
Hondu$P16ST.F[Hondu$P16ST.F  == -2] <- NA
Hondu$P19N.H[Hondu$P19N.H  == -1] <- NA # gerar missing
Hondu$P19N.H[Hondu$P19N.H  == -2] <- NA
Hondu$P12TG.B[Hondu$P12TG.B  == -1] <- NA # gerar missing
Hondu$P12TG.B[Hondu$P12TG.B  == -2] <- NA
Hondu$P46N.C[Hondu$P46N.C  == -1] <- NA # gerar missing
Hondu$P46N.C[Hondu$P46N.C  == -2] <- NA
table(Hondu$P69ST.C)#desaprova casamento gay
Hondu$P69ST.C[Hondu$P69ST.C  == -1] <- NA
Hondu$P69ST.C[Hondu$P69ST.C  == -2] <- NA

Mexico <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 484)
table(Mexico$P16TGB.B)#POLICIA
table(Mexico$P19ST.C)#partidos
table(Mexico$P16ST.F)#congresso
table(Mexico$P19N.H)#justiça eleitoral
table(Mexico$P46N.C)#antidireito
table(Mexico$P12TG.B)#insatisfeito com economia
Mexico$P16TGB.B[Mexico$P16TGB.B  == -1] <- NA # gerar missing
Mexico$P16TGB.B[Mexico$P16TGB.B  == -2] <- NA
Mexico$P19ST.C[Mexico$P19ST.C  == -1] <- NA # gerar missing
Mexico$P19ST.C[Mexico$P19ST.C  == -2] <- NA
Mexico$P16ST.F[Mexico$P16ST.F  == -1] <- NA # gerar missing
Mexico$P16ST.F[Mexico$P16ST.F  == -2] <- NA
Mexico$P19N.H[Mexico$P19N.H  == -1] <- NA # gerar missing
Mexico$P19N.H[Mexico$P19N.H  == -2] <- NA
Mexico$P12TG.B[Mexico$P12TG.B  == -1] <- NA # gerar missing
Mexico$P12TG.B[Mexico$P12TG.B  == -2] <- NA
Mexico$P46N.C[Mexico$P46N.C  == -1] <- NA # gerar missing
Mexico$P46N.C[Mexico$P46N.C  == -2] <- NA
table(Mexico$P69ST.C)#desaprova casamento gay
Mexico$P69ST.C[Mexico$P69ST.C  == -1] <- NA
Mexico$P69ST.C[Mexico$P69ST.C  == -2] <- NA

Nic <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 558)
table(Nic$P16TGB.B)#POLICIA
table(Nic$P19ST.C)#partidos
table(Nic$P16ST.F)#congresso
table(Nic$P19N.H)#justiça eleitoral
table(Nic$P46N.C)#antidireito
table(Nic$P12TG.B)#insatisfeito com economia
Nic$P16TGB.B[Nic$P16TGB.B  == -1] <- NA # gerar missing
Nic$P16TGB.B[Nic$P16TGB.B  == -2] <- NA
Nic$P19ST.C[Nic$P19ST.C  == -1] <- NA # gerar missing
Nic$P19ST.C[Nic$P19ST.C  == -2] <- NA
Nic$P16ST.F[Nic$P16ST.F  == -1] <- NA # gerar missing
Nic$P16ST.F[Nic$P16ST.F  == -2] <- NA
Nic$P19N.H[Nic$P19N.H  == -1] <- NA # gerar missing
Nic$P19N.H[Nic$P19N.H  == -2] <- NA
Nic$P12TG.B[Nic$P12TG.B  == -1] <- NA # gerar missing
Nic$P12TG.B[Nic$P12TG.B  == -2] <- NA
Nic$P46N.C[Nic$P46N.C  == -1] <- NA # gerar missing
Nic$P46N.C[Nic$P46N.C  == -2] <- NA
table(Nic$P69ST.C)#desaprova casamento gay
Nic$P69ST.C[Nic$P69ST.C  == -1] <- NA
Nic$P69ST.C[Nic$P69ST.C  == -2] <- NA


Panama <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 591)
table(Panama$P16TGB.B)#POLICIA
table(Panama$P19ST.C)#partidos
table(Panama$P16ST.F)#congresso
table(Panama$P19N.H)#justiça eleitoral
table(Panama$P46N.C)#antidireito
table(Panama$P12TG.B)#insatisfeito com economia
Panama$P16TGB.B[Panama$P16TGB.B  == -1] <- NA # gerar missing
Panama$P16TGB.B[Panama$P16TGB.B  == -2] <- NA
Panama$P19ST.C[Panama$P19ST.C  == -1] <- NA # gerar missing
Panama$P19ST.C[Panama$P19ST.C  == -2] <- NA
Panama$P16ST.F[Panama$P16ST.F  == -1] <- NA # gerar missing
Panama$P16ST.F[Panama$P16ST.F  == -2] <- NA
Panama$P19N.H[Panama$P19N.H  == -1] <- NA # gerar missing
Panama$P19N.H[Panama$P19N.H  == -2] <- NA
Panama$P12TG.B[Panama$P12TG.B  == -1] <- NA # gerar missing
Panama$P12TG.B[Panama$P12TG.B  == -2] <- NA
Panama$P46N.C[Panama$P46N.C  == -1] <- NA # gerar missing
Panama$P46N.C[Panama$P46N.C  == -2] <- NA
table(Panama$P69ST.C)#desaprova casamento gay
Panama$P69ST.C[Panama$P69ST.C  == -1] <- NA
Panama$P69ST.C[Panama$P69ST.C  == -2] <- NA

Paraguai <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 600)
table(Paraguai$P16TGB.B)#POLICIA
table(Paraguai$P19ST.C)#partidos
table(Paraguai$P16ST.F)#congresso
table(Paraguai$P19N.H)#justiça eleitoral
table(Paraguai$P46N.C)#antidireito
table(Paraguai$P12TG.B)#insatisfeito com economia
Paraguai$P16TGB.B[Paraguai$P16TGB.B  == -1] <- NA # gerar missing
Paraguai$P16TGB.B[Paraguai$P16TGB.B  == -2] <- NA
Paraguai$P19ST.C[Paraguai$P19ST.C  == -1] <- NA # gerar missing
Paraguai$P19ST.C[Paraguai$P19ST.C  == -2] <- NA
Paraguai$P16ST.F[Paraguai$P16ST.F  == -1] <- NA # gerar missing
Paraguai$P16ST.F[Paraguai$P16ST.F  == -2] <- NA
Paraguai$P19N.H[Paraguai$P19N.H  == -1] <- NA # gerar missing
Paraguai$P19N.H[Paraguai$P19N.H  == -2] <- NA
Paraguai$P12TG.B[Paraguai$P12TG.B  == -1] <- NA # gerar missing
Paraguai$P12TG.B[Paraguai$P12TG.B  == -2] <- NA
Paraguai$P46N.C[Paraguai$P46N.C  == -1] <- NA # gerar missing
Paraguai$P46N.C[Paraguai$P46N.C  == -2] <- NA
table(Paraguai$P69ST.C)#desaprova casamento gay
Paraguai$P69ST.C[Paraguai$P69ST.C  == -1] <- NA
Paraguai$P69ST.C[Paraguai$P69ST.C  == -2] <- NA


Peru <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 604)
table(Peru$P16TGB.B)#POLICIA
table(Peru$P19ST.C)#partidos
table(Peru$P16ST.F)#congresso
table(Peru$P19N.H)#justiça eleitoral
table(Peru$P46N.C)#antidireito
table(Peru$P12TG.B)#insatisfeito com economia
Peru$P16TGB.B[Peru$P16TGB.B  == -1] <- NA # gerar missing
Peru$P16TGB.B[Peru$P16TGB.B  == -2] <- NA
Peru$P19ST.C[Peru$P19ST.C  == -1] <- NA # gerar missing
Peru$P19ST.C[Peru$P19ST.C  == -2] <- NA
Peru$P16ST.F[Peru$P16ST.F  == -1] <- NA # gerar missing
Peru$P16ST.F[Peru$P16ST.F  == -2] <- NA
Peru$P19N.H[Peru$P19N.H  == -1] <- NA # gerar missing
Peru$P19N.H[Peru$P19N.H  == -2] <- NA
Peru$P12TG.B[Peru$P12TG.B  == -1] <- NA # gerar missing
Peru$P12TG.B[Peru$P12TG.B  == -2] <- NA
Peru$P46N.C[Peru$P46N.C  == -1] <- NA # gerar missing
Peru$P46N.C[Peru$P46N.C  == -2] <- NA
table(Peru$P69ST.C)#desaprova casamento gay
Peru$P69ST.C[Peru$P69ST.C  == -1] <- NA
Peru$P69ST.C[Peru$P69ST.C  == -2] <- NA




Uru <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 858)
table(Uru$P16TGB.B)#POLICIA
table(Uru$P19ST.C)#partidos
table(Uru$P16ST.F)#congresso
table(Uru$P19N.H)#justiça eleitoral
table(Uru$P46N.C)#antidireito
table(Uru$P12TG.B)#insatisfeito com economia
Uru$P16TGB.B[Uru$P16TGB.B  == -1] <- NA # gerar missing
Uru$P16TGB.B[Uru$P16TGB.B  == -2] <- NA
Uru$P19ST.C[Uru$P19ST.C  == -1] <- NA # gerar missing
Uru$P19ST.C[Uru$P19ST.C  == -2] <- NA
Uru$P16ST.F[Uru$P16ST.F  == -1] <- NA # gerar missing
Uru$P16ST.F[Uru$P16ST.F  == -2] <- NA
Uru$P19N.H[Uru$P19N.H  == -1] <- NA # gerar missing
Uru$P19N.H[Uru$P19N.H  == -2] <- NA
Uru$P12TG.B[Uru$P12TG.B  == -1] <- NA # gerar missing
Uru$P12TG.B[Uru$P12TG.B  == -2] <- NA
Uru$P46N.C[Uru$P46N.C  == -1] <- NA # gerar missing
Uru$P46N.C[Uru$P46N.C  == -2] <- NA
table(Uru$P69ST.C)#desaprova casamento gay
Uru$P69ST.C[Uru$P69ST.C  == -1] <- NA
Uru$P69ST.C[Uru$P69ST.C  == -2] <- NA


Ven <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 862)
table(Ven$P16TGB.B)#POLICIA
table(Ven$P19ST.C)#partidos
table(Ven$P16ST.F)#congresso
table(Ven$P19N.H)#justiça eleitoral
table(Ven$P46N.C)#antidireito
table(Ven$P12TG.B)#insatisfeito com economia
Ven$P16TGB.B[Ven$P16TGB.B  == -1] <- NA # gerar missing
Ven$P16TGB.B[Ven$P16TGB.B  == -2] <- NA
Ven$P19ST.C[Ven$P19ST.C  == -1] <- NA # gerar missing
Ven$P19ST.C[Ven$P19ST.C  == -2] <- NA
Ven$P16ST.F[Ven$P16ST.F  == -1] <- NA # gerar missing
Ven$P16ST.F[Ven$P16ST.F  == -2] <- NA
Ven$P19N.H[Ven$P19N.H  == -1] <- NA # gerar missing
Ven$P19N.H[Ven$P19N.H  == -2] <- NA
Ven$P12TG.B[Ven$P12TG.B  == -1] <- NA # gerar missing
Ven$P12TG.B[Ven$P12TG.B  == -2] <- NA
Ven$P46N.C[Ven$P46N.C  == -1] <- NA # gerar missing
Ven$P46N.C[Ven$P46N.C  == -2] <- NA
table(Ven$P69ST.C)#desaprova casamento gay
Ven$P69ST.C[Ven$P69ST.C  == -1] <- NA
Ven$P69ST.C[Ven$P69ST.C  == -2] <- NA

rm(Latinobarometro_2015_Esp)

# Função para reduzir um data frame para as variáveis desejadas
reduce_dataframe <- function(df) {
  selected_vars <- c("P16TGB.B", "P19ST.C", "P16ST.F", "P19N.H", "P46N.C", "P12TG.B", "P69ST.C")
  reduced_df <- df[, selected_vars, drop = FALSE]
  return(reduced_df)
}

df_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colomb", "CostaRica",
              "Ecuador", "elSalv", "Hondu", "Mexico", "Nic", "Panama", "Paraguai",
              "Peru", "RepDom", "Uru", "Ven","Guat")

# Loop para reduzir todos os data frames
for (df_name in df_names) {
  if (is.data.frame(get(df_name))) {
    assign(df_name, reduce_dataframe(get(df_name)))
  }
}

#pasta_raiz <- "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/latinobaromterodf15_especi_PELA_casGay/latinoBar_2015_resultados afc apendice cap 2 tese"

df_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colomb", "CostaRica",
             "Ecuador", "elSalv", "Hondu", "Mexico", "Nic", "Panama", "Paraguai",
            "Peru", "RepDom", "Uru", "Ven", "Guat")

# Crie as subpastas para cada data base
#for (nome in df_names) {
 # caminho_subpasta <- file.path(pasta_raiz, nome)
 # dir.create(caminho_subpasta, recursive = TRUE, showWarnings = FALSE)
#}

# Listar as pastas criadas
#list.dirs(path = pasta_raiz, full.names = TRUE, recursive = TRUE)

#remover NAS

# Lista de nomes dos dataframes
df_names <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colomb", "CostaRica",
              "Ecuador", "elSalv", "Hondu", "Mexico", "Nic", "Panama", "Paraguai",
              "Peru", "RepDom", "Uru", "Ven", "Guat")

# Loop para remover os NA de cada dataframe
for (df_name in df_names) {
  # Acesse o dataframe pelo nome
  df <- get(df_name)
  
  # Remova as linhas com NA
  df <- na.omit(df)
  
  # Atribua o dataframe de volta ao seu nome original
  assign(df_name, df)
}

