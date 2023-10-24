#obs rodar parte 1 antes
setwd("C:/Users/grego/OneDrive/Desktop/work/tese/escrevinhando o cap 2/latinoBar_2015_resultados afc apendice cap 2 tese")
Argentina %>% glimpse()#pra ver se deu certo

library(lavaan)

library(semTools)
library(psych)





Argentina$Desconfia_Policia <- Argentina$P16TGB.B
table(Argentina$P16TGB.B)#POLICIA
table(Argentina$P19ST.C)#partidos

Argentina$Desconfia_Partidos <- Argentina$P19ST.C
Argentina$Desconfia_Parlamento <- Argentina$P16ST.F
table(Argentina$P16ST.F)#congresso

Argentina$Desconfia_Justica <- Argentina$P19N.H
table(Argentina$P19N.H)#justiça eleitoral
table(Argentina$P46N.C)#antidireito
Argentina$ContraDireitosTrabalhistas <- Argentina$P46N.C
Argentina$ContraCasamentoGay <-Argentina$P69ST.C


Argentina %>% glimpse()#pra ver se deu certo


# Defina o modelo AFC padrão com os parâmetros fixos
modelo_afc <- '
  # Modelo AFC padrão
  DesconfiançaInst =~ Desconfia_Policia + Desconfia_Justica + Desconfia_Partidos + Desconfia_Parlamento
  QuestõesEconômicas =~ ContraDireitosTrabalhistas
  CasamentoGay =~ ContraCasamentoGay
'
Argentina_fit <- cfa(modelo_afc, data = Argentina)
summary(Argentina_fit, standardized = TRUE)
semTools::fitmeasures(Argentina_fit, c("tli", "cfi", "rmsea", "srmr"))
library(lavaanPlot)
??lavaanPlot
lavaanPlot(model = Argentina_fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
