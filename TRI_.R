#TRI

#### packages 
library(mirt)
library(tidyverse)
library(mice)
#  primeiro criar os modelos

#ONDA 1 #####
# arg01
arg01m <- read_csv("arg01m.csv")
arg01d <- read_csv("arg01d.csv")
summary(arg01m)
arg01m <- subset(arg01m, select=-c(X049,X051,Y001,E179WVS,E182,X025R))#excluir as NANS
arg01d <- subset(arg01d, select=-c(X049,X051,Y001,E179WVS,E182,X025R))#excluir as NANS
imp <- mice(arg01m, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
arg01m <-complete(imp, 1)
rm(imp)
#
m_arg01d <- mirt(arg01d[1:8],4)
m_arg01m <- mirt(arg01m[1:8],4)


#mexico 01
mex01m <- read_csv("mex01m.csv")
mex01d <- read_csv("mex01d.csv")
summary(mex01m)# pra conferir se precisa excluir NANS
mex01m <- subset(mex01m, select=-c(X049,X051,Y001,E179WVS,E182,X025R))#excluir as NANS
mex01d <- subset(mex01d, select=-c(X049,X051,Y001,E179WVS,E182,X025R))#excluir as NANS
imp <- mice(mex01m, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
mex01m <-complete(imp, 1)
rm(imp)
#
m_mex01d <- mirt(mex01d[1:8],4)#Cuidado com o intervalo das colunas - conferir caso a caso
m_mex01m <- mirt(mex01m[1:8],4)

modelosOnda1 <- list(m_arg01d, m_arg01m, m_mex01d, m_mex01m)#backup de segurança
save(modelosOnda1, file="TRI_ONDA1.RData")#backup de segurança
# não gerei os fscores dos modelos da onda 1 - mas como fiz backup posso conseguir lá

#resultados m_

summary(m_arg01d, suppress =0.399)#rotação oblimin padrão?
summary(m_arg01m, suppress =0.399)
summary(m_arg01d, rotate = "varimax", suppress = 0.399)
summary(m_arg01m, rotate = "varimax", suppress = 0.399)

summary(m_mex01d, rotate = "varimax", suppress = 0.399)
summary(m_mex01m, rotate = "varimax", suppress = 0.399)

rm(arg01d,arg01m,mex01d,mex01m)#por hora

#ONDAS 2 a 7 ########

# primeiro criar as bases

#arg02
arg02m <- read_csv("arg02m.csv")
arg02d <- read_csv("arg02d.csv")
summary(arg02m)# pra conferir se precisa excluir NANS
arg02m <- subset(arg02m, select=-c(X051,E179WVS,E182,X025R))#excluir as NANS
arg02d <- subset(arg02d, select=-c(X051,E179WVS,E182,X025R))#excluir as NANS
imp <- mice(arg02m, seed=23109)# repetir o mesmo seed usado na argentina(e em qualquer país)
arg02m <-complete(imp, 1)
rm(imp)


#mex02
mex02m <- read_csv("mex02m.csv")
mex02d <- read_csv("mex02d.csv")
summary(mex02m)# pra conferir se precisa excluir NANS
mex02m <- subset(mex02m, select=-c(E182,X025R))#excluir as NANS
mex02d <- subset(mex02d, select=-c(E182,X025R))#excluir as NANS
options(scipen=999)
mex02m$B006 <- as.factor(mex02m$B006)
imp <- mice(mex02m[1:15], m=1,seed=23109)# repetir o mesmo seed  (e em qualquer país)
mex02m <-complete(imp, 1)
rm(imp)

#bra02
bra02m <- read_csv("bra02m.csv")
bra02d <- read_csv("bra02d.csv")
summary(bra02m)# pra conferir se precisa excluir NANS
imp <- mice(bra02m, m=1,seed=23109)# repetir o mesmo seed 
bra02m <-complete(imp, 1)
rm(imp)

#chi02
chi02m <- read_csv("chi02m.csv")
chi02d <- read_csv("chi02d.csv")
summary(chi02m)# pra conferir se precisa excluir NANS
imp <- mice(chi02m, m=1,seed=23109)# repetir o mesmo seed 
chi02m <-complete(imp, 1)
rm(imp)

#arg03
arg03m <- read_csv("arg03m.csv")
arg03d <- read_csv("arg03d.csv")
summary(arg03m)# pra conferir se precisa excluir NANS
imp <- mice(arg03m, m=1,seed=23109)# repetir o mesmo seed
arg03m <-complete(imp, 1)
rm(imp)

#chi03
chi03m <- read_csv("chi03m.csv")
chi03d <- read_csv("chi03d.csv")
summary(chi03m)# pra conferir se precisa excluir NANS
imp <- mice(chi03m, m=1,seed=23109)# repetir o mesmo seed 
chi03m <-complete(imp, 1)
rm(imp)

#mex03
mex03m <- read_csv("mex03m.csv")
mex03d <- read_csv("mex03d.csv")
summary(mex03m)# pra conferir se precisa excluir NANS
imp <- mice(mex03m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na mexentina(e em qualquer país)
mex03m <-complete(imp, 1)
rm(imp)

#peru03
peru03m <- read_csv("peru03m.csv")
peru03d <- read_csv("peru03d.csv")
peru03d <- subset(peru03d, select=-c(X049))
peru03m <- subset(peru03m, select=-c(X049))
summary(peru03m)# pra conferir se precisa excluir NANS
imp <- mice(peru03m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na peruentina(e em qualquer país)
peru03m <-complete(imp, 1)
rm(imp)

#uru03
uru03m <- read_csv("uru03m.csv")
uru03d <- read_csv("uru03d.csv")
summary(uru03m)# pra conferir se precisa excluir NANS
imp <- mice(uru03m, m=1,seed=23109)# repetir o mesmo seed usado na uruentina(e em qualquer país)
uru03m <-complete(imp, 1)
rm(imp)

#ven03
ven03m <- read_csv("ven03m.csv")
ven03d <- read_csv("ven03d.csv")
summary(ven03m)# pra conferir se precisa excluir NANS
imp <- mice(ven03m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
ven03m <-complete(imp, 1)
rm(imp)

#arg04
arg04m <- read_csv("arg04m.csv")
arg04d <- read_csv("arg04d.csv")
summary(arg04m)# pra conferir se precisa excluir NANS
arg04d <- subset(arg04d, select=-c(X049,X051))
arg04m <- subset(arg04m, select=-c(X049,X051))
imp <- mice(arg04m[1:14], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
arg04m <-complete(imp, 1)
rm(imp)
#mex04
mex04m <- read_csv("mex04m.csv")
mex04d <- read_csv("mex04d.csv")
summary(mex04m)# pra conferir se precisa excluir NANS
imp <- mice(mex04m[1:14], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
mex04m <-complete(imp, 1)
rm(imp)
#peru04
peru04m <- read_csv("peru04m.csv")
peru04d <- read_csv("peru04d.csv")
summary(peru04m)# pra conferir se precisa excluir NANS
peru04d <- subset(peru04d, select=-c(X049))
peru04m <- subset(peru04m, select=-c(X049))
imp <- mice(peru04m[1:14], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
peru04m <-complete(imp, 1)
rm(imp)
#ch04
ch04m <- read_csv("ch04m.csv")
ch04d <- read_csv("ch04d.csv")
summary(ch04m)# pra conferir se precisa excluir NANS
imp <- mice(ch04m[1:14], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
ch04m <-complete(imp, 1)
rm(imp)
#ven04
ven04m <- read_csv("ven04m.csv")
ven04d <- read_csv("ven04d.csv")
summary(ven04m)# pra conferir se precisa excluir NANS
imp <- mice(ven04m[1:14], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
ven04m <-complete(imp, 1)#verificar se tá na ordem correta
rm(imp)
#arg05
arg05m <- read_csv("arg05m.csv")
arg05d <- read_csv("arg05d.csv")
summary(arg05m)# pra conferir se precisa excluir NANS
imp <- mice(arg05m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
arg05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#bra05
bra05m <- read_csv("bra05m.csv")
bra05d <- read_csv("bra05d.csv")
summary(bra05m)# pra conferir se precisa excluir NANS
imp <- mice(bra05m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
bra05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#chi05
chi05m <- read_csv("chi05m.csv")
chi05d <- read_csv("chi05d.csv")
summary(chi05m)# pra conferir se precisa excluir NANS
imp <- mice(chi05m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
chi05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#col05
col05m <- read_csv("col05m.csv")
col05d <- read_csv("col05d.csv")
summary(col05m)# pra conferir se precisa excluir NANS
imp <- mice(col05m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
col05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#guat05
guat05m <- read_csv("guat05m.csv")
guat05d <- read_csv("guat05d.csv")
summary(guat05m)# pra conferir se precisa excluir NANS
imp <- mice(guat05m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
guat05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#mex05
mex05m <- read_csv("mex05m.csv")
mex05d <- read_csv("mex05d.csv")
summary(mex05m)# pra conferir se precisa excluir NANS
imp <- mice(mex05m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
mex05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#peru05
peru05m <- read_csv("peru05m.csv")
peru05d <- read_csv("peru05d.csv")
summary(peru05m)# pra conferir se precisa excluir NANS
imp <- mice(peru05m[1:8], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
peru05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#uru05
uru05m <- read_csv("uru05m.csv")
uru05d <- read_csv("uru05d.csv")
summary(uru05m)# pra conferir se precisa excluir NANS
imp <- mice(uru05m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
uru05m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#arg06
arg06m <- read_csv("arg06m.csv")
arg06d <- read_csv("arg06d.csv")
summary(arg06m)# pra conferir se precisa excluir NANS
imp <- mice(arg06m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
arg06m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#bra06
bra06m <- read_csv("bra06m.csv")
bra06d <- read_csv("bra06d.csv")
summary(bra06m)# pra conferir se precisa excluir NANS
imp <- mice(bra06m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
bra06m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#uru06
uru06m <- read_csv("uru06m.csv")
uru06d <- read_csv("uru06d.csv")
summary(uru06m)# pra conferir se precisa excluir NANS
imp <- mice(uru06m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
uru06m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#ch6
ch6m <- read_csv("ch6m.csv")
ch6d <- read_csv("ch6d.csv")
summary(ch6m)# pra conferir se precisa excluir NANS
imp <- mice(ch6m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
ch6m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#mex06
mex06m <- read_csv("mex06m.csv")
mex06d <- read_csv("mex06d.csv")
summary(mex06m)# pra conferir se precisa excluir NANS
imp <- mice(mex06m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
mex06m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#col06
col06m <- read_csv("col06m.csv")
col06d <- read_csv("col06d.csv")
summary(col06m)# pra conferir se precisa excluir NANS
imp <- mice(col06m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
col06m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#haiti6
haiti6m <- read_csv("haiti6m.csv")
haiti6d <- read_csv("haiti6d.csv")
summary(haiti6m)# pra conferir se precisa excluir NANS
imp <- mice(haiti6m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
haiti6m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#peru06
peru06m <- read_csv("peru06m.csv")
peru06d <- read_csv("peru06d.csv")
summary(peru06m)# pra conferir se precisa excluir NANS
imp <- mice(peru06m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
peru06m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#equad06
equad06m <- read_csv("equad06m.csv")
equad06d <- read_csv("equad06d.csv")
summary(equad06m)# pra conferir se precisa excluir NANS
imp <- mice(equad06m[1:13], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
equad06m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#br7
br7m <- read_csv("br7m.csv")
br7d <- read_csv("br7d.csv")
summary(br7m)# pra conferir se precisa excluir NANS
imp <- mice(br7m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
br7m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#ch7
ch7m <- read_csv("ch7m.csv")
ch7d <- read_csv("ch7d.csv")
summary(ch7m)# pra conferir se precisa excluir NANS
imp <- mice(ch7m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
ch7m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#arg07
arg07m <- read_csv("arg07m.csv")
arg07d <- read_csv("arg07d.csv")
summary(arg07m)# pra conferir se precisa excluir NANS
imp <- mice(arg07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
arg07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#ven07
ven07m <- read_csv("ven07m.csv")
ven07d <- read_csv("ven07d.csv")
summary(ven07m)# pra conferir se precisa excluir NANS
imp <- mice(ven07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na venentina(e em qualquer país)
ven07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#mex07
mex07m <- read_csv("mex07m.csv")
mex07d <- read_csv("mex07d.csv")
summary(mex07m)# pra conferir se precisa excluir NANS
imp <- mice(mex07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na mexentina(e em qualquer país)
mex07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#bol07
bol07m <- read_csv("bol07m.csv")
bol07d <- read_csv("bol07d.csv")
summary(bol07m)# pra conferir se precisa excluir NANS
imp <- mice(bol07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na bolentina(e em qualquer país)
bol07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#col07
col07m <- read_csv("col07m.csv")
col07d <- read_csv("col07d.csv")
summary(col07m)# pra conferir se precisa excluir NANS
imp <- mice(col07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na colentina(e em qualquer país)
col07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#equ07
equ07m <- read_csv("equ07m.csv")
equ07d <- read_csv("equ07d.csv")
summary(equ07m)# pra conferir se precisa excluir NANS
imp <- mice(equ07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na equentina(e em qualquer país)
equ07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#guat07
guat07m <- read_csv("guat07m.csv")
guat07d <- read_csv("guat07d.csv")
summary(guat07m)# pra conferir se precisa excluir NANS
imp <- mice(guat07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na guatentina(e em qualquer país)
guat07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#nicaragua07
nicaragua07m <- read_csv("nicaragua07m.csv")
nicaragua07d <- read_csv("nicaragua07d.csv")
summary(nicaragua07m)# pra conferir se precisa excluir NANS
imp <- mice(nicaragua07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na nicaraguaentina(e em qualquer país)
nicaragua07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#Peru07
Peru07m <- read_csv("Peru07m.csv")
Peru07d <- read_csv("Peru07d.csv")
summary(Peru07m)# pra conferir se precisa excluir NANS
imp <- mice(Peru07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na Peruentina(e em qualquer país)
Peru07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)
#PortoRico07
PortoRico07m <- read_csv("PortoRico07m.csv")
PortoRico07d <- read_csv("PortoRico07d.csv")
summary(PortoRico07m)# pra conferir se precisa excluir NANS
imp <- mice(PortoRico07m[1:15], m=1,seed=23109)# repetir o mesmo seed usado na PortoRicoentina(e em qualquer país)
PortoRico07m <-complete(imp, 1)#verificar se termina em G006
rm(imp)

#bra03
bra03m <- read_csv("bra03m.csv")
bra03d <- read_csv("bra03d.csv")
summary(bra03m)# pra conferir se precisa excluir NANS
imp <- mice(bra03m[1:15], m=1,seed=23109)# repetir o mesmo seed
bra03m <-complete(imp, 1)
rm(imp)

#TRIS ondas 2 a 7 - rodar os modelos o que mais demora
# OBS alguns casos um modelo só poucos missings - parte mais demorando deixa rodando e vai passar demorará horas.
m_arg02d <- mirt(arg02d[1:15],4)
m_arg02m <- mirt(arg02m[1:15],4)
m_arg03d <- mirt(arg03d[1:14],4)
m_arg03m <- mirt(arg03m[1:14],4)
m_arg04d <- mirt(arg04d[1:14],4)
m_arg04m <- mirt(arg04m[1:14],4)
m_arg05d <- mirt(arg05d[1:13],4)
m_arg05m <- mirt(arg05m[1:13],4)
m_arg06d <- mirt(arg06d[1:13],4)
m_arg06m <- mirt(arg06m[1:13],4)
m_arg07d <- mirt(arg07d[1:15],4)
m_arg07m <- mirt(arg07m[1:15],4)
ARG2a7 <- list(m_arg02d, m_arg02m, m_arg03d, m_arg03m, m_arg04d, m_arg04m, m_arg05d,
               m_arg06d, m_arg06m, m_arg07d,m_arg07m)#backup de segurança
save(ARG2a7, file="ARG2a7.RData")

m_br7d <- mirt(br7d[1:15],4)
m_br7m <- mirt(br7m[1:15],4)
m_bra02d <- mirt(bra02d[1:15],4)
m_bra02m <- mirt(bra02d[1:15],4)
m_bra03m <- mirt(bra03m[1:14],4)
m_bra03d <- mirt(bra03d[1:14],4)
#5 e 6 deu bug
BRAtotal <- list(m_bra03m, m_bra03d, m_br7d, m_br7m, m_bra02d, m_bra02m, m_bra05d,
                 m_bra05m, m_bra06d, m_bra06m)
save(BRAtotal, file="BRAtotal.Rdata")#backup


m_ch7d <- mirt(ch7d[1:15],4)
m_ch7m <- mirt(ch7m[1:15],4)
m_chi02d <- mirt(chi02d[1:15],4)
m_chi02m <- mirt(chi02d[1:15],4)
m_chi05d <- mirt(chi05d[1:13],4)
m_chi05m <- mirt(chi05d[1:13],4)
#OBS -> m_chile6 foi esquecido na etapa anterior
m_chi03m <- mirt(chi03m[1:14],4)
m_chi03d <- mirt(chi03d[1:14],4)
chile_total <- list(m_chi03m, m_chi03d, m_ch7d, m_ch7m, m_chi02d, m_chi02m, m_chi05d,
                 m_chi05m)
save(chile_total, file="chitotal.Rdata")#backup

m_bol07d <- mirt(bol07d[1:15],4) #soh d
m_PortoRico07m <-mirt(PortoRico07m[1:15],4)
m_PortoRico07d <-mirt(PortoRico07d[1:15],4)
m_nicaragua07d <-mirt(nicaragua07d[1:15],4)#soh d
BOL_PORTORICO_NIC <- list(m_bol07d, m_PortoRico07d, m_PortoRico07m, m_nicaragua07d)
save(BOL_PORTORICO_NIC, file="BOL_PORTORICO_NIC.Rdata")

m_col05d <- mirt(col05d[1:13],4)#soh d 
m_col06d <- mirt(col06d[1:13],4)#soh d
m_col07d <- mirt(col07d[1:15],4)#soh d
COLOMBIA_TOTAL <- list(m_col05d, m_col06d, m_col07d)
save(COLOMBIA_TOTAL, file="COLOMBIA_TOTAL.Rdata")

m_equad06d <- mirt(equad06d[1:13],4)#soh d
m_equ07d <- mirt(equ07d[1:15],4)#soh d
ECUADOR_TOTAL <- list(m_equad06d, m_equ07d)
save(ECUADOR_TOTAL, file="ECUADOR_TOTAL.Rdata")

M_GUAT05D<-mirt(guat05d[1:13],4)#soh d
M_HAITI06D<-mirt(haiti6d[1:13],4)#soh d
M_GUAT07D<-mirt(guat07d[1:15],4)#soh d
HAITI_gUATEMALA <- list(M_HAITI06D, M_GUAT05D, M_GUAT07D)
save(HAITI_gUATEMALA, file="HAITI_E_GUATEMALA_.Rdata")

m_mex02d <- mirt(mex02d[1:15],4)
m_mex02m <- mirt(mex02m[1:15],4)
m_mex03d <- mirt(mex03d[1:14],4)
m_mex03m <- mirt(mex03m[1:14],4)
m_mex04d <- mirt(mex04d[1:14],4)
m_mex04m <- mirt(mex04m[1:14],4)
m_mex05d <- mirt(mex05d[1:13],4)
m_mex05m <- mirt(mex05m[1:13],4)
m_mex06d <- mirt(mex06d[1:13],4)
m_mex06m <- mirt(mex06m[1:13],4)
#m_mex07d nao fiz
#m_mex07m #não fiz
mex2a7 <- list(m_mex02d, m_mex02m, m_mex03d, m_mex03m, m_mex04d, m_mex04m, m_mex05d,
               m_mex06d, m_mex06m)#backup de segurança
save(mex2a7, file="mex2a71.RData")

m_uru03d <- mirt(uru03d[1:14],4)
m_uru03m <- mirt(uru03m[1:14],4)
m_uru05d <- mirt(uru05d[1:13],4)
m_uru05m <- mirt(uru05m[1:13],4)
m_uru06d <- mirt(uru06d[1:13],4)
m_uru06m <- mirt(uru06m[1:13],4)
uru_total <- list(m_uru03d, m_uru03m, m_uru05d,
                  m_uru6d, m_uru06m)#backup de segurança
save(uru_total, file="uru_total_TRI.RData")


m_ven03d <- mirt(ven03d[1:14],4)
m_ven03m <- mirt(ven03m[1:14],4)
m_ven04d <- mirt(ven04d[1:14],4)
m_ven04m <- mirt(ven04m[1:14],4)
m_ven07d <- mirt(ven06d[1:15],4)#sohD
vene_TRI <- list(m_ven03d, m_ven03m, m_ven04d, m_ven04m,m_ven07d)#backup de segurança
save(vene_TRI, file="ven_TRI.RData")


m_peru03m <- mirt(peru03m[1:15],4) # soh m
m_peru04m <- mirt(peru04m[1:14],4)
m_peru04d <- mirt(peru04d[1:14],4)
m_peru05d <- mirt(peru05d[1:13],4)
#m_peru05m não fiz
m_peru06d <- mirt(peru06d[1:13],4)
m_peru06m <- mirt(peru06m[1:13],4)
m_peru07d <- mirt(Peru07d[1:15],4)
m_peru07m <- mirt(Peru07m[1:13],4)
PERU_TRI <- list(m_peru03m, m_peru04d, m_peru04m, m_peru05d, m_peru06d,
                 m_peru06m, m_peru07m, m_peru07d)#backup de segurança
save(PERU_TRI, file="PERU.RData")


## resultados # m_
mirtCluster()
summary(m_arg02d, rotate="varimax", suppress=0.31)
summary(m_arg02m, rotate="varimax", suppress=0.31)
anova(m_arg02d, m_arg02m)

summary(m_arg03d, rotate="varimax", suppress=0.31)
summary(m_arg03m, rotate="varimax", suppress=0.31)

summary(m_arg04d, rotate="varimax", suppress=0.31)
summary(m_arg04m, rotate="varimax", suppress=0.31)

summary(m_arg05d, rotate="varimax", suppress=0.31)
summary(m_arg05m, rotate="varimax", suppress=0.31)

summary(m_arg06d, rotate="varimax", suppress=0.31)
summary(m_arg06m, rotate="varimax", suppress=0.31)

summary(m_arg07d, rotate="varimax", suppress=0.31)
summary(m_arg07m, rotate="varimax", suppress=0.31)

summary(m_bol07d, rotate="varimax", suppress=0.31)


#Brasil
summary(m_bra02d, rotate="varimax", suppress=0.31)
summary(m_bra02m, rotate="varimax", suppress=0.31)
summary(m_bra03d, rotate="varimax", suppress=0.31)
summary(m_bra03m, rotate="varimax", suppress=0.31)
summary(m_br7d, rotate="varimax", suppress=0.31)
summary(m_br7m, rotate="varimax", suppress=0.31)

#
summary(m_col05d,rotate="varimax", suppress=0.31)
summary(m_col06d,rotate="varimax", suppress=0.31)
summary(m_col07d,rotate="varimax", suppress=0.31)

# pra outros só fazer
load("D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/modelosde4.RData")#para mais testes

summary(m_equad06d,rotate="varimax", suppress=0.31)
summary(m_equ07d,rotate="varimax", suppress=0.31)


summary(M_GUAT05D,rotate="varimax", suppress=0.31)
summary(M_GUAT07D,rotate="varimax", suppress=0.31)

summary(M_HAITI06D,rotate="varimax", suppress=0.31)

summary(m_nicaragua07d,rotate="varimax", suppress=0.31)


summary(m_chi05d, rotate="varimax", suppress=0.31)
summary(m_ch7d, rotate="varimax", suppress=0.31)


summary(m_mex05d, rotate="varimax", suppress=0.31)
summary(m_mex06d, rotate="varimax", suppress=0.31)
summary(m_mex06m, rotate="varimax", suppress=0.31)


summary(m_peru04m, rotate="varimax", suppress=0.31)
summary(m_peru05d, rotate="varimax", suppress=0.31)
summary(m_peru07d, rotate="varimax", suppress=0.31)
summary(m_peru07m, rotate="varimax", suppress=0.31)


summary(m_PortoRico07d, rotate="varimax", suppress=0.31)
summary(m_PortoRico07m, rotate="varimax", suppress=0.31)


