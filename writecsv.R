#Write.csv criar bases de cada paíse e ano. Se basear em Tabelas.xls cap 2
setwd("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7")
library(tidyverse)
df <- read_csv("WVS_TimeSeries_4_0.csv")
df <- subset(df, select=c(S002VS,
                          S003,
                          B008,
                          C001,                     
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
                          G006,
                          E069_07,
                          E069_06,
                          E069_12,
                          E069_17))
#paises
selecao <- df$S003 == 32
arg <- df[selecao, ]
selecao <- df$S003 == 68
bolivia <- df[selecao, ]
selecao <- df$S003 == 76
brasil <- df[selecao, ]
selecao <- df$S003 == 152
chile <- df[selecao, ]
selecao <- df$S003 == 170
colomb <- df[selecao, ]
selecao <- df$S003 == 214
repdom <- df[selecao, ]
selecao <- df$S003 == 218
equad <- df[selecao, ]
selecao <- df$S003 == 222
elSalvador <- df[selecao, ]
selecao <- df$S003 == 320
guat <- df[selecao, ]
selecao <- df$S003 == 484
mexico <- df[selecao, ]
selecao <- df$S003 == 558
nic <- df[selecao, ]
selecao <- df$S003 == 604
peru <- df[selecao, ]
selecao <- df$S003 == 858
uru <- df[selecao, ]
selecao <- df$S003 == 862
ven <- df[selecao, ]
selecao <- df$S003 == 630
portoRico <- df[selecao, ]
selecao <- df$S003 == 332
haiti <- df[selecao, ]


#ondas sepas
selecao <- arg$S002VS == 2
arg2 <- arg[selecao, ]
selecao <- arg$S002VS == 3
arg3 <- arg[selecao, ]
selecao <- arg$S002VS == 4
arg4 <- arg[selecao, ]
selecao <- arg$S002VS == 5
arg5 <- arg[selecao, ]
selecao <- arg$S002VS == 6
arg6 <- arg[selecao, ]
selecao <- arg$S002VS == 7
arg7 <- arg[selecao, ]
selecao <- bolivia$S002VS == 7
bolivia7 <- bolivia[selecao, ]
selecao <- brasil$S002VS == 2
brasil2 <- brasil[selecao, ]
selecao <- brasil$S002VS == 3
brasil3 <- brasil[selecao, ]
selecao <- brasil$S002VS == 5
brasil5 <- brasil[selecao, ]
selecao <- brasil$S002VS == 6
brasil6 <- brasil[selecao, ]
selecao <- brasil$S002VS == 7
brasil7 <- brasil[selecao, ]
selecao <- chile$S002VS == 2
chile2 <- chile[selecao, ]
selecao <- chile$S002VS == 3
chile3 <- chile[selecao, ]
selecao <- chile$S002VS == 4
chile4 <- chile[selecao, ]
selecao <- chile$S002VS == 5
chile5 <- chile[selecao, ]
selecao <- chile$S002VS == 6
chile6 <- chile[selecao, ]
selecao <- chile$S002VS == 7
chile7 <- chile[selecao, ]
selecao <- colomb$S002VS == 7
colomb7 <- colomb[selecao, ]
selecao <- haiti$S002VS == 6
haiti6 <- haiti[selecao, ]
selecao <- elSalvador$S002VS == 3
elSalvador3 <- elSalvador[selecao, ]
selecao <- equad$S002VS == 6
equad6 <- equad[selecao, ]
selecao <- equad$S002VS == 7
equad7 <- equad[selecao, ]
selecao <- guat$S002VS == 5
guat5 <- guat[selecao, ]
selecao <- guat$S002VS == 7
guat7 <- guat[selecao, ]
selecao <- mexico$S002VS == 2
mexico2 <- mexico[selecao, ]
selecao <- mexico$S002VS == 3
mexico3 <- mexico[selecao, ]
selecao <- mexico$S002VS == 4
mexico4 <- mexico[selecao, ]
selecao <- mexico$S002VS == 5
mexico5 <- mexico[selecao, ]
selecao <- mexico$S002VS == 6
mexico6 <- mexico[selecao, ]
selecao <- mexico$S002VS == 7
mexico7 <- mexico[selecao, ]
selecao <- nic$S002VS == 7
nic7 <- nic[selecao, ]
selecao <- peru$S002VS == 3
peru3 <- peru[selecao, ]
selecao <- peru$S002VS == 4
peru4 <- peru[selecao, ]
selecao <- peru$S002VS == 6
peru6 <- peru[selecao, ]
selecao <- peru$S002VS == 7
peru7 <- peru[selecao, ]
selecao <- portoRico$S002VS == 3
portoRico3 <- portoRico[selecao, ]
selecao <- portoRico$S002VS == 4
portoRico4 <- portoRico[selecao, ]
selecao <- portoRico$S002VS == 7
portoRico7 <- portoRico[selecao, ]
selecao <- repdom$S002VS == 3
repdom3 <- repdom[selecao, ]
selecao <- uru$S002VS == 3
uru3 <- uru[selecao, ]
selecao <- uru$S002VS == 5
uru5 <- uru[selecao, ]
selecao <- uru$S002VS == 6
uru6 <- uru[selecao, ]
selecao <- uru$S002VS == 7
uru7 <- uru[selecao, ]
selecao <- ven$S002VS == 3
ven3 <- ven[selecao, ]
selecao <- ven$S002VS == 4
ven4 <- ven[selecao, ]
selecao <- ven$S002VS == 7
ven7 <- ven[selecao, ]




write.csv(arg2,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/arg2.csv")



write.csv(arg3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/arg3.csv")

write.csv(arg4,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/arg4.csv")


write.csv(arg5,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/arg5.csv")


write.csv(arg6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/arg6.csv")


write.csv(arg7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/arg7.csv")


write.csv(bolivia7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/bolivia7.csv")


write.csv(brasil2,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/brasil2.csv")


write.csv(brasil3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/brasil3.csv")

write.csv(brasil5,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/brasil5.csv")

write.csv(brasil6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/brasil6.csv")

write.csv(brasil7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/brasil7.csv")


write.csv(chile2,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/chile2.csv")

write.csv(chile4,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/chile4.csv")

write.csv(chile3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/chile3.csv")

write.csv(chile5,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/chile5.csv")

write.csv(chile6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/chile6.csv")

write.csv(chile7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/chile7.csv")


write.csv(colomb7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/colomb7.csv")

write.csv(elSalvador3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/elSalvador3.csv")

write.csv(equad6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/equad6.csv")

write.csv(equad7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/equad7.csv")

write.csv(guat5,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/guat5.csv")

write.csv(guat7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/guat7.csv")

write.csv(haiti6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/haiti6.csv")

write.csv(mexico2,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/mexico2.csv")

write.csv(mexico3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/mexico3.csv")

write.csv(mexico4,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/mexico4.csv")

write.csv(mexico5,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/mexico5.csv")

write.csv(mexico6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/mexico6.csv")

write.csv(mexico7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/mexico7.csv")

write.csv(nic7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/nic7.csv")


write.csv(peru3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/peru3.csv")


write.csv(peru4,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/peru4.csv")


write.csv(peru6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/peru6.csv")

write.csv(peru7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/peru7.csv")


write.csv(portoRico3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/portoRico3.csv")



write.csv(portoRico4,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/portoRico4.csv")


write.csv(portoRico7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/portoRico7.csv")

write.csv(repdom3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/repdom3.csv")

write.csv(uru3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/uru3.csv")

write.csv(uru5,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/uru5.csv")

write.csv(uru6,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/uru6.csv")

write.csv(uru7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/uru7.csv")

write.csv(ven3,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/ven3.csv")

write.csv(ven4,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/ven4.csv")

write.csv(ven7,
          "C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/WVS/WVS full secaum atualizado_ onda 1 a onda 7/Tabela 3 _ Capitulo 2 tese/ven7.csv")

