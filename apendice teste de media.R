#rodar parte 1 e 2 anos
summary(scores_lista)
# Reorganize a ordem dos países dentro de scores_lista
scores_lista <- scores_lista[c("Venezuela", "Uruguai", "República Dominicana", "Peru", "Paraguai", "Panamá", "Nicarágua", "Honduras","Guatemala", "El Salvador", "Equador", "Costa Rica", "Colômbia", "Chile", "Brasil", "Bolívia", "Argentina")]
scores_lista <- rev(scores_lista)
summary(scores_lista)
scores_lista -> scores_lista_backup
scores_lista <- lapply(scores_lista, function(x) {
  x <- (x - min(x)) / (max(x) - min(x))
  return(x)
})


scores_lista$Argentina[,1]

library(forcats)




#normalizar entre 0 e 1


# Crie um data frame com as médias e intervalos de confiança de 90% para DesconfiançaInst
summary_df <- data.frame(País = names(scores_lista))
summary_df$media<- sapply(scores_lista, function(x) mean(x[,"DesconfiançaInst"]))
summary_df$Lower <- sapply(scores_lista, function(x) quantile(x[,"DesconfiançaInst"], 0.10))
summary_df$Upper <- sapply(scores_lista, function(x) quantile(x[,"DesconfiançaInst"], 0.90))
# Reordena a variável "País" com base na ordem alfabética inversa
summary_df <- summary_df %>% 
  mutate(País = fct_rev(País))

# Plote o gráfico com a ordem personalizada
ggplot(summary_df, aes(x = País, y = media)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.6, color = "black") +
  geom_point(color = "red", size = 3) +
  labs(title = "Comparação de Contrário ao Casamento Gay por País",
       x = "País", y = "Contrário ao Casamento Gay") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
summary_df$País
# Crie o gráfico de barras com barras de erro
library(ggplot2)
a<-ggplot(summary_df, aes(x = País, y = media)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.6, color = "black") +
  labs(title = "",
       x = "País", y = "Desconfiança Institucional") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+coord_flip()+
  geom_point(color = "black", size = 3)
a


#
summary_df <- data.frame(País = names(scores_lista))
summary_df$media<- sapply(scores_lista, function(x) mean(x[,"QuestõesEconômicas"]))
summary_df$Lower <- sapply(scores_lista, function(x) quantile(x[,"QuestõesEconômicas"], 0.10))
summary_df$Upper <- sapply(scores_lista, function(x) quantile(x[,"QuestõesEconômicas"], 0.90))
# Reordena a variável "País" com base na ordem alfabética inversa
summary_df <- summary_df %>% 
  mutate(País = fct_rev(País))

# Plote o gráfico com a ordem personalizada
ggplot(summary_df, aes(x = País, y = media)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.6, color = "black") +
  geom_point(color = "red", size = 3) +
  labs(title = "Comparação de Contrário ao Casamento Gay por País",
       x = "País", y = "Contrário ao Casamento Gay") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Crie o gráfico de barras com barras de erro
library(ggplot2)

b<-ggplot(summary_df, aes(x = País, y = media)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.6, color = "black") +
  labs(title = "",
       x = "País", y = "Contrariedade Direitos trabalhistas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+coord_flip()+
  geom_point(color = "black", size = 3)



b
summary_df <- data.frame(País = names(scores_lista))
summary_df$media<- sapply(scores_lista, function(x) mean(x[,"CasGay"]))
summary_df$Lower <- sapply(scores_lista, function(x) quantile(x[,"CasGay"], 0.10))
summary_df$Upper <- sapply(scores_lista, function(x) quantile(x[,"CasGay"], 0.90))
# Reordena a variável "País" com base na ordem alfabética inversa
summary_df <- summary_df %>% 
  mutate(País = fct_rev(País))

# Plote o gráfico com a ordem personalizada
ggplot(summary_df, aes(x = País, y = media)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.6, color = "black") +
  geom_point(color = "red", size = 3) +
  labs(title = "Comparação de Contrário ao Casamento Gay por País",
       x = "País", y = "Contrário ao Casamento Gay") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Crie o gráfico de barras com barras de erro
library(ggplot2)

c<-ggplot(summary_df, aes(x = País, y = media)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.6, color = "black") +
  labs(title = "",
       x = "País", y = "Contrariedade ao Casamento Gay") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+coord_flip()+
  geom_point(color = "black", size = 3)
c

grid.arrange(a,b,c,ncol=2)
