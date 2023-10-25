#teste regressão
#rodar depois de apendice teste de media
head(scores_lista$Argentina)
summary(scores_lista)

# Crie um vetor com todas as observações de DesconfiançaInst
desconfianca <- c(
  scores_lista$Argentina[, "DesconfiançaInst"],
  scores_lista$Bolívia[, "DesconfiançaInst"],
  scores_lista$Brasil[, "DesconfiançaInst"],
  scores_lista$Chile[, "DesconfiançaInst"],
  scores_lista$Colômbia[, "DesconfiançaInst"],
  scores_lista$`Costa Rica`[, "DesconfiançaInst"],
  scores_lista$Equador[, "DesconfiançaInst"],
  scores_lista$`El Salvador`[, "DesconfiançaInst"],
  scores_lista$Guatemala[, "DesconfiançaInst"],
  scores_lista$Honduras[, "DesconfiançaInst"],
  scores_lista$Nicarágua[, "DesconfiançaInst"],
  scores_lista$Panamá[, "DesconfiançaInst"],
  scores_lista$Paraguai[, "DesconfiançaInst"],
  scores_lista$Peru[, "DesconfiançaInst"],
  scores_lista$`República Dominicana`[, "DesconfiançaInst"],
  scores_lista$Uruguai[, "DesconfiançaInst"],
  scores_lista$Venezuela[, "DesconfiançaInst"]
)

# Crie um vetor com o nome do país correspondente para cada observação
paises <- rep(
  c(
    "Argentina", "Bolívia", "Brasil", "Chile", "Colômbia", "Costa Rica",
    "Equador", "El Salvador", "Guatemala", "Honduras", "Nicarágua",
    "Panamá", "Paraguai", "Peru", "República Dominicana", "Uruguai", "Venezuela"
  ),
  sapply(scores_lista, nrow)  # Isso conta o número de observações por país
)

# Crie o dataframe final
df <- data.frame(País = paises, DesconfiançaInst = desconfianca)



# Crie o dataframe final
df <- data.frame(País = paises, DesconfiançaInst = desconfianca)

df %>% glimpse()
table(df$País)
df$País <- as.factor(df$País)
by(df$DesconfiançaInst, df$País,mean)
table(df$País)
# Crie um novo fator com a ordem desejada com base nas médias
df$País <- factor(df$País, levels = c("Equador", "Venezuela","El Salvador","Guatemala", "Honduras","Panamá","Colômbia","Paraguai", "Costa Rica","República Dominicana", "Bolívia", "Brasil", "Peru","Nicarágua", "Argentina", "Chile", "Uruguai"))

# Ordene o dataframe com base no novo fator
df <- df[order(df$País), ]

# Exiba o dataframe ordenado
levels(df$País)


model <- lm(DesconfiançaInst~País, data=df)
summary(model)

# Ordene o dataframe em ordem decrescente dos valores preditos


# Crie o gráfico com os valores ordenados
library(marginaleffects)
plot_cap(model, data = df, condition = "País", conf_level = 0.9)
library(marginaleffects)
a<-plot_cap(model, condition="País",conf_level = .9)

a + coord_flip()

library(easystats)
library(tidyverse)  # dplyr, ggplot2, and friends
library(scales)     # Functions to format things nicely

a + labs(y="Desconfiança Institucional")+
  theme_lucid()+
  scale_x_discrete(labels = label_wrap(10))+coord_flip()+theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))


