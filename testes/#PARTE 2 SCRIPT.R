#obs rodar parte 1 antes
setwd("C:/Users/grego/OneDrive/Desktop/work/tese/escrevinhando o cap 2/latinoBar_2015_resultados afc apendice cap 2 tese")
Argentina %>% glimpse()#pra ver se deu certo

library(lavaan)

library(semTools)
library(psych)
#scree
scree(Argentina, main="Argentina")
scree(Bolivia)
scree(Brasil)
scree(Chile)
scree(Colomb)
scree(CostaRica)
scree(Ecuador)
scree(elSalv)
scree(Guat)
scree(Hondu)
#scree(Mexico)
scree(Nic)
scree(Panama)
scree(Paraguai)
scree(RepDom)
scree(Uru)
scree(Ven)
rm(df, Mexico)
# Defina o modelo AFC padrão com os parâmetros fixos
modelo_afc <- '
  # Modelo AFC padrão
  DesconfiançaInst =~ P16TGB.B + P19ST.C + P16ST.F + P19N.H
  QuestõesEconômicas =~ P46N.C
  CasGay =~ P69ST.C
'
Argentina_fit <- cfa(modelo_afc, data = Argentina)
summary(Argentina_fit, standardized = TRUE)
semTools::fitmeasures(Argentina_fit, c("tli", "cfi", "rmsea", "srmr"))

Bolivia_fit <- cfa(modelo_afc, data = Bolivia)
summary(Bolivia_fit, standardized = TRUE)
semTools::fitmeasures(Bolivia_fit, c("tli", "cfi", "rmsea", "srmr"))

Brasil_fit <- cfa(modelo_afc, data = Brasil)
summary(Brasil_fit, standardized = TRUE)
semTools::fitmeasures(Brasil_fit, c("tli", "cfi", "rmsea", "srmr"))

Chile_fit <- cfa(modelo_afc, data = Chile)
summary(Chile_fit, standardized = TRUE)
semTools::fitmeasures(Chile_fit, c("tli", "cfi", "rmsea", "srmr"))

Colomb_fit <- cfa(modelo_afc, data = Colomb)
summary(Colomb_fit, standardized = TRUE)
semTools::fitmeasures(Colomb_fit, c("tli", "cfi", "rmsea", "srmr"))

CostaRica_fit <- cfa(modelo_afc, data = CostaRica)
summary(CostaRica_fit, standardized = TRUE)
semTools::fitmeasures(CostaRica_fit, c("tli", "cfi", "rmsea", "srmr"))

Ecuador_fit <- cfa(modelo_afc, data = Ecuador)
summary(Ecuador_fit, standardized = TRUE)
semTools::fitmeasures(Ecuador_fit, c("tli", "cfi", "rmsea", "srmr"))

elSalv_fit <- cfa(modelo_afc, data = elSalv)
summary(elSalv_fit, standardized = TRUE)
semTools::fitmeasures(elSalv_fit, c("tli", "cfi", "rmsea", "srmr"))

Guat_fit <- cfa(modelo_afc, data = Guat)
summary(Guat_fit, standardized = TRUE)
semTools::fitmeasures(Guat_fit, c("tli", "cfi", "rmsea", "srmr"))

Hondu_fit <- cfa(modelo_afc, data = Hondu)
summary(Hondu_fit, standardized = TRUE)
semTools::fitmeasures(Hondu_fit, c("tli", "cfi", "rmsea", "srmr"))

#Mexico %>% glimpse()#nao rolou
#Mexico_fit <- cfa(modelo_afc, data = Mexico)
#summary(Mexico_fit, standardized = TRUE)
#semTools::fitmeasures(Mexico_fit, c("tli", "cfi", "rmsea", "srmr"))

Nic_fit <- cfa(modelo_afc, data = Nic)
summary(Nic_fit, standardized = TRUE)
semTools::fitmeasures(Nic_fit, c("tli", "cfi", "rmsea", "srmr"))


Panama_fit <- cfa(modelo_afc, data = Panama)
summary(Panama_fit, standardized = TRUE)
semTools::fitmeasures(Panama_fit, c("tli", "cfi", "rmsea", "srmr"))

Paraguai_fit <- cfa(modelo_afc, data = Paraguai)
summary(Paraguai_fit, standardized = TRUE)
semTools::fitmeasures(Paraguai_fit, c("tli", "cfi", "rmsea", "srmr"))

Peru_fit <- cfa(modelo_afc, data = Peru)
summary(Peru_fit, standardized = TRUE)
semTools::fitmeasures(Peru_fit, c("tli", "cfi", "rmsea", "srmr"))

RepDom_fit <- cfa(modelo_afc, data = RepDom)
summary(RepDom_fit, standardized = TRUE)
semTools::fitmeasures(RepDom_fit, c("tli", "cfi", "rmsea", "srmr"))

Uru_fit <- cfa(modelo_afc, data = Uru)
summary(Uru_fit, standardized = TRUE)
semTools::fitmeasures(Uru_fit, c("tli", "cfi", "rmsea", "srmr"))

Ven_fit <- cfa(modelo_afc, data = Ven)
summary(Ven_fit, standardized = TRUE)
semTools::fitmeasures(Ven_fit, c("tli", "cfi", "rmsea", "srmr"))

# Vetores com os nomes dos países
paises <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colomb", "CostaRica",
            "Ecuador", "elSalv", "Hondu", "Nic", "Panama", "Paraguai",
            "Peru", "RepDom", "Uru", "Ven", "Guat")

# Lista para armazenar os resultados
resultados_lista <- list()

# Loop para calcular e armazenar as medidas de ajuste para cada país
for (pais in paises) {
  # Gere o nome do objeto do modelo
  fit_name <- paste0(pais, "_fit")
  
  # Calcule as medidas de ajuste
  medidas_ajuste <- semTools::fitmeasures(get(fit_name), c("tli", "cfi", "rmsea", "srmr"))
  
  # Acrescente o nome do país
  medidas_ajuste$pais <- pais
  
  # Armazene o resultado na lista
  resultados_lista[[pais]] <- medidas_ajuste
}

# Combine os resultados em um único data frame
resultados <- do.call(rbind, resultados_lista)

# Exiba o data frame final com as medidas de ajuste para todos os países
print(resultados)

# Crie uma matriz com os resultados
# Resultados 
resultados <- data.frame(
  tli = c(0.8471969, 0.9125331, 0.94328, 0.7926857, 0.9473427, 0.9213811, 0.916457, 0.9665753, 0.9628882, 0.9608239, 0.9813061, 0.9209027, 0.8363056, 0.965582, 0.9439844, 0.9516343, 0.9777769),
  cfi = c(0.918505, 0.953351, 0.9697494, 0.8894324, 0.9719161, 0.9580699, 0.9554438, 0.9821735, 0.980207, 0.9791061, 0.9900299, 0.9578148, 0.9126963, 0.9816437, 0.970125, 0.9742049, 0.9881477),
  rmsea = c(0.0832218, 0.0563187, 0.04719367, 0.08729538, 0.04361727, 0.04801994, 0.0819337, 0.04369117, 0.03974138, 0.05356074, 0.02622781, 0.06576903, 0.07391742, 0.03986411, 0.05085758, 0.06030796, 0.03520739),
  srmr = c(0.04650998, 0.03122022, 0.02666689, 0.04924233, 0.02486933, 0.02924261, 0.03378357, 0.0227484, 0.02246619, 0.02316179, 0.0218113, 0.03019721, 0.03716519, 0.02361297, 0.0329118, 0.02672648, 0.02239598),
  pais = c("Argentina", "Bolivia", "Brasil", "Chile", "Colomb", "CostaRica", "Ecuador", "elSalv", "Hondu", "Nic", "Panama", "Paraguai", "Peru", "RepDom", "Uru", "Ven", "Guat")
)

# Formate o nome dos países
resultados$pais <- gsub("Argentina", "Argentina", resultados$pais)
resultados$pais <- gsub("Bolivia", "Bolívia", resultados$pais)
resultados$pais <- gsub("Brasil", "Brasil", resultados$pais)
resultados$pais <- gsub("Chile", "Chile", resultados$pais)
resultados$pais <- gsub("Colomb", "Colômbia", resultados$pais)
resultados$pais <- gsub("CostaRica", "Costa Rica", resultados$pais)
resultados$pais <- gsub("Ecuador", "Equador", resultados$pais)
resultados$pais <- gsub("elSalv", "El Salvador", resultados$pais)
resultados$pais <- gsub("Hondu", "Honduras", resultados$pais)
resultados$pais <- gsub("Nic", "Nicarágua", resultados$pais)
resultados$pais <- gsub("Panama", "Panamá", resultados$pais)
resultados$pais <- gsub("Paraguai", "Paraguai", resultados$pais)
resultados$pais <- gsub("Peru", "Peru", resultados$pais)
resultados$pais <- gsub("RepDom", "República Dominicana", resultados$pais)
resultados$pais <- gsub("Uru", "Uruguai", resultados$pais)
resultados$pais <- gsub("Ven", "Venezuela", resultados$pais)
resultados$pais <- gsub("Guat", "Guatemala", resultados$pais)

# Crie um data frame com os resultados
resultados_df <- as.data.frame(resultados)

# Imprima o data frame formatado
print(resultados_df)#terminar no EXcel, adicionar observação sobre méxico lá
#write.csv(resultados, "C:/Users/grego/OneDrive/Desktop/work/tese/escrevinhando o cap 2/latinoBar_2015_resultados afc apendice cap 2 tese/dados_resultados.csv", row.names = FALSE)


#plotar
#histogramas scores
# Crie um vetor com os nomes dos países
paises <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colomb", "CostaRica",
            "Ecuador", "elSalv", "Hondu", "Nic", "Panama", "Paraguai",
            "Peru", "RepDom", "Uru", "Ven", "Guat")

# Crie uma lista para armazenar os scores de cada modelo
scores_lista <- list()

# Loop através de cada país e seu respectivo modelo
for (pais in paises) {
  # Use a função lavPredict para gerar scores
  scores <- lavPredict(get(paste0(pais, "_fit")))
  
  # Armazene os scores na lista
  scores_lista[[pais]] <- scores
}

# Agora, scores_lista contém os scores para cada modelo
head(scores_lista$Argentina)
head(scores_lista$Brasil[[1]])
names(scores_lista)

# Defina um vetor com os nomes corrigidos em português
nomes_corrigidos <- c("Argentina", "Bolívia", "Brasil", "Chile", "Colômbia", "Costa Rica", "Equador", "El Salvador", "Honduras", "Nicarágua", "Panamá", "Paraguai", "Peru", "República Dominicana", "Uruguai", "Venezuela", "Guatemala")

# Atribua os nomes corrigidos à lista scores_lista
names(scores_lista) <- nomes_corrigidos


# arg
nome_pais <- "Argentina"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_arg <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

names(scores_lista)
nome_pais <- "Bolívia"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_bol <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
desconfia_bol
nome_pais <- "Brasil"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_bra <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Chile"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_chi <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Colômbia"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_col <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Costa Rica"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_crc <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Equador"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_equ <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "El Salvador"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_el <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Honduras"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_hond <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Nicarágua"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_nic <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Panamá"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_pan <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Paraguai"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_parag <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Peru"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_peru <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "República Dominicana"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_dom <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Uruguai"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_uru <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Venezuela"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_ven <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Guatemala"
# Extraia os scores de desconfiança institucional para o país
scores_desconfianca <- scores_lista[[nome_pais]][, "DesconfiançaInst"]
# Crie o gráfico de distribuição
desconfia_guat <- ggplot(data = data.frame(DesconfiançaInst = scores_desconfianca), aes(x = DesconfiançaInst)) +
  geom_histogram(binwidth = 0.022, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Desconfiança
       Institucional ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())


desconfia_guat
nome_pais <- "Argentina"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
CasGay_arg <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
CasGay_arg
names(scores_lista)
nome_pais <- "Bolívia"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_bol <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
CasGay_bol
nome_pais <- "Brasil"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_bra <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Chile"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_chi <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade 
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Colômbia"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_col <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Costa Rica"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_crc <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Equador"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_equ <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "El Salvador"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_el <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Honduras"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_hond <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Nicarágua"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_nic <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Panamá"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_pan <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Paraguai"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_parag <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Peru"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_peru <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "República Dominicana"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_dom <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Uruguai"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_uru <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Venezuela"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_ven <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Guatemala"
score_CasGay_ <- scores_lista[[nome_pais]][, "CasGay"]
# Crie o gráfico de distribuição
CasGay_guat <- ggplot(data = data.frame(CasGay = score_CasGay_), aes(x = CasGay)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contriedade
       ao Casamento Gay ->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

CasGay_guat

nome_pais <- "Argentina"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
economia_arg <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

names(scores_lista)
nome_pais <- "Bolívia"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_bol <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
economia_bol
nome_pais <- "Brasil"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_bra <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Chile"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_chi <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Colômbia"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_col <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Costa Rica"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_crc <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "Equador"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_equ <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
nome_pais <- "El Salvador"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_el <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Honduras"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_hond <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Nicarágua"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_nic <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Panamá"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_pan <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Paraguai"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_parag <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Peru"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_peru <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "República Dominicana"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_dom <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())

nome_pais <- "Uruguai"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_uru <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())+
  theme(axis.text.y = element_blank())

nome_pais <- "Venezuela"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_ven <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->>", y = "")+theme_bw()+   theme(axis.text.y = element_blank())
economia_ven
nome_pais <- "Guatemala"
scores_economia <- scores_lista[[nome_pais]][, "QuestõesEconômicas"]
# Crie o gráfico de distribuição
economia_guat <- ggplot(data = data.frame(QuestõesEconômicas = scores_economia), aes(x = QuestõesEconômicas)) +
  geom_histogram(binwidth = 0.12, fill = "blue", color = "black") +
  labs(subtitle = paste("", nome_pais),
       x = "Contrário
       direitos trab.->", y = "")+theme_bw()+   theme(axis.text.y = element_blank())



economia_guat
CasGay_guat
desconfia_guat
library(gridExtra)
grid.arrange(CasGay_arg,CasGay_bol,CasGay_bra,CasGay_chi,CasGay_col,
             CasGay_crc,CasGay_dom, CasGay_el,CasGay_equ, CasGay_guat,
             CasGay_hond, CasGay_nic, CasGay_pan, CasGay_parag,
             CasGay_peru, CasGay_uru, CasGay_ven)
grid.arrange(desconfia_arg,desconfia_bol,desconfia_bra,desconfia_chi,desconfia_col,
             desconfia_crc,desconfia_dom, desconfia_el,desconfia_equ, desconfia_guat,
             desconfia_hond, desconfia_nic, desconfia_pan, desconfia_parag,
             desconfia_peru, desconfia_uru, desconfia_ven)

grid.arrange(economia_arg,economia_bol,economia_bra,economia_chi,economia_col,
             economia_crc,economia_dom, economia_el,economia_equ, economia_guat,
             economia_hond, economia_nic, economia_pan, economia_parag,
             economia_peru, economia_uru, economia_ven)

