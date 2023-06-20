
# Códigos usados no Google colab
# No colab tem os resultados para cada um respectivamente

library(ggplot2)
library(dplyr)
library(scales)
base_parte_0 <- read.csv2("DFp1.csv", sep = ";", quote = "")
base_parte_5 <- read.csv2("DFp5.csv", sep = ";", quote = "")
# juntando os arquivos
df_baseCovid = rbind(base_parte_0, base_parte_5)
nrow(df_baseCovid)
head(df_baseCovid, 1)
colnames(df_baseCovid)
# Separando colunas
df_analisedescritiva <- subset(df_baseCovid, select = c(X.paciente_idade., X.paciente_enumSexoBiologico., X.paciente_racaCor_codigo., X.paciente_racaCor_valor., X.vacina_fabricante_nome., X.vacina_dataAplicacao., X.vacina_descricao_dose.))
head(df_analisedescritiva, 1)
df_analisedescritiva <- df_analisedescritiva %>%
  mutate_all(~ gsub("\"", "", .))
df_analisedescritiva <- df_analisedescritiva %>%
  mutate(X.paciente_idade. = as.integer(X.paciente_idade.))
df_analisedescritiva <- df_analisedescritiva %>%
  mutate(X.paciente_racaCor_codigo. = as.integer(X.paciente_racaCor_codigo.))
df_analisedescritiva <- df_analisedescritiva %>%
  mutate(X.vacina_dataAplicacao. = as.Date(X.vacina_dataAplicacao.))
df_analisedescritiva <- df_analisedescritiva %>%
  rename_all(~ gsub("^X\\.|\\.", "", .))
df_analisedescritiva <- df_analisedescritiva %>%
  rename_all(~ gsub("^paciente_|^vacina_", "", .))
head(df_analisedescritiva, 1)
df_analisedescritiva <- df_analisedescritiva %>%
  mutate(grupo_idade = case_when(
    idade <= 4                      ~ "0-4 anos",
    idade >= 5  & idade <= 9  ~ "5-9 anos",
    idade >= 10 & idade <= 14 ~ "10-14 anos",
    idade >= 15 & idade <= 19 ~ "15-19 anos",
    idade >= 20 & idade <= 24 ~ "20-24 anos",
    idade >= 25 & idade <= 29 ~ "25-29 anos",
    idade >= 30 & idade <= 34 ~ "30-34 anos",
    idade >= 35 & idade <= 39 ~ "35-39 anos",
    idade >= 40 & idade <= 44 ~ "40-44 anos",
    idade >= 45 & idade <= 49 ~ "45-49 anos",
    idade >= 50 & idade <= 54 ~ "50-54 anos",
    idade >= 55 & idade <= 59 ~ "55-59 anos",
    idade >= 60 & idade <= 64 ~ "60-64 anos",
    idade >= 65 & idade <= 69 ~ "65-69 anos",
    idade >= 70 & idade <= 74 ~ "70-74 anos",
    idade >= 75 & idade <= 79 ~ "75-79 anos",
    idade >= 80                      ~ "80 mais",
    TRUE                                      ~ NA_character_
  ))

ordem_grupo_idade <- c("0-4 anos", "5-9 anos", "10-14 anos", "15-19 anos", "20-24 anos", "25-29 anos", "30-34 anos", "35-39 anos", "40-44 anos", "45-49 anos", "50-54 anos", "55-59 anos", "60-64 anos", "65-69 anos", "70-74 anos", "75-79 anos", "80 mais")
df_analisedescritiva$grupo_idade <- factor(df_analisedescritiva$grupo_idade, levels = ordem_grupo_idade)

head(df_analisedescritiva, 1)

# Gráfico de barras da distribuição pelo grupo de idade
ggplot(df_analisedescritiva, aes(x = grupo_idade)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "Grupo de Idade", y = "Frequência", title = "Distribuição pelo Grupo de Idade") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub(" anos", "", x)) +
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
# Definindo as faixas etárias
faixa_etaria <- cut(df_analisedescritiva$idade,
                    breaks = c(0, 18, 65, Inf),
                    labels = c("Crianças", "Adultos", "Idosos"),
                    include.lowest = TRUE)
# Contagem das faixas etárias
contagem_faixa_etaria <- table(faixa_etaria)
# Definindo as cores corretas
cores <- c("Crianças" = "#FFD700",
           "Adultos" = "#8B4513",
           "Idosos" = "#808080")
# Definindo a ordem das categorias
ordem_categorias <- c("Crianças", "Adultos", "Idosos")
# Criando o gráfico de barras
grafico_barras <- ggplot(data.frame(contagem_faixa_etaria), aes(x = reorder(names(contagem_faixa_etaria), -Freq), y = Freq, fill = names(contagem_faixa_etaria))) +
  geom_bar(stat = "identity") +
  labs(x = "Faixa Etária", y = "Frequência") +
  scale_fill_manual(values = cores, breaks = ordem_categorias, labels = ordem_categorias) +
  theme_bw()
# Exibindo o gráfico
print(grafico_barras)

# Definindo as faixas etárias
faixa_etaria <- cut(df_analisedescritiva$idade,
                    breaks = c(0, 12, 18, 35, 50, Inf),
                    labels = c("Crianças", "Adolescentes", "Jovens Adultos", "Adultos", "Idosos"),
                    include.lowest = TRUE)

# Contagem das faixas etárias
contagem_faixa_etaria <- table(faixa_etaria)


# Definindo as cores corretas
cores <- c("Crianças" = "#98FB98",  # Verde mais neutro
           "Adolescentes" = "#4169E1",  # Azul mais escuro
           "Jovens Adultos" = "#00FFFF",  # Outra cor de exemplo
           "Adultos" = "#8B4513",  # Marrom
           "Idosos" = "#A9A9A9")  # Cinza

# Definindo a ordem das categorias
ordem_categorias <- c("Crianças", "Adolescentes", "Jovens Adultos", "Adultos", "Idosos")


# Criando o gráfico de barras
grafico_barras <- ggplot(data.frame(contagem_faixa_etaria), aes(x = factor(names(contagem_faixa_etaria), levels = ordem_categorias), y = Freq, fill = names(contagem_faixa_etaria))) +
  geom_bar(stat = "identity") +
  labs(x = "Faixa Etária", y = "Frequência") +
  scale_fill_manual(values = cores, breaks = ordem_categorias, labels = ordem_categorias) +
  theme_bw()


# Exibir o gráfico
print(grafico_barras)

# Filtrar valores não finitos na variável "idade"
df_filtrado <- df_analisedescritiva[is.finite(df_analisedescritiva$idade), ]

# Criar o gráfico de barras da distribuição das idades
grafico_idades <- ggplot(df_filtrado, aes(x = idade)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "Idade", y = "Frequência", title = "Distribuição das Idades") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(df_filtrado$idade), max(df_filtrado$idade), by = 5))

# Exibir o gráfico
print(grafico_idades)
# Filtrar apenas as categorias "Masculino" e "Feminino"
df_filtrado <- subset(df_analisedescritiva, enumSexoBiologico %in% c("M", "F"))
# Contagem dos sexos
contagem_sexo <- table(df_filtrado$enumSexoBiologico)
# Calculando as porcentagens
porcentagens <- prop.table(contagem_sexo) * 100
# Criando o gráfico de pizza
grafico_pizza <- ggplot(data.frame(contagem_sexo, porcentagens), aes(x = "", y = Freq, fill = names(contagem_sexo))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Sexo") +
  scale_fill_manual(values = c("M" = "blue", "F" = "hotpink"),
                    labels = c("M" = "Masculino", "F" = "Feminino")) +
  geom_text(aes(label = paste0(round(porcentagens, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void()
# Exibindo o gráfico
print(grafico_pizza)
# Obter todas as raças únicas
racas_unicas <- unique(df_analisedescritiva$racaCor_valor)

racas_unicas
# Criando um novo dataframe filtrado
df_filtrado <- df_analisedescritiva

# Agrupando os valores nulos/vazios no grupo "SEM INFORMACAO"
df_filtrado$racaCor_valor[is.na(df_filtrado$racaCor_valor) | df_filtrado$racaCor_valor == ""] <- "SEM INFORMACAO"

# Contagem das raças/cor
contagem_raca <- table(df_filtrado$racaCor_valor)

# Criando o gráfico de barras sem cores
grafico_barras <- ggplot(data.frame(contagem_raca), aes(x = reorder(names(contagem_raca), -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Raça/Cor", y = "Frequência") +
  theme_bw()

# Exibindo o gráfico
print(grafico_barras)

# Definindo as cores corretas
cores <- c("SEM INFORMACAO" = "#808080",
           "PARDA" = "#8B4513",
           "BRANCA" = "#FFC0CB",
           "PRETA" = "#000000",
           "AMARELA" = "#FFFF00",
           "INDIGENA" = "#FF0000")

# Definindo a ordem das categorias
ordem_categorias <- c("SEM INFORMACAO", "PARDA", "BRANCA", "PRETA", "AMARELA", "INDIGENA")

# Criando o gráfico de barras
grafico_barras_colorido <- ggplot(data.frame(contagem_raca), aes(x = reorder(names(contagem_raca), -Freq), y = Freq, fill = names(contagem_raca))) +
  geom_bar(stat = "identity") +
  labs(x = "Raça/Cor", y = "Frequência") +
  scale_fill_manual(values = cores, breaks = ordem_categorias, labels = ordem_categorias) +
  theme_bw()

# Exibindo o gráfico
print(grafico_barras_colorido)

# Criando o gráfico de pizza
grafico_pizza <- ggplot(data.frame(contagem_raca), aes(x = "", y = Freq, fill = names(contagem_raca))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Raça/Cor") +
  scale_fill_manual(values = cores, breaks = ordem_categorias, labels = ordem_categorias) +
  theme_void()

# Exibindo o gráfico
print(grafico_pizza)

unique(df_analisedescritiva$fabricante_nome)

# Substituir os valores no dataframe
df_filtrado <- df_analisedescritiva %>%
  mutate(fabricante_nome = case_when(
    fabricante_nome == "ASTRAZENECA/FIOCRUZ" ~ "ASTRAZENECA",
    fabricante_nome == "SINOVAC/BUTANTAN" ~ "SINOVAC",
    fabricante_nome == "JANSSEN PHARMACEUTICA NV" ~ "JANSSEN",
    fabricante_nome == "PFIZER - PEDIÁTRICA MENOR DE 5 ANOS" ~ "PFIZER",
    fabricante_nome == "PFIZER - PEDIÁTRICA" ~ "PFIZER",
    fabricante_nome == "PFIZER - PEDI?TRICA" ~ "PFIZER",
    fabricante_nome == "PFIZER MANUFACTURING BELGIUM NV - BELGICA" ~ "PFIZER",
    fabricante_nome == "Pendente Identifica??o" ~ "Sem Identificação",
    fabricante_nome == "Pendente Identificação" ~ "Sem Identificação",
    fabricante_nome == "" ~ "Sem Identificação",
    TRUE ~ fabricante_nome
  ))

unique(df_filtrado$fabricante_nome)

# Contagem dos fabricantes de vacina substituídos
contagem_fabricante <- as.data.frame(table(df_filtrado$fabricante_nome))
contagem_fabricante$Freq <- as.numeric(contagem_fabricante$Freq)

# Gráfico de barras da distribuição por fabricante de vacina
ggplot(contagem_fabricante, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(x = "Fabricante de Vacina", y = "Frequência", title = "Distribuição por Fabricante de Vacina") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Converter a coluna dataAplicacao para o formato Date, se necessário
df_analisedescritiva$dataAplicacao <- as.Date(df_analisedescritiva$dataAplicacao)

# Extrair o mês da coluna dataAplicacao
df_analisedescritiva$mes <- format(df_analisedescritiva$dataAplicacao, "%Y-%m")

# Contar a quantidade de vacinações por mês
contagem_mes <- table(df_analisedescritiva$mes)

# Converter os dados em um dataframe
dados_grafico <- data.frame(Mês = names(contagem_mes), Vacinações = as.numeric(contagem_mes))

# Criar o gráfico de barras
grafico_vacinacoes <- ggplot(dados_grafico, aes(x = Mês, y = Vacinações)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Mês", y = "Quantidade de Vacinações") +
  theme_bw() +
  scale_x_discrete(breaks = unique(dados_grafico$Mês)[c(TRUE, FALSE, FALSE, FALSE)]) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Exibir o gráfico
print(grafico_vacinacoes)

# Contar a quantidade de vacinações por semana
contagem_semana <- table(format(df_analisedescritiva$dataAplicacao, "%Y-%U"))

# Converter os dados em um dataframe
dados_grafico <- data.frame(Semana = names(contagem_semana), Vacinacoes = as.numeric(contagem_semana))

# Ordenar as semanas de forma correta
dados_grafico$Semana <- factor(dados_grafico$Semana, levels = unique(dados_grafico$Semana))

# Criar o gráfico de barras
grafico_vacinacoes <- ggplot(dados_grafico, aes(x = Semana, y = Vacinacoes)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Semana", y = "Quantidade de Vacinações") +
  theme_bw() +
  scale_x_discrete(breaks = unique(dados_grafico$Semana)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)]) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Exibir o gráfico
print(grafico_vacinacoes)

library(IRdisplay)

# Filtrar valores "I" (Indefinido) na coluna enumSexoBiologico
df_filtrado <- df_analisedescritiva %>%
  filter(enumSexoBiologico %in% c("M", "F"))

# Agrupar os dados por mês e sexo, e calcular a contagem de vacinações
dados_grafico <- df_filtrado %>%
  group_by(mes, enumSexoBiologico) %>%
  summarise(Vacinacoes = n())

# Reordenar os meses
dados_grafico$mes <- factor(dados_grafico$mes, levels = unique(dados_grafico$mes))

# Definir as cores para os sexos
cores <- c("M" = "#1F77B4", "F" = "#FF69B4")

# Definir os nomes dos meses
nomes_meses <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")

# Criar o gráfico de barras
grafico_barras <- ggplot(dados_grafico, aes(x = mes, y = Vacinacoes, fill = enumSexoBiologico)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "Mês", y = "Quantidade de Vacinações", fill = "Sexo") +
  scale_fill_manual(values = cores, labels = c("Feminino", "Masculino")) +
  theme_bw() +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = nomes_meses)

# Exibir o gráfico
print(grafico_barras)

# Converter a coluna "dataAplicacao" para o formato de data
df_analisedescritiva$dataAplicacao <- as.Date(df_analisedescritiva$dataAplicacao)

# Extrair o número da semana a partir da coluna "dataAplicacao"
df_analisedescritiva$semana <- week(df_analisedescritiva$dataAplicacao)

# Filtrar os valores "I" (Indefinido) na coluna "enumSexoBiologico"
df_filtrado <- df_analisedescritiva %>%
  filter(enumSexoBiologico %in% c("M", "F"))

# Agrupar os dados por semana e sexo, e calcular a contagem de vacinações
dados_grafico <- df_filtrado %>%
  group_by(semana, enumSexoBiologico) %>%
  summarise(Vacinacoes = n())

# Reordenar as semanas
dados_grafico$semana <- factor(dados_grafico$semana, levels = unique(dados_grafico$semana))

# Definir as cores para os sexos
cores <- c("M" = "#1F77B4", "F" = "#FF69B4")

# Criar o gráfico de barras
grafico_barras <- ggplot(dados_grafico, aes(x = semana, y = Vacinacoes, fill = enumSexoBiologico)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Semana", y = "Quantidade de Vacinações", fill = "Sexo") +
  scale_fill_manual(values = cores, labels = c("Feminino", "Masculino")) +
  theme_bw() +
  scale_y_continuous(labels = comma)

# Exibir o gráfico
print(grafico_barras)

# Criando o box plot de idade por raça
boxplot_data <- df_analisedescritiva

# Definindo as cores corretas
cores <- c("SEM INFORMACAO" = "#808080",
           "PARDA" = "#8B4513",
           "BRANCA" = "#FFC0CB",
           "PRETA" = "#000000",
           "AMARELA" = "#FFFF00",
           "INDIGENA" = "#FF0000")

grafico_boxplot <- ggplot(boxplot_data, aes(x = racaCor_valor, y = idade, fill = racaCor_valor)) +
  geom_boxplot() +
  labs(x = "Raça", y = "Idade") +
  scale_fill_manual(values = cores) +
  theme_bw() +
  guides(fill = guide_legend(title = "Boxplot de Idade por Raça"))

# Exibir o gráfico
print(grafico_boxplot)

# Lista de raças de interesse
racas <- c("PRETA", "AMARELA", "BRANCA", "PARDA", "INDIGENA")

# Função para calcular a probabilidade de uma vacinação ser de cada raça
calcular_probabilidade <- function(dose) {
  # Subconjunto dos dados para o tipo de dose específico
  dados_dose <- df_analisedescritiva[df_analisedescritiva$descricao_dose == dose, ]
  
  # Cálculo das probabilidades para cada raça
  probabilidades <- sapply(racas, function(raca) {
    total <- sum(dados_dose$racaCor_valor != "SEM INFORMACAO")
    ocorrencias <- sum(dados_dose$racaCor_valor == raca)
    probabilidade <- ocorrencias / total
    return(probabilidade)
  })
  
  # Retornar um vetor de probabilidades
  return(probabilidades)
}

# Função para calcular a probabilidade de uma vacinação ser de cada raça
calcular_probabilidade_reforco <- function() {
  # Subconjunto dos dados para o tipo de dose específico
  dados_dose <- df_analisedescritiva[df_analisedescritiva$descricao_dose %in% c("Reforço", "2º Reforço", "3º Reforço", "1º Reforço"), ]
  
  # Cálculo das probabilidades para cada raça
  probabilidades <- sapply(racas, function(raca) {
    total <- sum(dados_dose$racaCor_valor != "SEM INFORMACAO")
    ocorrencias <- sum(dados_dose$racaCor_valor == raca)
    probabilidade <- ocorrencias / total
    return(probabilidade)
  })
  
  # Retornar um vetor de probabilidades
  return(probabilidades)
}

calcular_probabilidade("1ª Dose")*100
calcular_probabilidade("2ª Dose")*100
calcular_probabilidade_reforco()*100