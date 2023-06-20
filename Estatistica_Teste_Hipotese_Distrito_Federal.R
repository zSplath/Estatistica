install.packages("tidyverse")
install.packages("basedosdados")
library(tidyverse)
library(scales)

library("basedosdados")

# Defina o seu projeto no Google Cloud
set_billing_id("estatistica-390217")

base_parte_0 <- read.csv2("Base/Vacinacao/part-00000-1ffe51ff-585f-4f7d-a6d0-9bf9fb097aae.c000.csv", sep = ";")
base_parte_1 <- read.csv2("Base/Vacinacao/part-00001-1ffe51ff-585f-4f7d-a6d0-9bf9fb097aae.c000.csv", sep = ";")
base_parte_2 <- read.csv2("Base/Vacinacao/part-00002-1ffe51ff-585f-4f7d-a6d0-9bf9fb097aae.c000.csv", sep = ";")
base_parte_3 <- read.csv2("Base/Vacinacao/part-00003-1ffe51ff-585f-4f7d-a6d0-9bf9fb097aae.c000.csv", sep = ";")
base_parte_4 <- read.csv2("Base/Vacinacao/part-00004-1ffe51ff-585f-4f7d-a6d0-9bf9fb097aae.c000.csv", sep = ";")

#consolida base
df_baseCovid = rbind(base_parte_0, base_parte_1, base_parte_2, base_parte_3, base_parte_4)

#informações sobre a abase
nrow(df_baseCovid)
head(df_baseCovid, 5)
glimpse(df_baseCovid)

# Filtrar os dados do ano de 2021
df_baseCovid <- df_baseCovid %>%
  filter(format(as.Date(vacina_dataAplicacao), "%Y") == "2021")

#==================================================================================
# População estimada para 2021 no DF - base Ministério da Saúde
df_populacao_DistritoFederal <- bdplyr("basedosdados-dev.br_ms_populacao.municipio") %>%
  #filtrar o município pelo código do IBGE e o ano de 2021
  filter(id_municipio == "5300108",
         ano == 2021) %>% 
  bd_collect()

head(df_populacao_DistritoFederal,5)
glimpse(df_populacao_DistritoFederal)
#==================================================================================
#Agrupa Pacientes em Grupos de Idades
df_baseCovid <- df_baseCovid %>% 
  mutate(paciente_grupo_idade = case_when(
    paciente_idade <=4 ~ "0-4 anos",
    paciente_idade >=5 & paciente_idade <=9 ~ "5-9 anos",
    paciente_idade >=10 & paciente_idade <=14 ~ "10-14 anos",
    paciente_idade >=15 & paciente_idade <=19 ~ "15-19 anos",
    paciente_idade >=20 & paciente_idade <=24 ~ "20-24 anos",
    paciente_idade >=25 & paciente_idade <=29 ~ "25-29 anos",
    paciente_idade >=30 & paciente_idade <=34 ~ "30-34 anos",
    paciente_idade >=35 & paciente_idade <=39 ~ "35-39 anos",
    paciente_idade >=40 & paciente_idade <=44 ~ "40-44 anos",
    paciente_idade >=45 & paciente_idade <=49 ~ "45-49 anos",
    paciente_idade >=50 & paciente_idade <=54 ~ "50-54 anos",
    paciente_idade >=55 & paciente_idade <=59 ~ "55-59 anos",
    paciente_idade >=60 & paciente_idade <=64 ~ "60-64 anos",
    paciente_idade >=65 & paciente_idade <=69 ~ "65-69 anos",
    paciente_idade >=70 & paciente_idade <=74 ~ "70-74 anos",
    paciente_idade >=75 & paciente_idade <=79 ~ "75-79 anos",
    paciente_idade >=80 ~ "80 mais"
  ))

#renomeia coluna paciente_enumSexoBiologico para sexo
df_baseCovid <- df_baseCovid %>%
  rename(sexo = "paciente_enumSexoBiologico")

#Conta pacientes em cada grupo de idade
df_baseCovidCountIdade <- df_baseCovid %>%
  select(paciente_grupo_idade, sexo, vacina_descricao_dose	) %>%
  group_by(paciente_grupo_idade, sexo, vacina_descricao_dose	)

#=================================================================================
#compatibilizando dados e e nome das colunas com base COVID
df_populacao_DistritoFederal <- df_populacao_DistritoFederal %>%
  mutate(sexo = recode(sexo,
                       "masculino" = "M",
                       "feminino" = "F"))

df_populacao_DistritoFederal <- df_populacao_DistritoFederal %>%
  rename(paciente_grupo_idade = "grupo_idade")


#=================================================================================

#Criando dataframe para teste de correlação entre grupo de idade e a marca das vacinas
#além disse está sendo ajustada as variaveis para o tipo categorico
TESTE_CORRELACAO <- df_baseCovid %>%
  mutate(paciente_grupo_idade = factor(paciente_grupo_idade)) %>%
  mutate(vacina_codigo = factor(vacina_codigo))

#retirando valores nulos da base
TESTE_CORRELACAO <- TESTE_CORRELACAO[complete.cases(TESTE_CORRELACAO), ]


# criando uma tabela de contingência entre as duas variáveis categóricas
TESTE_contingencia <- table(TESTE_CORRELACAO$paciente_grupo_idade, TESTE_CORRELACAO$vacina_codigo)
print(TESTE_contingencia)


#para realizar o teste qui-quadrado de independência é necesário retirar as frequencias muito baixas 
# Remover colunas "98" e "99"
TESTE_contingencia <- TESTE_contingencia[, !(colnames(TESTE_contingencia) %in% c("98", "99"))]

# Remover linhas "0-4 anos", "5-9 anos"
TESTE_contingencia <- TESTE_contingencia[!(rownames(TESTE_contingencia) %in% c("0-4 anos", "5-9 anos")), ]


# Realizar o teste qui-quadrado de independência
TESTE_resultado <- chisq.test(TESTE_contingencia)

# Exibir o resultado do teste
print(TESTE_resultado)
