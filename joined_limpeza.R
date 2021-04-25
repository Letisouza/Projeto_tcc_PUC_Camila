library(tidyverse)
library(skimr)

#Filtragem do banco pra diminuir os anos.

banco_joined_final <- banco_joined %>% 
  filter(year >= 1975,
         year <= 2015) 

# Checar NAs em polity

count_teste = banco_joined_final %>% 
  group_by(ï..Country.Name, polity2) %>%
  summarise(varc=n())

# tirando paises só com casos NA em polity2

library(readxl)
paises_na <- read_excel("paises_polity2_na.xlsx") %>% 
  select(country_na_polity2) %>% 
  as.vector()

paises_na

banco_joined_final <- banco_joined_final %>% 
  filter(!ï..Country.Name %in% paises_na$country_na_polity2)


# Agrupar o banco por país e indicador e analisar os dados de value e polity

a <- banco_joined_final %>% 
  group_by(ï..Country.Name, Indicator.Code) %>% 
  skim(value, polity)

# Salvando banco limpo total (válidos e inválidos de indicadores)

banco_joined_final %>% 
  write_delim("banco_joined_final.csv", delim = ";")

####################

# Filtrar a variável de rate de completude de dados por value e selecionar as variáveis 
# para contagem de casos válidos nas variáveis code de país e indicador.

b <- a %>% 
  filter(skim_variable == "value") %>% 
  filter(complete_rate >= .70) %>% 
  select(ï..Country.Name, Indicator.Code)

# Agrupar por indicador e contar

rate <- b %>% 
  group_by(Indicator.Code) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 30)

# baseado no range de cima, agrupa pelo indicador e conta para quantas vezes ele aparece.
# Depois, filtrando pela quantidade >= a 30 de vezes, seleciona os indicadores e une com 
# a base que tem a contagem de indicadores por país.

indicator_filter <- b %>% 
  group_by(Indicator.Code) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 30) %>% 
  select(Indicator.Code) %>% 
  inner_join(b, by = "Indicator.Code") %>% 
  select(Indicator.Code, ï..Country.Name)

# Do banco final, filtrar os indicadores educacionais e os países que foram selecionados
# no chunk acima. Depois, salvar esses dados em .csv.

banco_70 <- banco_joined_final %>% 
  filter(Indicator.Code %in% indicator_filter$Indicator.Code, 
         ï..Country.Name %in% indicator_filter$ï..Country.Name) %>% 
  write_delim("basefinal_70.csv", delim = ";")

###############################

# Brasil

indicator_filter_br <- b %>% 
  group_by(Indicator.Code) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 30) %>% 
  select(Indicator.Code) %>% 
  inner_join(b, by = "Indicator.Code") %>% 
  select(Indicator.Code, ï..Country.Name) %>% 
  filter(ï..Country.Name == "Brazil")


banco_70_br <- banco_joined_final %>% 
  filter(Indicator.Code %in% indicator_filter_br$Indicator.Code, 
         ï..Country.Name %in% indicator_filter_br$ï..Country.Name) %>% 
  write_delim("basefinal_70_br.csv", delim = ";")
