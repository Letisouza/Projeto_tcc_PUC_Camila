library(tidyverse)
library(skimr)

#Filtragem do banco pra diminuir os anos.

banco_joined_final <- banco_joined %>% 
  filter(year >= 1975,
         year <= 2015)

# Agrupar o banco por país e indicador e analisar os dados de value e polity

a <- banco_joined_final %>% 
  group_by(Country.Code, Indicator.Code) %>% 
  skim(value, polity)

# Filtrar a variável de rate de competude de dado por value e selecionar as variáveis 
# para contagem de casos válidos.

b <- a %>% 
  filter(skim_variable == "value") %>% 
  filter(complete_rate >= .80) %>% 
  select(Country.Code, Indicator.Code)

# agrupar por país válido e contar

b %>% 
  group_by(Country.Code) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

b %>% 
  group_by(Indicator.Code) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Agrupar por indicador e contar

rate <- b %>% 
  group_by(Indicator.Code) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 30) %>% 
  select(Indicator.Code)


####################

# Filtrar a variável de rate de completude de dados por value e selecionar as variáveis 
# para contagem de casos válidos nas variáveis code de país e indicador.

b <- a %>% 
  filter(skim_variable == "value") %>% 
  filter(complete_rate >= .60) %>% 
  select(Country.Code, Indicator.Code)

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
  select(Indicator.Code, Country.Code)

# Do banco final, filtrar os indicadores educacionais e os países que foram selecionados
# no chunk acima. Depois, salvar esses dados em .csv.

banco_joined_final %>% 
  filter(Indicator.Code %in% indicator_filter$Indicator.Code, 
         Country.Code %in% indicator_filter$Country.Code) %>% 
  write_delim("basefinal_60.csv", delim = ";")




