edstats <-  read.csv("EdStatsData.csv")

library(tidyverse)

glimpse(edstats)

skimr::skim(edstats)

# filtrando pelas variáveis indesejadas

edstats_filtrado <- edstats %>% 
  select(-c( "X2020", "X2025", "X2030", "X2035", "X2040", "X2045", "X2050", "X2055", "X2060", "X2065", "X2070", "X2075", "X2080", "X2085", "X2090", "X2095", "X2100", "X"))


# Mudando o formato

edstats_mod <- edstats_filtrado %>% 
  pivot_longer(
  cols = starts_with("X"), 
  names_to = "year", 
  values_to = "value"
)

glimpse(edstats_mod)


# selecionando os indicadores

library(readxl)
Variaveis_edstats <- read_excel("Variaveis_edstats.xlsx") %>% 
  select(Código) %>% 
  as.vector()
Variaveis_edstats

edstats_mod <- edstats_mod %>% 
  filter(Indicator.Code %in% Variaveis_edstats$Código)

# tirando valores de países agregados

agregados <- c("WLD", "ARB", "EAP","EAS", "ECA","ECS", "EMU", "EUU", "HPC", "LCN", "LDC", "LIC", "LMC", "LMY", "MEA", "MIC", "MNA", "NAC", "OED", "SAS", "SSA", "SSF", "UMC", "HIC", "LAC")

edstats_mod <- edstats_mod %>% 
  filter(!Country.Code %in% agregados) %>% 
  mutate(year = substring(year, 2))

##########################################

# juntando bases pelo nome do país

banco_joined <- left_join(edstats_mod, pv2018_mod1, 
                          by = c("ï..Country.Name" = "country",
                                 "year" = "year"))
View(banco_joined)


