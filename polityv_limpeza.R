library(readxl)
p5v2018 <- read_excel("p5v2018.xls", sheet = "p5v2018")


head(p5v2018)


library(tidyverse)

glimpse(p5v2018)

# selecionando variáveis: p5 = flag, polity2 = recodificação baseado no p5, as outras são variáveis de definição.
pv2018_mod1 <- p5v2018 %>% 
  select(scode, year, polity, polity2, country)
skim(pv2018_mod1)

glimpse(pv2018_mod1$year)

pv2018_mod1 <- pv2018_mod1 %>% 
  filter(year >= 1970, year <= 2017) %>% 
  mutate(year = as.character(year))

#############################
