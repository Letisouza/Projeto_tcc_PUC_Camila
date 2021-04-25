library(readxl)
p5v2018 <- read_excel("p5v2018.xls", sheet = "p5v2018")


head(p5v2018)


library(tidyverse)

glimpse(p5v2018)

# selecionando variáveis: p5 = flag, polity2 = recodificação baseado no p5, as outras são variáveis de definição.
pv2018_mod1 <- p5v2018 %>% 
  select(scode, year, polity, polity2)
skim(pv2018_mod1)

glimpse(pv2018_mod1$year)

pv2018_mod1 <- pv2018_mod1 %>% 
  filter(year >= 1970, year <= 2017) %>% 
  mutate(year = as.character(year))

pv2018_mod1$p5

# contagem dos dados de p5 para ver quantos países tem infos o polity 5.
count = pv2018_mod1 %>% 
  group_by(p5) %>% 
  summarise(varp5=n())

# Contagem dos dados de country para ver quantos anos tem cada país
count2 = pv2018_mod1 %>% 
  group_by(country) %>% 
  summarise(varc=n())

View(count2)

# selecionando países por p5 == 1
pv2018_mod2 <- pv2018_mod1 %>% 
  select(p5, country, cyear, scode, year, polity, polity2) %>% 
  filter(p5 == 1)


glimpse(pv2018_mod2)  

# Contagem das variáveis de score

count3 = pv2018_mod1 %>% 
  group_by(polity) %>% 
  summarise(varpolity=n())

View(count3)


count4 = pv2018_mod1 %>% 
  group_by(polity2) %>% 
  summarise(varpolity=n())

View(count4)

# Teste de seleção usando filtro

pv2018_mod_filtropolity <- pv2018_mod1 %>% 
  select(p5, country, cyear, scode, year, polity, polity2) %>% 
  filter(polity != -88,
         polity != -66,
         polity != -77)

count5 = pv2018_mod_filtropolity %>% 
  group_by(country) %>% 
  summarise(varc=n())

View(count5) # 194 países


pv2018_mod_filtropolity2 <- pv2018_mod1 %>% 
  select(p5, country, cyear, scode, year, polity, polity2) %>% 
  filter(polity2 != -88,
         polity2 != -66)

count6 = pv2018_mod_filtropolity2 %>% 
  group_by(country) %>% 
  summarise(varc=n())

View(count6)

# Contagem de casos por anos

count7 = pv2018_mod1 %>% 
  group_by(year) %>% 
  summarise(varano=n())

View(count7)

# filtragem e contagem ano e país

pv2018_mod3 <- pv2018_mod1 %>% 
  select(p5, country, cyear, scode, year, polity, polity2) %>% 
  filter(year >= "2012" & year <= 2018)
  
count8 = pv2018_mod3 %>% 
  group_by(country) %>% 
  summarise(varcountry = n())
View(count8)  # 168 países

glimpse(pv2018_mod3) 

# contagem por polity e polity2

count9 = pv2018_mod3 %>% 
  group_by(polity) %>% 
  summarise(varpolity = n())
View(count9)

count10 = pv2018_mod3 %>% 
  group_by(polity2) %>% 
  summarise(varpolity2 = n())
View(count10)
 
# Tirando NAs de polity2

pv2018_mod3_semNA <- pv2018_mod3 %>% 
  drop_na(polity2)

count11 = pv2018_mod3_semNA %>% 
  group_by(polity2) %>% 
  summarise(varpolity2 = n())

View(count11)

count12 = pv2018_mod3_semNA %>% 
  group_by(country) %>% 
  summarise(varcountry = n())
View(count12)

glimpse(pv2018_mod3_semNA)

# Linhas precisam ser tiradas: Afeganistão, CoteD'Ivoire e Ivoy Coast.