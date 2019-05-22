#######################################################
### Tesis: Desigualdad de oportunidades y crecimiento economico en Mexico 
### ENIGH MCS 2012
#######################################################

### Librerias ----
library(dplyr)
library(readxl)
library(officer)
library(rmarkdown)
library(flextable)
library(rvg)
library(mschart)
library(devEMF)
library(tidyverse)
library(stargazer)
library(stats)
library(ggplot2)
library(GGally)
library(ggthemes)
library(aTSA)
library(xtable)
library(ggthemes)
library(magrittr)
library(broom)
library(haven)
library(foreign)
library(sjlabelled)
library(survey)
library(tis)
options(scipen = 1000000) #Prevenir Notacion Cientifica
Sys.setlocale("LC_ALL", "es_ES.UTF-8") #Permitir Acentos




### ENIGH 2012 ----

base_hijos <- read_dta("Bases de datos/MCS 2012/base_hijos_mf.dta")
base_madre <- read_dta("Bases de datos/MCS 2012/base_madre_mf.dta")
base_padre <- read_dta("Bases de datos/MCS 2012/base_padre_mf.dta")

# Merge ----

#mergem
mergep <- merge(base_hijos, base_padre, by =c("folioviv","foliohog","IDp"),suffixes = c(".h",".p"))

#mergep

mergem <- merge(base_hijos, base_madre, by =c("folioviv","foliohog","IDm"),suffixes = c(".h",".m"))

#merget <-   merge(mergep, base_madre, by =c("folioviv","foliohog"),suffixes = c(".h","#.m"))
#mergetot <- full_join(mergep, base_madre, by = c("folioviv","foliohog"))

#### Division por entidades federativas ----

#Aqui divido las observaciones por entidad federativa para, posteriormente, hacer regresiones para cada una de las entidades 

mergep<- mergep %>% 
  mutate(EDO = str_sub(folioviv, 1, 2))

mergep<- mergep %>%   
  mutate(codigo = case_when(
    EDO == "01" ~ 1,
    EDO == "02" ~ 2,
    EDO == "03" ~ 3,
    EDO == "04" ~ 4,
    EDO == "05" ~ 5,
    EDO == "06" ~ 6,
    EDO == "07" ~ 7,
    EDO == "08" ~ 8,
    EDO == "09" ~ 9,
    EDO == "10" ~ 10,
    EDO == "11" ~ 11,
    EDO == "12" ~ 12,
    EDO == "13" ~ 13,
    EDO == "14" ~ 14,
    EDO == "15" ~ 15,
    EDO == "16" ~ 16,
    EDO == "17" ~ 17,
    EDO == "18" ~ 18,
    EDO == "19" ~ 19,
    EDO == "20" ~ 20,
    EDO == "21" ~ 21,
    EDO == "22" ~ 22,
    EDO == "23" ~ 23,
    EDO == "24" ~ 24,
    EDO == "25" ~ 25,
    EDO == "26" ~ 26,
    EDO == "27" ~ 27,
    EDO == "28" ~ 28,
    EDO == "29" ~ 29,
    EDO == "30" ~ 30,
    EDO == "31" ~ 31,
    EDO == "32" ~ 32))

mergem<- mergem %>% 
  mutate(EDO = str_sub(folioviv, 1, 2)) 

mergem<- mergem %>%   
  mutate(codigo = case_when(
    EDO == "01" ~ 1,
    EDO == "02" ~ 2,
    EDO == "03" ~ 3,
    EDO == "04" ~ 4,
    EDO == "05" ~ 5,
    EDO == "06" ~ 6,
    EDO == "07" ~ 7,
    EDO == "08" ~ 8,
    EDO == "09" ~ 9,
    EDO == "10" ~ 10,
    EDO == "11" ~ 11,
    EDO == "12" ~ 12,
    EDO == "13" ~ 13,
    EDO == "14" ~ 14,
    EDO == "15" ~ 15,
    EDO == "16" ~ 16,
    EDO == "17" ~ 17,
    EDO == "18" ~ 18,
    EDO == "19" ~ 19,
    EDO == "20" ~ 20,
    EDO == "21" ~ 21,
    EDO == "22" ~ 22,
    EDO == "23" ~ 23,
    EDO == "24" ~ 24,
    EDO == "25" ~ 25,
    EDO == "26" ~ 26,
    EDO == "27" ~ 27,
    EDO == "28" ~ 28,
    EDO == "29" ~ 29,
    EDO == "30" ~ 30,
    EDO == "31" ~ 31,
    EDO == "32" ~ 32))


datos2012 <- read_excel("Bases de datos/MCS 2012/datos2012.xlsx")

# Aqui aniado variables de interes por entidad federativa para todas las observaciones para correr el modelo
mergep <- left_join(mergep, datos2012, by = "codigo")
mergem <- left_join(mergem, datos2012, by = "codigo")

# Modelo lineal de desigualdad de oportunidades ----

### Regresiones por entidad federativa en la base de los padres----

by_entidad <- mergep %>% 
  group_by(EDO) %>% 
  nest()
by_entidad 

#Funcion de la regresion para padres de todas las entidades federativas

model_entidad  <- function(df) {
  lm(IEhij ~ IEpad + `Pobreza 2012`+ log10(EscolProm2012), data = df)
}

models <- map(by_entidad$data, model_entidad)

by_entidad <- by_entidad %>% 
  mutate(model = map(data, model_entidad))
by_entidad


#Resultados del modelo por entidad federativa, R cuadradas 
rsquared_p <- by_entidad %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
rsquared_p


#Grafica de R cuadrada por entidad federativa
rsquared_p %>% 
  mutate(Entidad = case_when(EDO == "01" ~ "Aguascalientes", 
                             EDO == "02" ~ "Baja California",
                             EDO == "03" ~ "Baja California Sur",
                             EDO == "04" ~ "Campeche",
                             EDO == "05" ~ "Coahuila ",
                             EDO == "06" ~ "Colima ",
                             EDO == "07" ~ "Chiapas",
                             EDO == "08" ~ "Chihuahua",
                             EDO == "09" ~ "Ciudad de Mexico",
                             EDO == "10" ~ "Durango",
                             EDO == "11" ~ "Guanajuato",
                             EDO == "12" ~ "Guerrero",
                             EDO == "13" ~ "Hidalgo",
                             EDO == "14" ~ "Jalisco",
                             EDO == "15" ~ "Estado de Mexico",
                             EDO == "16" ~ "Michoacan",
                             EDO == "17" ~ "Morelos",
                             EDO == "18" ~ "Nayarit",
                             EDO == "19" ~ "Nuevo Leon",
                             EDO == "20" ~ "Oaxaca",
                             EDO == "21" ~ "Puebla",
                             EDO == "22" ~ "Queretaro",
                             EDO == "23" ~ "Quintana Roo",
                             EDO == "24" ~ "San Luis Potosi ",
                             EDO == "25" ~ "Sinaloa",
                             EDO == "26" ~ "Sonora",
                             EDO == "27" ~ "Tabasco",
                             EDO == "28" ~ "Tamaulipas",
                             EDO == "29" ~ "Tlaxcala",
                             EDO == "30" ~ "Veracruz",
                             EDO == "31" ~ "Yucatan",
                             EDO == "32" ~ "Zacatecas")) %>% 
  ggplot(mapping = aes(x = reorder(Entidad, r.squared), y = r.squared)) + 
  geom_point(na.rm = TRUE)+
  labs(title = "Desigualdad de oportunidades por entidad federativa",
       subtitle = "Grafica de R cuadrada por entidad federativa (padres)",
       x = "Entidad federativa",
       y = "R cuadrada",
       caption = "Fuente: Elaborado por Sergio Parra con datos de la ENIGH.") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1,# just "horizontal" de texto 
                                   vjust = 0.5))



#Resultados del modelo por entidad federativa, coeficientes

coeff_p <- by_entidad %>% 
  mutate(tidy = map(model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE)

coeff_p

#Grafica de coeficientes e interceptos por entidad federativa

coeff_p %>% 
  filter(term == "IEpad") %>% 
  mutate(Entidad = case_when(EDO == "01" ~ "Aguascalientes", 
                             EDO == "02" ~ "Baja California",
                             EDO == "03" ~ "Baja California Sur",
                             EDO == "04" ~ "Campeche",
                             EDO == "05" ~ "Coahuila ",
                             EDO == "06" ~ "Colima ",
                             EDO == "07" ~ "Chiapas",
                             EDO == "08" ~ "Chihuahua",
                             EDO == "09" ~ "Ciudad de Mexico",
                             EDO == "10" ~ "Durango",
                             EDO == "11" ~ "Guanajuato",
                             EDO == "12" ~ "Guerrero",
                             EDO == "13" ~ "Hidalgo",
                             EDO == "14" ~ "Jalisco",
                             EDO == "15" ~ "Estado de Mexico",
                             EDO == "16" ~ "Michoacan",
                             EDO == "17" ~ "Morelos",
                             EDO == "18" ~ "Nayarit",
                             EDO == "19" ~ "Nuevo Leon",
                             EDO == "20" ~ "Oaxaca",
                             EDO == "21" ~ "Puebla",
                             EDO == "22" ~ "Queretaro",
                             EDO == "23" ~ "Quintana Roo",
                             EDO == "24" ~ "San Luis Potosi ",
                             EDO == "25" ~ "Sinaloa",
                             EDO == "26" ~ "Sonora",
                             EDO == "27" ~ "Tabasco",
                             EDO == "28" ~ "Tamaulipas",
                             EDO == "29" ~ "Tlaxcala",
                             EDO == "30" ~ "Veracruz",
                             EDO == "31" ~ "Yucatan",
                             EDO == "32" ~ "Zacatecas")) %>% 
  ggplot(mapping = aes(x = reorder(Entidad, estimate), y = estimate))+
  geom_point(na.rm = TRUE)+
  expand_limits(x = 0, y = 0.1) +
  #geom_jitter(width = 0.5)+
  labs(title = "Desigualdad de oportunidades por entidad federativa",
       subtitle = "Esta grafica muestra la persistencia de la desigualdad de oportunidades (padres)",
       x = "Entidad federativa",
       y = "Estimacion",
       caption = "Fuente: Elaborado por Sergio Parra con datos de la ENIGH.") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1,# just "horizontal" de texto 
                                   vjust = 0.5))

coeffp2012<- coeff_p %>% 
  filter(term == "IEpad") %>% 
  select(EDO, term, estimate)
View(coeffp2012)

rsquared_p2012 <- rsquared_p %>% 
  select(EDO, r.squared)
View(rsquared_p2012)

### Regresiones por entidad federativa en la base de las madres ----
#En este punto aniado mas variables para hacer una mejor estimacion de logro educativo 

by_entidad_m <- mergem %>% 
  group_by(EDO) %>% 
  nest()
by_entidad_m

#Funcion de la regresion para padres de todas las entidades federativas

model_entidad_m  <- function(dfm) {
  lm(IEhij ~ IEmad + `Pobreza 2012` + log10(EscolProm2012), data = dfm)
}

models_m <- map(by_entidad_m$data, model_entidad_m)

by_entidad_m <- by_entidad_m %>% 
  mutate(model = map(data, model_entidad_m))
by_entidad_m


#Resultados del modelo por entidad federativa, R cuadradas 
rsquared_m <- by_entidad_m %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
rsquared_m


#Grafica de R cuadrada por entidad federativa
rsquared_m %>% 
  mutate(Entidad = case_when(EDO == "01" ~ "Aguascalientes", 
                             EDO == "02" ~ "Baja California",
                             EDO == "03" ~ "Baja California Sur",
                             EDO == "04" ~ "Campeche",
                             EDO == "05" ~ "Coahuila ",
                             EDO == "06" ~ "Colima ",
                             EDO == "07" ~ "Chiapas",
                             EDO == "08" ~ "Chihuahua",
                             EDO == "09" ~ "Ciudad de Mexico",
                             EDO == "10" ~ "Durango",
                             EDO == "11" ~ "Guanajuato",
                             EDO == "12" ~ "Guerrero",
                             EDO == "13" ~ "Hidalgo",
                             EDO == "14" ~ "Jalisco",
                             EDO == "15" ~ "Estado de Mexico",
                             EDO == "16" ~ "Michoacan",
                             EDO == "17" ~ "Morelos",
                             EDO == "18" ~ "Nayarit",
                             EDO == "19" ~ "Nuevo Leon",
                             EDO == "20" ~ "Oaxaca",
                             EDO == "21" ~ "Puebla",
                             EDO == "22" ~ "Queretaro",
                             EDO == "23" ~ "Quintana Roo",
                             EDO == "24" ~ "San Luis Potosi ",
                             EDO == "25" ~ "Sinaloa",
                             EDO == "26" ~ "Sonora",
                             EDO == "27" ~ "Tabasco",
                             EDO == "28" ~ "Tamaulipas",
                             EDO == "29" ~ "Tlaxcala",
                             EDO == "30" ~ "Veracruz",
                             EDO == "31" ~ "Yucatan",
                             EDO == "32" ~ "Zacatecas")) %>% 
  ggplot(mapping = aes(x = reorder(Entidad, r.squared), y = r.squared)) + 
  geom_point(na.rm = TRUE)+
  labs(title = "Desigualdad de oportunidades por entidad federativa (madres)",
       subtitle = "Grafica de R cuadrada por entidad federativa",
       x = "Entidad federativa",
       y = "R cuadrada",
       caption = "Fuente: Elaborado por Sergio Parra con datos de la ENIGH 2014.") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1,# just "horizontal" de texto 
                                   vjust = 0.5))



#Resultados del modelo por entidad federativa, coeficientes

coeff_m <- by_entidad_m %>% 
  mutate(tidy = map(model, broom::tidy)) %>% 
  unnest(tidy, .drop = TRUE)

coeff_m


#Grafica de coeficientes e interceptos por entidad federativa

coeff_m %>% 
  filter(term == "IEmad") %>% 
  mutate(Entidad = case_when(EDO == "01" ~ "Aguascalientes", 
                             EDO == "02" ~ "Baja California",
                             EDO == "03" ~ "Baja California Sur",
                             EDO == "04" ~ "Campeche",
                             EDO == "05" ~ "Coahuila ",
                             EDO == "06" ~ "Colima ",
                             EDO == "07" ~ "Chiapas",
                             EDO == "08" ~ "Chihuahua",
                             EDO == "09" ~ "Ciudad de Mexico",
                             EDO == "10" ~ "Durango",
                             EDO == "11" ~ "Guanajuato",
                             EDO == "12" ~ "Guerrero",
                             EDO == "13" ~ "Hidalgo",
                             EDO == "14" ~ "Jalisco",
                             EDO == "15" ~ "Estado de Mexico",
                             EDO == "16" ~ "Michoacan",
                             EDO == "17" ~ "Morelos",
                             EDO == "18" ~ "Nayarit",
                             EDO == "19" ~ "Nuevo Leon",
                             EDO == "20" ~ "Oaxaca",
                             EDO == "21" ~ "Puebla",
                             EDO == "22" ~ "Queretaro",
                             EDO == "23" ~ "Quintana Roo",
                             EDO == "24" ~ "San Luis Potosi ",
                             EDO == "25" ~ "Sinaloa",
                             EDO == "26" ~ "Sonora",
                             EDO == "27" ~ "Tabasco",
                             EDO == "28" ~ "Tamaulipas",
                             EDO == "29" ~ "Tlaxcala",
                             EDO == "30" ~ "Veracruz",
                             EDO == "31" ~ "Yucatan",
                             EDO == "32" ~ "Zacatecas")) %>% 
  ggplot(mapping = aes(x = reorder(Entidad, estimate), y = estimate))+
  geom_point(na.rm = TRUE)+
  expand_limits(x = 0, y = 0.1) +
  labs(title = "Desigualdad de oportunidades por entidad federativa",
       subtitle = "Esta grafica muestra la persistencia de la desigualdad de oportunidades en Mexico (madres)",
       x = "Entidad federativa",
       y = "Estimacion",
       caption = "Fuente: Elaborado por Sergio Parra con datos de la ENIGH 2012.") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1,# just "horizontal" de texto 
                                   vjust = 0.5))

coeffm2012<- coeff_m %>% 
  filter(term == "IEmad") %>% 
  select(EDO, term, estimate)
View(coeffm2012)

rsquared_m2012 <- rsquared_m %>% 
  select(EDO, r.squared)
View(rsquared_m2012)

