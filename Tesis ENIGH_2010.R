#######################################################
### Tesis: Desigualdad de oportunidades y crecimiento economico en Mexico ENIGH 2010
######################################################

### Librerias ----
library(dplyr)
library(readxl)
library(officer)
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

#ENIGH 2010 ----

poblacion10 <- read_dta("Desktop/Tesis/Bases de datos/MCS 2010/Pobla10.dta")

#### Indice de Educacion ----

poblacion10 <- poblacion10 %>% 
  filter(!(parentesco >400 & parentesco<500)) %>% 
  filter(!(parentesco>700 & parentesco<=715))

table(poblacion10$parentesco)

#Asistencia Escolar 

poblacion10$asis_esc<-as.numeric(poblacion10$asis_esc)
#Definicion
#Situacion que distingue a las personas de 3 o mas anios segun asistan o no a algun establecimiento de ensenianza escolar del Sistema Educativo Nacional (SEN), en cualquier nivel.
poblacion10 <- poblacion10 %>% 
  mutate(
    AsisEsc = case_when(
      asis_esc==1 ~ 1,
      asis_esc==2 ~ 0))

names(poblacion10)

##Escolaridad esperada 

#Normalizamos la edad para tener educacion esperada. 
poblacion10 <- poblacion10 %>% 
  mutate(
    norm = case_when(
      edad==6  ~ 1 ,
      edad==7  ~ 2 ,
      edad==8  ~ 3 ,
      edad==9  ~ 4 ,
      edad==10 ~ 5 ,
      edad==11 ~ 6 ,
      edad==12 ~ 7 ,
      edad==13 ~ 8 ,
      edad==14 ~ 9 ,
      edad==15 ~ 10,
      edad==16 ~ 11,
      edad==17 ~ 12,
      edad==18 ~ 13,
      edad==19 ~ 14,
      edad==20 ~ 15,
      edad==21 ~ 16,
      edad==22 ~ 17,
      edad==23 ~ 18,
      edad==24 ~ 18,
      edad>=25 ~ 15))

###ERROR: en algunos numeros

poblacion10 <- poblacion10 %>% 
  mutate(gradoaprob = as.numeric(gradoaprob),
         nivelaprob = as.numeric(nivelaprob),
         edad = as.numeric(edad),
         antec_esc = as.numeric(antec_esc)) %>% 
  mutate( 
    Esc = case_when(
      nivelaprob==0 ~ 0,
      nivelaprob==1 ~ 0,
      #Primaria Secundaria y Preparatoria
      nivelaprob==2 & gradoaprob==1 ~ 1,
      nivelaprob==2 & gradoaprob==2 ~ 2,
      nivelaprob==2 & gradoaprob==3 ~ 3,
      nivelaprob==2 & gradoaprob==4 ~ 4,
      nivelaprob==2 & gradoaprob==5 ~ 5,
      nivelaprob==2 & gradoaprob>=6 ~ 6,
      nivelaprob==3 & gradoaprob==0 ~ 6,
      nivelaprob==3 & gradoaprob==1 ~ 7,
      nivelaprob==3 & gradoaprob==2 ~ 8,
      nivelaprob==3 & gradoaprob>=3 ~ 9,
      nivelaprob==6 & gradoaprob>=1 & antec_esc == NA ~ 6,
      nivelaprob==4 & gradoaprob==0 ~ 10,
      nivelaprob==4 & gradoaprob==1 ~ 10,
      nivelaprob==4 & gradoaprob==2 ~ 11,
      nivelaprob==4 & gradoaprob>=3 ~ 12,
      #5 Normal. Antecedentes: 2 sec, 3 prepa. Grados aprobados: 1,2,3,4 Maximo 4
      nivelaprob==5 & antec_esc ==1 & gradoaprob==0 ~ 6,
      nivelaprob==5 & antec_esc ==1 & gradoaprob==1 ~ 7,
      nivelaprob==5 & antec_esc ==1 & gradoaprob==2 ~ 8,
      nivelaprob==5 & antec_esc ==1 & gradoaprob==3 ~ 9,
      nivelaprob==5 & antec_esc ==1 & gradoaprob>=4 ~ 10,
      nivelaprob==5 & antec_esc ==2 & gradoaprob==0 ~ 9,
      nivelaprob==5 & antec_esc ==2 & gradoaprob==1 ~ 10,
      nivelaprob==5 & antec_esc ==2 & gradoaprob==2 ~ 11,
      nivelaprob==5 & antec_esc ==2 & gradoaprob==3 ~ 12,
      nivelaprob==5 & antec_esc ==2 & gradoaprob>=4 ~ 13,
      nivelaprob==5 & antec_esc ==3 & gradoaprob==0 ~ 12,
      nivelaprob==5 & antec_esc ==3 & gradoaprob==1 ~ 13,
      nivelaprob==5 & antec_esc ==3 & gradoaprob==2 ~ 14,
      nivelaprob==5 & antec_esc ==3 & gradoaprob==3 ~ 15,
      nivelaprob==5 & antec_esc ==3 & gradoaprob>=4 ~ 16,
      #Hay dos casos que tienen licenciatura + normal se topo en 18 anios 
      nivelaprob==5 & antec_esc ==4 ~ 18,
      #6 Carrera Tecnica. Antecedentes: 1 prim, 2 sec, 3 prepa. Grados Aprobados:1,2,3,4,5,***Maximo 3
      nivelaprob==6 & antec_esc ==1 ~ 6 + gradoaprob,
      nivelaprob==6 & antec_esc ==1 & gradoaprob>=3 ~ 9,#m
      nivelaprob==6 & antec_esc ==2 ~ 9 + gradoaprob, 
      nivelaprob==6 & antec_esc ==2 & gradoaprob>=3 ~ 12,
      nivelaprob==6 & antec_esc ==3 & gradoaprob==0 ~ 12,
      nivelaprob==6 & antec_esc ==3 & gradoaprob==1 ~ 13,
      nivelaprob==6 & antec_esc ==3 & gradoaprob==2 ~ 14,
      nivelaprob==6 & antec_esc ==3 & gradoaprob>=3 ~ 15,
      nivelaprob==6 & antec_esc >=4 ~ 17,
      #7 Profesional. Antecedentes: 3 prepa. Grados Aprobados:1,2,3,4,5,6**
      nivelaprob==7 & gradoaprob==0 ~ 12,
      nivelaprob==7 & gradoaprob==1 ~ 13,
      nivelaprob==7 & gradoaprob==2 ~ 14,
      nivelaprob==7 & gradoaprob==3 ~ 15,
      nivelaprob==7 & gradoaprob==4 ~ 16,
      nivelaprob==7 & gradoaprob>=5 ~ 17,
      nivelaprob==7 & antec_esc ==3 & gradoaprob>5 ~ 17,
      #8 Maestria. Antecedentes: 4 Licenciatura. Grados Aprobados:1,2,3,4,5,6**
      nivelaprob==8 & antec_esc ==4 & gradoaprob==0 ~ 17,
      nivelaprob==8 & gradoaprob==1 ~ 18,
      nivelaprob==8 & gradoaprob==1 ~ 18,
      nivelaprob==8 & gradoaprob==2 ~ 19,
      nivelaprob==8 & gradoaprob>=2 ~ 19,
      #9 Doctorado. Antecedentes: 4 Licenciatura, 5 maestria. Grados Aprobados:1,2,3,4,5,6**
      nivelaprob==9 & gradoaprob==0 ~ 19,
      nivelaprob==9 & gradoaprob==1 ~ 20,
      nivelaprob==9 & gradoaprob==2 ~ 21,
      nivelaprob==9 & gradoaprob==3 ~ 22,
      nivelaprob==9 & gradoaprob==4 ~ 22,
      nivelaprob==9 & gradoaprob>=5 ~ 22)) 

poblacion10 <- poblacion10 %>% 
  mutate(Esc = na_if(Esc, edad <= 5)) 

mean(poblacion10$Esc, na.rm = TRUE)

table(poblacion10$Esc)

#Generamos escolaridad esperada
poblacion10 <- poblacion10 %>% 
  mutate(esc_esp = Esc + AsisEsc) %>% 
  mutate(esc_esp = replace(esc_esp, nivelaprob==9 & gradoaprob>=3, 22))

#casos especiales
#nivelaprob == NA ~ AsisEsc,
#edad>24 ~ Esc))


table(poblacion10$esc_esp)

#Generamos la tasa de esc dividiendo la esccolaridad entre la norma**

##ERROR NO ME ESTA GENERANDO DE MANERA CORRECTA LOS INDICES 
poblacion10<- poblacion10 %>% 
  mutate(IE = case_when(
    edad >= 6 ~ esc_esp / norm
  )) %>% 
  mutate(IE = replace(IE, IE>1, 1)) 

#mutate(IE = replace(IE, IE != NA_real_, 1)) #Falta ver esta condicion en do file 


table(poblacion10$IE)
mean(poblacion10$IE, na.rm = TRUE)

#Asignamos a los menores de 6 el indice promedio del hogar**

poblacion10[order(poblacion10$folioviv, poblacion10$foliohog),]

poblacion10<- poblacion10 %>% 
  mutate(folio = str_c(folioviv, foliohog, collapse = NULL))

poblacion10[order(poblacion10$folio, poblacion10$folioviv),]

poblacion10 <- poblacion10 %>% 
  transform %>% 
  group_by(folio) %>% 
  mutate(MediaIE = mean(IE))

print(poblacion10$MediaIE)
sum(is.na(poblacion10$MediaIE)) 

poblacion10 <- poblacion10 %>% 
  mutate(IE = replace(IE, edad<6, MediaIE)) 



#Cambiamos nombres de variables 

poblacion10 <- poblacion10 %>%
  var_labels(AsisEsc = "Asistencia escolar",
             Esc  = "Escolaridad mayores de 25 anios",
             norm = "Norma",
             esc_esp = "Escolaridad esperada",
             MediaIE ="media IE",
             IE = "Indice de educacion") 

#Cambiamos valor para la variable de sexo 
poblacion10 <- poblacion10 %>% 
  mutate(
    sex = case_when(
      sexo ==1 ~ 0,
      sexo ==2 ~ 1))

poblacion10<- select (poblacion10,-c(sexo))
poblacion10 <- poblacion10 %>% rename(sexo = sex)

# Base de hijos

poblacion10_h <- poblacion10

poblacion10_h <- poblacion10_h %>% 
  filter((id_madre == 1 | id_padre == 1)) ##Aqui cambio el ident  ificador de la madre y el padre 
poblacion10_h <- poblacion10_h %>% 
  mutate(IDp = str_c(folioviv, foliohog, nr_padre,collapse = NULL))

poblacion10_h <- poblacion10_h %>% 
  mutate(IDm = str_c(folioviv, foliohog, nr_madre,collapse = NULL))


#Base de padres: nos quedamos con los hombres 

poblacion10_p <- poblacion10
poblacion10_p<- subset(poblacion10_p, sexo == 0 )

poblacion10_p <- poblacion10_p %>% 
  mutate(leng = str_length(numren))

poblacion10_p %>% 
  mutate(
    numren = case_when(
      leng == 1 ~ "0"
    )
  )


#Generamos identificadores unicos de hogar

poblacion10_p <- poblacion10_p %>% 
  mutate(IDp = str_c(folioviv, foliohog, numren,collapse = NULL),
         padre = 1)

colnames(poblacion10_p)


#Base de madres 

poblacion10_m <- poblacion10

poblacion10_m<- subset(poblacion10_m, sexo == 1 )

###Nota: la base en r ya me lo toma como character, no necesito hacer transformacion

poblacion10_m <- poblacion10_m %>% 
  mutate(leng = str_length(numren))


poblacion10_m %>% 
  mutate(
    numren = case_when(
      leng == 1 ~ "0"
    )
  )

#Generamos identificadores unicos de hogar

poblacion10_m <- poblacion10_m %>% 
  mutate(IDm = str_c(folioviv, foliohog, numren,collapse = NULL),
         madre = 1)


colnames(poblacion10_m)


#### Merge ----

#mergep

mergep <- merge(poblacion10_h, poblacion10_p, by =c("folioviv","foliohog","IDp"),suffixes = c(".h",".p"))

#mergem 

mergem <- merge(poblacion10_h, poblacion10_m, by =c("folioviv","foliohog","IDm"),suffixes = c(".h",".m"))
#### Division por entidades federativas ----

#Aqui divido las observaciones por entidad federativa para, posteriormente, hacer regresiones para cada una de las entidades 

mergep<- mergep %>% 
  mutate(EDO = str_sub(folioviv, 1, 2)) 

mergem<- mergem %>% 
  mutate(EDO = str_sub(folioviv, 1, 2)) 

# Modelo lineal de desigualdad de oportunidades ----

###Regresiones por entidad federativa en la base de los padres ----

by_entidad <- mergep %>% 
  group_by(EDO) %>% 
  nest()
by_entidad 

#Funcion de la regresion para padres de todas las entidades federativas


model_entidad  <- function(df) {
  lm(IE.h ~ IE.p, data = df)
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


#Grafica de R cuadrada por entidad federativa para padres 
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
       subtitle = "Esta grafica muestra la desigualdad de oportunidades en Mexico",
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


#Grafica de coeficientes e interceptos por entidad federativa para padres 

coeff_p %>% 
  filter(term == "IE.p") %>% 
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
       subtitle = "Esta grafica muestra la persistencia de la desigualdad de oportunidades por los padres",
       x = "Entidad federativa",
       y = "Estimacion",
       caption = "Fuente: Elaborado por Sergio Parra con datos de la ENIGH.") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1,# just "horizontal" de texto 
                                   vjust = 0.5))



#ggcoef(tail(broom::tidy(models, conf.int = TRUE)))
#


#Prueba para ver si las regresiones se hicieron correctamente 
edo05<- mergep %>% 
  filter(EDO == "05")

linearMod_hp_edo05 <- lm(IE.h ~ IE.p , data=edo05)
print(linearMod_hp_edo05)
stargazer(linearMod_hp_edo05)


ggcoef(linearMod_hp_edo05, exponentiate = FALSE, exclude_intercept = TRUE)

ic <- sapply(models, function(x) exp(c( coef(x)[2], confint(x)[2,]) ) )

###Regresiones por entidad federativa en la base de las madres ----


by_entidad_m <- mergem %>% 
  group_by(EDO) %>% 
  nest()
by_entidad_m

#Funcion de la regresion para padres de todas las entidades federativas

model_entidad_m  <- function(dfm) {
  lm(IE.h ~ IE.m, data = dfm)
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


#Grafica de R cuadrada por entidad federativa para madres 
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
  labs(title = "Desigualdad de oportunidades por entidad federativa",
       subtitle = "Esta grafica muestra la desigualdad de oportunidades en Mexico",
       x = "Entidad federativa",
       y = "R cuadrada",
       caption = "Fuente: Elaborado por Sergio Parra con datos de la ENIGH.") +
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
  filter(term == "IE.m") %>% 
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
       caption = "Fuente: Elaborado por Sergio Parra con datos de la ENIGH.") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1,# just "horizontal" de texto 
                                   vjust = 0.5))


### Variaciones entre madres, padres, hijas e hijos a nivel nacional ----
#.h es el IE de los hijos, .m es el IE de las madres, .p es el IE de los padres

#Modelo interactuado padres 
lm_interaccion <- lm(IE.h ~ IE.p + (sexo.h*IE.p), data = mergep )
print(lm_interaccion)
stargazer(lm_interaccion)

#Modelo interactiado para las madres

lm_interaccion_m <- lm(IE.h ~ IE.m + (sexo.h*IE.m), data = mergem )
print(lm_interaccion_m)
stargazer(lm_interaccion_m)

### Estadistica descriptiva y graficas ----

#Escolaridad mayores de 25 anios en Mexico 
poblacion10 %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = Esc)) +
  theme(axis.text.x = element_text(angle = 90))


poblacion10 %>% 
  mutate(Grado = case_when(Esc == 0 ~ "Nulo",
                           Esc == 1 ~ "Primaria", 
                           Esc == 2 ~ "Primaria",
                           Esc == 3 ~ "Primaria",
                           Esc == 4 ~ "Primaria",
                           Esc == 5 ~ "Primaria",
                           Esc == 6 ~ "Primaria",
                           Esc == 7 ~ "Secundaria",
                           Esc == 8 ~ "Secundaria",
                           Esc == 9 ~ "Secundaria",
                           Esc == 10 ~ "Preparatoria",
                           Esc == 11 ~ "Preparatoria",
                           Esc == 12 ~ "Preparatoria",
                           Esc == 13 ~ "Profesional",
                           Esc == 14 ~ "Profesional",
                           Esc == 15 ~ "Profesional",
                           Esc == 16 ~ "Profesional",
                           Esc == 17 ~ "Profesional",
                           Esc == 18 ~ "Maestria",
                           Esc == 19 ~ "Maestria",
                           Esc == 20 ~ "Doctorado",
                           Esc == 21 ~ "Doctorado",
                           Esc == 22 ~ "Doctorado")) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = Esc, y = (..count..)/sum(..count..),colour = Grado), alpha=.03) + #Para quitar porcentaje solo eliminar operacion de y
  labs(title = "Escolaridad para mayores de 25 anios",
       subtitle = "Esta grafica muestra el logro educativo para personas mayores de 25 anios",
       x = "Anios de logro educativo",
       y = "Poblacion",
       colour = "Grado",
       caption = "Fuente: Elaborado por Sergio Parra con datos del INEGI.") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1,# just "horizontal" de texto 
                                   vjust = 0.5))


summary(poblacion10$Esc, na.rm = TRUE)


# Modelo de crecimiento y desigualdad de oportunidades ----

PIBEstados <- read_excel("Desktop/PIBEstados.xlsx") 


#Promedio crecimiento 2010-2017
logprom_10_17 <- PIBEstados %>% 
  gather(key = entidad,
         value = valor, 
         -Ano) %>% 
  filter(!str_detect(entidad, "Unidos"),
         Ano >= 2010) %>% 
  mutate(valor_lead_1 = lead(valor),
         valor_lead_2 = lead(valor, n = 2),
         valor_lead_3 = lead(valor, n = 3),
         valor_lead_4 = lead(valor, n = 4),
         valor_lead_5 = lead(valor, n = 5),
         valor_lead_6 = lead(valor, n = 6),
         valor_lead_7 = lead(valor, n = 7),
         log_promedio = ((log10(valor_lead_7) - log10(valor_lead_1))/7)*100) 


#Nos quedamos solo con 2010
crec2010<- logprom_10_17 %>% 
  filter(Ano == 2010) %>% 
  select(Ano, entidad, log_promedio) %>% 
  mutate(EDO = 1:32)

###Modelo de desigualdad y crecimiento para los padres ----

#Con coeficientes de desigualdad de oportunidades para padres 
coeff_p <- coeff_p %>% 
  filter(term == "IE.p") %>% 
  mutate(EDO = as.numeric(EDO))

io_p<- merge(crec2010, coeff_p, by = "EDO")

i_op<- lm(log_promedio ~ estimate , data=io_p)
stargazer(i_op)

#Con R cuadrada para padres 

rsquared_p$EDO<-as.numeric(rsquared_p$EDO)

io_p_r <- merge(crec2010, rsquared_p, by = "EDO")

iop_r <- lm(log_promedio ~ r.squared, data = io_p_r)
stargazer(iop_r)

###Modelo de desigualdad y crecimiento para las madres  ----

#Con coeficientes de desigualdad de oportunidades para las madres 
coeff_m <- coeff_m %>% 
  filter(term == "IE.m") %>% 
  mutate(EDO = as.numeric(EDO))

io_m<- merge(crec2010, coeff_m, by = "EDO")

i_om<- lm(log_promedio ~ estimate , data=io_m)
stargazer(i_om)


#Con R cuadradas para las madres 

rsquared_m$EDO<-as.numeric(rsquared_m$EDO)

io_m_r <- merge(crec2010, rsquared_m, by = "EDO")

iom_r  <- lm(log_promedio ~ r.squared, data = io_m_r)
stargazer(iom_r)


