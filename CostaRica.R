###----------------------------------------------------###
###-----Costa Rica - Encuesta de uso de tiempo---------###
###---------------------------------------------------###

rm(list = ls ())

library(readr)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(rdd)
library(dplyr)
library(tidyr)
library(stargazer)
library(haven)
library(zoo)
library(janitor)
library(reshape2)

dat <- read_sav("ENUT_2017.sav")

##Selección de variables

dat <- dat %>% select(
  zona, A3, A4, A6, A14, Edad_Recodificada, E1, E20_4, T_CUIDO12MASDEPENT_PARTICIPACION, E11_COL, E15_COL, E21_COL
)

##Variable dependiente

dat$T_CUIDO12MASDEPENT_PARTICIPACION <- factor(dat$T_CUIDO12MASDEPENT_PARTICIPACION)
rename(dat, cuidados = T_CUIDO12MASDEPENT_PARTICIPACION)

##Variables independientes

##Ingresos

ingresos <- data.frame(dat$E11_COL, dat$E15_COL, dat$E21_COL) 

dat <- dat %>% mutate(
  ingresos = (rowSums(ingresos, na.rm = TRUE))
)
dat$ingresos[dat$ingresos == 0] <- NA

#Zona
dat <- dat %>% mutate(
  rural = ifelse(zona == 2, 1, 0)
) 

#Edad
dat$Edad_Recodificada <- factor(dat$Edad_Recodificada)

#Trabajo
rename(dat, empleo = E1)

#Educación
rename(dat, educacion = A14)

# dat$educacion <- case_when(
#   dat$educacion == 
# )

