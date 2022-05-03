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
dat <- rename(dat, cuidadora = T_CUIDO12MASDEPENT_PARTICIPACION)

##Variables independientes

##Sexo

dat <- dat %>% mutate(
 mujer = ifelse(A4 == 2, 1, 0)
)

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
dat <- rename(dat, empleo = E1)

#Educación
dat <- rename(dat, educacion = A14)

dat$educacion <- case_when(
  dat$educacion == 0 | dat$educacion == 2 | dat$educacion == 11 | dat$educacion == 12 | dat$educacion == 13 |
  dat$educacion == 14 | dat$educacion == 15 | dat$educacion == 16 | dat$educacion == 17 | dat$educacion == 18 ~ 1,
  dat$educacion == 21 | dat$educacion == 22 | dat$educacion == 23 | dat$educacion == 24 | dat$educacion == 25 |
  dat$educacion == 26 | dat$educacion == 31 | dat$educacion == 32 | dat$educacion == 33 | dat$educacion == 34 |
  dat$educacion ==35 | dat$educacion == 36 | dat$educacion == 37 ~ 2,
  dat$educacion == 41 | dat$educacion == 42 | dat$educacion == 43 | dat$educacion == 51 | dat$educacion == 52 |
  dat$educacion == 53 | dat$educacion == 54 | dat$educacion == 55 | dat$educacion == 56 | dat$educacion == 71 |
  dat$educacion == 72 | dat$educacion == 74 | dat$educacion == 81 | dat$educacion == 82 | dat$educacion ==83 |
  dat$educacion == 84 | dat$educacion == 85 | dat$educacion == 86 ~ 3
)

dat$educacion <-  factor(dat$educacion,
                         levels = c(1,2,3),
                         labels = c("Primaria", "Secundaria", "Universitaria"))

##Relación de parentezco

dat <- rename(dat, parentezco = A3)

dat$parentezco <- case_when(
  dat$parentezco == 1 ~ 1,
  dat$parentezco == 2 ~ 2,
  dat$parentezco == 3 | dat$parentezco == 4 | dat$parentezco == 5 | dat$parentezco == 7 | dat$parentezco == 8 ~ 3,
  dat$parentezco == 6 | dat$parentezco == 9 | dat$parentezco == 10 ~ 4,
  dat$parentezco == 11 ~ 5,
  dat$parentezco == 12 ~ 6
)

dat$parentezco <- factor(dat$parentezco,
                         levels = c(1,2,3,4,5,6),
                         labels = c("Jefe", "Esposo/a / Pareja", "Primer grado", "Segundo grado", "Otros parientes", "Sin parentezco"))

##Cash transfer

dat <- rename(dat, cash_transfer = E20_4)

##Estado civil

dat <- rename(dat, estadocivil = A6)

dat$estadocivil <- case_when(
  dat$estadocivil == 1 | dat$estadocivil == 2 ~ 1,
  dat$estadocivil == 3 | dat$estadocivil == 4 ~ 2,
  dat$estadocivil == 5 ~ 3,
  dat$estadocivil == 6 ~ 4
)

dat$estadocivil <- factor(dat$estadocivil,
                          levels = c(1,2,3,4),
                          labels = c("Casados o unidos", "Divorciados o separados", "Viudos", "Solteros"))

##Modelo lineal

lm_cr1 <- lm(as.numeric(cuidadora) ~ mujer + Edad_Recodificada + educacion + empleo,
            data = dat)

lm_cr2 <- lm(as.numeric(cuidadora) ~ mujer + Edad_Recodificada + educacion + empleo + cash_transfer + rural + ingresos + parentezco + estadocivil,
            data = dat)

##Modelo Probit

probit_cr1 <- glm(cuidadora ~ mujer + Edad_Recodificada + educacion + empleo,
                 family = binomial(link = "probit"),
                 data = dat)

probit_cr2 <- glm(cuidadora ~ mujer + Edad_Recodificada + educacion + empleo + cash_transfer + rural + ingresos + parentezco + estadocivil,
                 family = binomial(link = "probit"), 
                 data = dat)

stargazer(lm_cr1, lm_cr2, probit_cr1, probit_cr2,
          type = "html",
          omit = c("Constant"),
         out = "C:/Users/jeffc/Desktop/Alex/Costa Rica/ENUT2017/ReporteCR.html")
