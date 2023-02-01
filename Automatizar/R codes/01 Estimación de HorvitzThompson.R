################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Returns:      Estimación de Horvitz Thompson para los dominios             ##
## Author:       Stalyn Guerrero - Joel Mendez - Carlos Pena - Andrés Gutiérrez##
## Date:         01-2023                                                         ##
################################################################################

#Librerias--------------------------------------------------------------------------------------------
#.libPaths('C:/R packs')
rm(list = ls())

library(survey)
library(tidyverse)
library(srvyr)
library(TeachingSampling)
library(haven)
library(readxl)
library(dplyr)
library(DBI)
library(odbc)

# Lectura de las bases de datos ------------------------------------------------
#-------------------------------------------------------------------------------
# Población dividida entre cuatro, ESTA FORMA PORQUE LA ENCUESTA 
# ES TRIMESTRAL, Y SU AGRUPACIÓN ANUAL IMPLICA QUE EL FACTOR EXPANSIÓN SEA 
# DIVIDIDO ENTRE 4 
#-------------------------------------------------------------------------------
setwd("Automatizar")
encuestaDOM <-  readRDS("Data/encuestaDOM.Rds")
## 
sum((encuestaDOM$factor_expansion)/4)

encuestaDOM <-
  encuestaDOM %>%
  mutate(
    upm = str_pad(string = upm,width = 9,pad = "0"),
    estrato = str_pad(string = estrato,width = 5,pad = "0"),
    factor_anual = factor_expansion / 4
  )

#Creación de objeto diseno--------------------------------------- 
#Diseños muestrales encuestas complejas TRIMESTRALES
options(survey.lonely.psu= 'adjust' )
disenoDOM <- encuestaDOM %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = factor_anual,
    nest=T
  )


sum(weights(disenoDOM))
summary(disenoDOM)
#Cálculo del indicador ----------------
indicador_dom <-
  disenoDOM %>% group_by(id_dominio) %>% 
  filter(ocupado == 1 & pet == 1) %>%
  summarise(
    n = unweighted(n()),
    Rd = survey_ratio(
      numerator = orden_sector == 2 ,
      denominator = 1,
      vartype = c("se", "ci", "var", "cv"),
      deff = T
    )
  )

indicador_dom <-
  full_join(indicador_dom, distinct((
    encuestaDOM %>% dplyr::select(id_dominio, des_municipio)
  )), by = "id_dominio") #Unión de data con los valores calculados por SurveyRatio

#Guardar data----------------------------
saveRDS(indicador_dom,'Data/indicador_dom.Rds' )
