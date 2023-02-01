################################################################################
## Title:       Modelo Fay Herriot para estimaciones directas utilizando      ##
##              transformación arcoseno y FGV                                 ##
## Returns:     Unificando codigos de la encuesta, shapefile, información aux ##
## Author:      Stalyn Guerrero - Joel Mendez - Carlos Pena - Andrés Gutiérrez##
## Date:        01-2023                                                       ##
################################################################################
setwd("D:/CEPAL/MaquinaSAEarea/Automatizar")
rm(list = ls())

library(tidyverse)
library(magrittr)
library(stringr)
library(readxl)
library(tmap)
library(sp)
library(sf)

## Lectura de encuesta. 
encuestaDOM <-  readRDS("Data/encuestaDOM.Rds")

encuestaDOM %<>% 
  mutate(id_dominio = str_pad(string = id_municipio, width = 4, pad = "0"),
         orden_region = str_pad(string = orden_region, width = 2, pad = "0"))

## Lectura de información auxiliar (covariable)
auxiliar_org <-  readRDS("Data/auxiliar_org.Rds")

auxiliar_org %<>%
  mutate(id_dominio = str_pad(
    string = id_municipio,
    width = 4,
    pad = "0"
  )) %>%
  mutate_at(
    .vars = c(
      "F182013_stable_lights",
      "X2016_crops.coverfraction",
      "X2016_urban.coverfraction",
      "accessibility",
      "accessibility_walking_only"
    ),
    scale
  ) %>%
  rename(
    luces_nocturnas = F182013_stable_lights,
    cubrimiento_rural = X2016_crops.coverfraction,
    cubrimiento_urbano = X2016_urban.coverfraction,
    accesibilidad_hospitales = accessibility,
    accesibilidad_hosp_caminado = accessibility_walking_only
  )
#Variables 
DEE_Mun <- read_xlsx('Data/datos del DEE ONE agrupdos por municipios.xlsx') %>% 
  mutate(id_dominio = str_pad(
    string = Cod,
    width = 4,
    pad = "0"
  ))

# Shapefile municipios

Shapefile <- read_sf( "shapefiles2010/MUNCenso2010.shp" ) %>% 
  mutate(
    ENLACE = substring(ENLACE,3),
    id_dominio = str_pad(
      string = ENLACE,
      width = 4,
      pad = "0"
    )
  )  %>% select(id_dominio)  
  
#st_write(obj = .,"shapefiles2010/DOM.shp")

## Validando id_dominio

encuestaDOM %>% distinct(id_dominio,DES_PROVINCIA) %>% 
  full_join(auxiliar_org %>% distinct(id_dominio))

encuestaDOM %>% distinct(id_dominio,DES_PROVINCIA) %>% 
  full_join(DEE_Mun %>% distinct(id_dominio,Des))

encuestaDOM %>% distinct(id_dominio,DES_PROVINCIA) %>% 
  full_join(Shapefile %>% data.frame() %>% select(id_dominio))


## Actualizando los archivos 
saveRDS(encuestaDOM, file = "Data/encuestaDOM.Rds")
saveRDS(auxiliar_org, file = "Data/statelevel_predictors_df.rds")
saveRDS(DEE_Mun, file = "Data/DEE_Mun.Rds")
st_write(obj = Shapefile,"shapefiles2010/DOM.shp")








