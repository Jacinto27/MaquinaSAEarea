################################################################################
## Title:       Modelo Fay Herriot para estimaciones directas utilizando      ##
##              transformación arcoseno y FGV                                 ##
## Returns:     Unificando codigos de la encuesta, shapefile, información aux ##
## Author:      Stalyn Guerrero - Joel Mendez - Carlos Pena - Andrés Gutiérrez##
## Date:        01-2023                                                       ##
################################################################################
rm(list = ls())

library(tidyverse)
library(magrittr)
library(stringr)
library(readxl)
library(tmap)
library(sp)
library(sf)
setwd("Depurar_rutinas/")
## Lectura de encuesta. 
encuestaDOM <-  readRDS("Data/encuestaDOM.Rds") 

encuestaDOM %<>% 
  mutate(id_dominio = str_pad(string = id_municipio, width = 4, pad = "0"),
         id_region = str_pad(string = orden_region, width = 2, pad = "0"))

## Lectura de información auxiliar (covariable)
auxiliar_org <-  readRDS("Data/auxiliar_org.Rds")  %>%
  mutate(id_dominio = str_pad(
    string = id_municipio,
    width = 4,
    pad = "0"
  ))


auxiliar_satelital <-  readRDS("Data/auxiliar_satelital.Rds") %>%
  mutate_if(is.numeric, function(x)as.numeric(scale(x))) %>%  mutate(
    ENLACE = substring(ENLACE, 3),
    id_dominio = str_pad(
      string = ENLACE,
      width = 4,
      pad = "0"
    )
  )

statelevel_predictors <- inner_join(auxiliar_org, auxiliar_satelital, 
                                    by = "id_dominio")

#Variables 
DEE_Mun <- read_xlsx('Data/datos del DEE ONE agrupdos por municipios.xlsx') %>% 
  mutate(id_dominio = str_pad(
    string = Cod,
    width = 4,
    pad = "0"
  ), Cod =NULL)

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

## Validando id_dominio
encuestaDOM %>% distinct(id_dominio, DES_PROVINCIA) %>%
  full_join(statelevel_predictors %>% distinct(id_dominio))

encuestaDOM %>% distinct(id_dominio, DES_PROVINCIA) %>%
  full_join(DEE_Mun %>% distinct(id_dominio, Des))

encuestaDOM %>% distinct(id_dominio, DES_PROVINCIA) %>%
  full_join(Shapefile %>% data.frame() %>% select(id_dominio))


## Actualizando los archivos 
saveRDS(encuestaDOM, file = "Data/encuestaDOM.Rds")
saveRDS(statelevel_predictors, file = "Data/statelevel_predictors_df.rds")
saveRDS(DEE_Mun, file = "Data/DEE_Mun.Rds")
st_write(obj = Shapefile,"shapefiles2010/DOM.shp")


### agregados municipales censales. 
agregado_region <- readRDS("Data/encuestaDOMRegion.Rds") %>% 
  transmute(pp_region = hh_depto,
         id_region = str_pad(string = grupo_region, width = 2,
                             pad = "0"))

saveRDS(agregado_region, file = "Data/agregado_persona_region.rds")

readRDS("Data/personas_dominio.Rds") %>% 
  mutate( id_region = str_pad(string = orden_region, width = 2,
                              pad = "0"),
          orden_region = NULL,
          pp_dominio = total_pp,
          total_pp  = NULL,
          id_municipio = NULL) %>% 
  saveRDS(file = "Data/agregado_persona_dominio.rds")  





