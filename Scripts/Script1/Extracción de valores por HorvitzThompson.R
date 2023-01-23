#Autor: Carlos
#Creación: 9/21/22
#Última educióm: 15/12/22
#Este código intenta extraer la cantidad de empleados en el sector formal por trimestre, incluyedno su error estandar.

#Aquí se presenta el código de sql que se utiliza para extraer los datos utilizados en este documento a partir de la base de datos del departamento

#CONSULTA SQL --------------------------------------------------------------------------------------------
# --Esta consulta extrae los datos a nivel de persona de la ENCFT, para el año 2021, de algunas covariables con las cuales se estima la ocupación formal
# select trimestre, factor_expansion, cast(ID_PROVINCIA as nvarchar)+cast(estrato as nvarchar)+cast(zona as nvarchar) as 'estrato'
# ,cast(ID_PROVINCIA as nvarchar)+cast(estrato as nvarchar)+cast(zona as nvarchar)+cast(upm as nvarchar) as 'upm',  edad, zona, sexo, ocupado, orden_sector,
# id_municipio, nivel_ultimo_ano_aprobado, ingreso_asalariado, id_provincia,pet,
# (case
#  when ocupado=1 and orden_sector=1 and edad>=15 then 1
#  else 0
#  end) as 'orden_sector_dic'
# ,IIF(pet=0,null,(
#   case
#   when orden_sector=1 then 1
#   when orden_sector=2 then 0
#   when ocupado=0 then null
#   when orden_sector=99 then 0 end
# )) as 'orden_sector_sisdom'
# ,iif(pet=0,null,case when ocupado=1 and categoria_principal!=5 then 1 else 0 end) as 'ocup_formal_sisdom'
# ,orden_rama,
# DES_PROVINCIA,
# barrio_seccion,
# paraje,
# DES_ESTRATO,
# grupo_rama,
# des_municipio,
# grupo_region,
# orden_region
# from encft_2021_miembros

#Estos se almacenan en un archvio llamado Estimación total de empleados en formato Excel
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
# sessionInfo()
##################SESSIONINFO###########################################
# R version 4.2.1 (2022-06-23 ucrt)

# other attached packages:
# [1] readxl_1.4.1           haven_2.5.1            TeachingSampling_4.1.1 magrittr_2.0.3         srvyr_1.1.2            forcats_0.5.2         
# [7] stringr_1.4.1          dplyr_1.0.10           purrr_0.3.5            readr_2.1.3            tidyr_1.2.1            tibble_3.1.8          
# [13] ggplot2_3.3.6          tidyverse_1.3.2        survey_4.1-1           survival_3.4-0         Matrix_1.5-1          
# 
##########################Leer y guardar data###################################

#Leer datos

# con <-  dbConnect( odbc(), 
#                    Driver = "SQL Server", 
#                    Server = "IP_SERVER\\SQLSERVER_DEV", 
#                    Database = "SME", 
#                    Trusted_Connection = "True" )
# 
# encuestaDOM <- dbGetQuery( con,"
# select trimestre, factor_expansion, cast(ID_PROVINCIA as nvarchar)+cast(estrato as nvarchar)+cast(zona as nvarchar) as 'estrato'
# ,cast(ID_PROVINCIA as nvarchar)+cast(estrato as nvarchar)+cast(zona as nvarchar)+cast(upm as nvarchar) as 'upm',  edad, zona, sexo, ocupado, orden_sector,
# id_municipio, nivel_ultimo_ano_aprobado, ingreso_asalariado, id_provincia,pet,
# (case
#  when ocupado=1 and orden_sector=1 and edad>=15 then 1
#  else 0
#  end) as 'orden_sector_dic'
# ,IIF(pet=0,null,(
#   case
#   when orden_sector=1 then 1
#   when orden_sector=2 then 0
#   when ocupado=0 then null
#   when orden_sector=99 then 0 end
# )) as 'orden_sector_sisdom'
# ,iif(pet=0,null,case when ocupado=1 and categoria_principal!=5 then 1 else 0 end) as 'ocup_formal_sisdom'
# ,orden_rama,
# DES_PROVINCIA,
# barrio_seccion,
# paraje,
# DES_ESTRATO,
# grupo_rama,
# des_municipio,
# grupo_region,
# orden_region
# from encft_2021_miembros
# " )
# 
# dbDisconnect(con)
# rm(con) 
#saveRDS(encuestaDOM,"...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script1\\Data\\encuestaDOM.Rds")

# encuestaDOM <- read_excel("...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script1\\Data\\EstimaciónTotalEmpleados.xlsx", sheet = 'Hoja2')
# saveRDS(encuestaDOM,"...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script1\\Data\\encuestaDOM.Rds" )
encuestaDOM <-  readRDS("...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script1\\Data\\encuestaDOM.Rds")
options(survey.lonely.psu= 'adjust' )
#Población dividida entre cuatro, ESTA FORMA PORQUE LA ENCUESTA ES TRIMESTRAL, Y SU AGRUPACIÓN ANUAL IMPLICA QUE EL FACTOR EXPANSIÓN SEA DIVIDIDO ENTRE 4 
dim(encuestaDOM)
sum((encuestaDOM$factor_expansion)/4)
encuestaDOM <-
  encuestaDOM %>% 
  mutate(upm = as.character(upm), estrato = as.character(estrato),factor_anual=factor_expansion/4)
length(unique(encuestaDOM$estrato))

#Creación de objeto diseno--------------------------------------- 
#Diseños muestrales encuestas complejas TRIMESTRALES
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
d <-
  disenoDOM %>% group_by(id_municipio) %>%filter(ocupado == 1 & pet == 1)%>%
  summarise(n = unweighted(n()), Rd = survey_ratio(numerator = orden_sector==2 ,
                                                   denominator = 1,
                                                   vartype = c("se", "ci", "var", "cv"),deff=T))

d <- full_join(d,distinct((encuestaDOM %>% dplyr::select(id_municipio,des_municipio))), by='id_municipio' ) #Unión de data con los valores calculados por SurveyRatio

#disenoDOM$variables %>% group_by(estrato) %>% n_distinct(disenoDOM$variables$upm) #Evaluación de total de UPM por estrato

# d <-
#   disenoDOM %>% group_by(id_municipio) %>% filter(ocupado == 1 &orden_sector == 2 & pet == 1) %>%
#   summarise(n = unweighted(n()), Nd = survey_total(vartype = c("se", "ci", "var", "cv"),deff=T ))

#Guardar data----------------------------
saveRDS(d,'...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script1\\Data\\d.Rds' )
# d %>%
#   mutate(ln_sigma=log(Rd_var))

