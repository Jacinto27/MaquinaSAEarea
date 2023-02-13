################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Returns:      Estimación de Horvitz Thompson para los dominios             ##
## Author:       Joel Mendez - Carlos Pena - Stalyn Guerrero- Andrés Gutiérrez##
## Date:         01-2023                                                      ##
## El código es empleado para estimar el modelo de área de FH                 ##
################################################################################

###--- Limpieza de memoria ---###
# .libPaths('C:/R packs')
rm(list = ls())
gc()

#######################
###--- Librerías ---###
#######################

########### CARGANDO PAQUETES (INSTALA AUTOMATICAMENTE SI NO ESTAN EN EL COMPUTADOR)
lista_paquetes <-
  c(
    "survey",
    "srvyr",
    "TeachingSampling",
    "stringr",
    "magrittr",
    "sae",
    "ggplot2",
    "emdi",
    "patchwork",
    "readxl",
    "dplyr"
  )

lapply(
  lista_paquetes,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



rm(lista_paquetes)

#-------------------- Variables + estimación directa + FGV --------------------#
## Renombrado variables y haciendo el cambio de escala
base_FH <- readRDS('Data/base_FH.Rds') #Estimación Directa + FGV
auxiliar_org <- readRDS("Data/auxiliar_org.Rds") %>%
  mutate_at(
    .vars = c(
      "F182013_stable_lights",
      "X2016_crops.coverfraction",
      "X2016_urban.coverfraction",
      "accessibility",
      "accessibility_walking_only"
    ),
    function(x) as.numeric(scale(x))
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
  mutate(id_municipio = as.numeric(Cod))

#------Breve edicion de datos----
auxiliar_org %<>% mutate(id_municipio = as.character(id_municipio))

base_completa <- base_FH %>%
  dplyr::select(
    -c('Area_km', 'ZONA_Rur', 'Densidad_Pob')
  ) %>% full_join( auxiliar_org, by = 'id_municipio')


base_completa$id_municipio <- as.numeric(base_completa$id_municipio)
base_completa <- left_join(base_completa, DEE_Mun, by='id_municipio')

saveRDS(base_completa, 'Data/base_completa.Rds')

#------------------------------------------------------------------------------#
#-------------- Modelo Fay - Herriot con transformación arcoseno --------------#
#------------------------------------------------------------------------------#


# emdi::step() Función para selección de variables
## validacion del modelo 

fh_arcsin <- fh(
  fixed = Rd ~ 0 +P45_TUVO_EMPLEO  +  P46_ACTIVIDAD_PORPAGA  + 
    P47_AYUDO_SINPAGA  +  P30_DONDE_NACE  +  P41_ANOS_UNIVERSITARIOS  + 
    P38_ANOEST  +  P44_PAIS_VIVIA_CODIGO  +  P50_DISPUESTO_TRABAJAR  + 
    P51_TRABAJO_ANTES  +  P40_SEGRADUO  +  P29_EDAD_ANOS_CUMPLIDOS  + 
    P35_SABE_LEER  +  P27_SEXO  +  H25C_TOTAL  +  P45R1_CONDICION_ACTIVIDAD_OCUPADO  + 
    P45R1_CONDICION_ACTIVIDAD_DISCAPACITADO  +  P45R1_CONDICION_ACTIVIDAD_1erTrabajo  + 
    P45R1_CONDICION_ACTIVIDAD_EDUCACION  + P54R1_CATOCUP_SINCOM +
    P27_SEXO_JEFE + P29_EDAD_JEFE + ZONA_Rur + H31_HACINAMIENTO + H15_PROCEDENCIA_AGUA +
    H17_ALUMBRADO + H12_SANITARIO + V03_PAREDES + V04_TECHO + V05_PISO +
    H32_GRADSAN + H35_GRUPSEC + Area_km
    +  luces_nocturnas  +cubrimiento_rural 
    +  cubrimiento_urbano + accesibilidad_hospitales +Num,
  vardir = "hat_var",
  combined_data = base_completa %>% data.frame(),
  domains = "id_municipio",
  method = "reml",
  transformation = "arcsin",
  backtransformation = "bc",
  eff_smpsize = "n_eff_FGV",
  MSE = TRUE,
  mse_type = "boot",
  B = 500
)

#wprint(fh_arcsin)
# var_u = 0.00024861 #
summary(fh_arcsin)

saveRDS(fh_arcsin, 'Data/fh_arcsin.Rds')

# ################
# # Estimaciones #
# ################

estimaciones <- estimators(fh_arcsin,
                           indicator = "All",
                           MSE = TRUE,
                           CV = TRUE) %>%
  as.data.frame() %>%
  left_join(base_completa %>%
              transmute(id_municipio,
                        n = ifelse(is.na(n), 0, n)),
            by = c("Domain" = "id_municipio")) %>%
  left_join(fh_arcsin$model$gamma,
            by = "Domain") %>%
dplyr::select(Domain, everything())

#----------- Exportando salidas: estimaciones del modelo FH ajustado ----------#
saveRDS(estimaciones, 'Data/estimaciones.Rds')

