################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Returns:      Ajuste de Modelo Fay Herriot                                 ##
## Author:       Felipe Molina - Andrés Gutiérrez - Diego Lemus               ##
## Institution:  CEPAL                                                        ##
## División:     División de Estadísticas                                     ##
## Date:         2021                                                         ##
## Disclaimer:   Estos códigos computacionales han sido programados con el    ##
##               fin de ejemplificar las metodologías propuestas por CEPAL.   ##
##               La responsabilidad del uso de los programas recae            ##
##               completamente sobre  los funcionarios a quienes se hace      ##
##               entrega. Se exime a la CEPAL de los errores que puedan ser   ##
##               ocasionados por el uso incorrecto de estos códigos.          ##
################################################################################

###--- Limpieza de memoria ---###
# .libPaths('C:/R packs')
rm(list = ls())
gc()

#######################
###--- Librerías ---###
#######################

########### CARGANDO PAQUETES (INSTALA AUTOMATICAMENTE SI NO ESTAN EN EL COMPUTADOR)
lista_paquetes <- c( "survey", "srvyr", "TeachingSampling", "stringr", "magrittr",
                     "sae", "ggplot2", "emdi", "patchwork","readxl","dplyr" )

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


###----------------- Cargue de función de recorte de Potter -----------------###

################################################################################
#       Bases de datos: CASEN y Variables auxiliares + estimación directa      #
################################################################################

#-------------------- Variables + estimación directa + FGV --------------------#

base_FH <- readRDS('...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script2\\Data\\base_FH.Rds') #Estimación Directa + FGV
auxiliar_org <-as.data.frame(readRDS("...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script2\\Data\\auxiliar_org.Rds" )) #Variables 
DEE_Mun <- read_xlsx('...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\datos del DEE ONE agrupdos por municipios.xlsx')

#------Breve edicion de datos----
# base_FH$id_municipio <- as.numeric(base_FH$id_municipio) #Transformación del tipo de dato del id_municipio de las variables a numerico
# auxiliar_org$id_municipio <- as.numeric(auxiliar_org$id_municipio)#Transformación del tipo de dato del id_municipio de la estimacion directa y la estimación de varianza a numerico
auxiliar_org$id_municipio <- as.character(auxiliar_org$id_municipio)
base_completa <- full_join(auxiliar_org,(base_FH %>% dplyr::select(-c('Area_km','ZONA_Rur','Densidad_Pob'))), by = 'id_municipio' )
base_completa[,2:6] <- scale(base_completa[,2:6])

DEE_Mun$id_municipio <-as.numeric(DEE_Mun$Cod) 
base_completa$id_municipio <- as.numeric(base_completa$id_municipio)
base_completa <- left_join(base_completa,DEE_Mun, by='id_municipio')

saveRDS(base_completa, '...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script3\\Data\\base_completa.Rds')

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
    +  F182013_stable_lights  +X2016_crops.coverfraction
    +  X2016_urban.coverfraction + accessibility +Num,
  vardir = "hat_var",
  combined_data = base_completa,
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


# base_Nobs <- anti_join(base_completa,base_completa3, by= )

saveRDS(fh_arcsin, '...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script3\\Data\\fh_arcsin.Rds')

# #-------------------- Exportando salidas: modelo FH ajustado ------------------#
# 
# saveRDS(fh_arcsin, file = "8. Metodologia final/2020_Cepal/Output/fh_arcsin_2020.rds")
# 
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
saveRDS(estimaciones, '...\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script3\\Data\\estimaciones.Rds')

