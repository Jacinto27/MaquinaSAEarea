################################################################################
## Title:        Benchmark del Modelo Fay Herriot                             ##
## Returns:      Estimaciones del Modelo Fay Herriot con Benchmark            ##
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
.libPaths('C://R pack')
rm(list = ls())
gc()

#######################
###--- Librerías ---###
#######################

library(survey)
library(srvyr)
library(TeachingSampling)
library(stringr)
library(magrittr)
library(sae)
library(ggplot2)
library(emdi)
library(patchwork)
library(DBI)
library(odbc)
library(flexmix)
library(modeltools)
library(sp)
library(sf)
library(rgdal)
library(tmap)
library(dplyr)
# dplyr::select <- dplyr::dplyr::select

################################################################################
#      Bases de datos: CASEN, Variables auxiliares + estimación modelo FH      #
################################################################################

#--------------- Variables auxiliares + estimación directa + FGV --------------

poligonos_comuna <- readOGR( "\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script4\\Data\\shapefiles2010\\MUNCenso2010.shp" )
poligonos_provincia <- readOGR( " \\\\10.0.2.93 \\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script4\\Data\\shapefiles2010\\PROVCenso2010.shp" )

###    Se excluyen algunas comunas que no tienen proximidad para el modelo   ###
###                    (Islas: Pascua y Juan Fernandez)                      ###

#poligonos_comuna <-poligonos_comuna[!poligonos_comuna$cod_comuna %in% c("0"), ]
poligonos_comuna$ENLACE <- substring(poligonos_comuna$ENLACE, 3)
poligonos_comuna$ENLACE <- as.numeric(poligonos_comuna$ENLACE)



#--------------------------------------------#
base_completa <- readRDS('\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script3\\Data\\base_completa.Rds')
estimacion <- readRDS('\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script3\\Data\\estimaciones.Rds')
base_FH <- left_join(base_completa,estimacion, by=c('id_municipio'='Domain'))

#--------------------------------- ENSIN 2015 ---------------------------------#

encuesta <-
  readRDS("\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script1\\Data\\encuestaDOM.Rds")


#------ Estimaciones del modelo ajustado: FH con transformación arcoseno ------#

estimacionesPre <-
  readRDS("\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script3\\Data\\estimaciones.Rds")
fh_arcsin <-
  readRDS("\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script3\\Data\\fh_arcsin.Rds")

#------------------------ Tamaño poblacional por municipio -----------------------#

# con <-  dbConnect( odbc(),
#                    Driver = "SQL Server",
#                    Server = "IP_SERVER\\SQLSERVER_DEV",
#                    Database = "SME",
#                    Trusted_Connection = "True" )
# 
# encuestaDOM <- dbGetQuery( con,"
# select distinct count(*) over (partition by cast(IIF(MUNICIPIO<10,
# CAST(PROVINCIA as nvarchar)+'0'+cast(MUNICIPIO  as nvarchar),
# CAST(PROVINCIA as nvarchar)+cast(MUNICIPIO  as nvarchar)) as FLOAT) ) as 'total_mun'
# ,cast(IIF(MUNICIPIO<10,
# CAST(PROVINCIA as nvarchar)+'0'+cast(MUNICIPIO  as nvarchar),
# CAST(PROVINCIA as nvarchar)+cast(MUNICIPIO  as nvarchar)) as FLOAT) as 'id_municipio'
# ,(case when REGION between 1 and 4  then 2
#  when REGION between 5 and 7 then 3
#  when REGION between 8 and 9 then 4
#  when REGION=10 then 1 else null end ) as 'grupo_region'
# from CENSO_RD_2010_PERSONAS
# " )
# dbDisconnect(con)
# rm(con)
# 
# saveRDS(encuestaDOM, '\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script5\\encuestaDOM.Rds')
encuestaDOM <- readRDS('\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script5\\encuestaDOM.Rds')


################################################################################
#----- Benchmark regional para las estimaciones SAE del modelo Fay-Herriot ----#
################################################################################
#a <- case_when(encuesta$orden_sector_dic==1 & encuesta$ocupado==1 & encuesta$pet==1 ~ 1,encuesta$ocupado==1 & encuesta$pet==1 ~ 0, T~NA_real_)
#unique(a)
#b <- case_when(is.na(a)~NA_real_, T~encuesta$factor_expansion)
#unique(b)
#--- Estimación directa por depto ---#
encuesta <-
  encuesta %>% 
  mutate(upm = as.character(upm), estrato = as.character(estrato),factor_anual=factor_expansion/4)

disenoDOM <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = factor_anual,
    nest=T
  )

directoDepto <- disenoDOM %>% 
  group_by(grupo_region,orden_region) %>%filter(ocupado == 1 & pet == 1)%>%
  summarise(Rd = survey_ratio(numerator = orden_sector==2 ,
                              denominator = 1,
                              vartype = c("se", "ci", "var", "cv"),deff=T)) %>% ##ARREGLAR VALOR AQUI
  transmute(grupo_region,
    orden_region2=as.character(orden_region),
            theta_depto = Rd)

#--- Tamaño poblacional por depto ---#
# con <-  dbConnect( odbc(),
#                    Driver = "SQL Server",
#                    Server = "IP_SERVER\\SQLSERVER_DEV",
#                    Database = "SME",
#                    Trusted_Connection = "True" )
# 
# encuestaDOMRegion <- dbGetQuery( con,"
# select count(*) as 'hh_depto'
# ,(case when REGION between 1 and 4  then 2
#  when REGION between 5 and 7 then 3
#  when REGION between 8 and 9 then 4
#  when REGION=10 then 1 else null end ) as 'grupo_region'
# from CENSO_RD_2010_PERSONAS group by (case when REGION between 1 and 4  then 2
#  when REGION between 5 and 7 then 3
#  when REGION between 8 and 9 then 4
#  when REGION=10 then 1 else null end )
# " )
# 
# dbDisconnect(con)
# rm(con)
# 
# saveRDS(encuestaDOMRegion,'\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script5\\encuestaDOMRegion.Rds')
encuestaDOMRegion <- readRDS('\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\SAERD-CPL-MEPyD\\Script5\\encuestaDOMRegion.Rds')

encuestaDOMRegion$orden_region <-as.character(encuestaDOMRegion$grupo_region)
directoDepto$orden_region <- as.character(directoDepto$orden_region2)
encuestaDOM$orden_region <-as.character(encuestaDOM$grupo_region)
estimacionesPre$Domain <-as.character(estimacionesPre$Domain)
encuestaDOM$id_municipio <-as.character(encuestaDOM$id_municipio)

#-- Consolidación BD: Región, Comuna, estimación región, estimación FH comuna -#

R_mpio <- directoDepto %>% 
  left_join(encuestaDOM, by = 'orden_region') %>%
  left_join(encuestaDOMRegion, by = "orden_region") %>%
  left_join(estimacionesPre %>% 
              transmute(Domain, FayHerriot = FH),
            by = c("id_municipio"='Domain')) %>% 
  mutate(hh_mpio = total_mun)

#encuestaDOMRegion$hh_depto <- encuestaDOMRegion$`Total por region`
#------------------------------- Pesos Benchmark ------------------------------#

R_mpio2 <- R_mpio %>% 
  group_by(orden_region) %>% 
  summarise(
    R_depto_RB = unique(theta_depto) / sum((hh_mpio  / hh_depto) * FayHerriot),
    R_depto_DB = unique(theta_depto) - sum((hh_mpio  / hh_depto) * FayHerriot)
  ) %>%
  left_join(directoDepto, by = "orden_region")

pesos <- R_mpio %>% 
  mutate(W_i = hh_mpio / hh_depto) %>% 
  dplyr::select(id_municipio, W_i)

#--------------------------- Estimación FH Benchmark --------------------------#

estimacionesBench <- estimacionesPre %>%
  left_join(R_mpio %>% 
              dplyr::select(orden_region, id_municipio), by = c('Domain'="id_municipio")) %>%
  left_join(R_mpio2, by = c("orden_region")) %>%
  mutate(FH_RBench = R_depto_RB * FH) %>%
  left_join(pesos, by = c("Domain" = "id_municipio"))

#------------------- Validación: Estimación FH con Benchmark ------------------#

#--- Comparación entre el valor reportado y el valor estimado FH Benchmark ---#

estimacionesBench %>% group_by(orden_region) %>%
  summarise(theta_reg_RB = sum(W_i * FH_RBench)) %>%
  left_join(directoDepto, by = "orden_region") %>% 
  View()

#--- Comparación entre estimación FH y FH con Benchmark por comuna ---#

View(
  estimacionesBench %>%
    transmute(
      Domain,
      Directo = Direct * 100,
      FayHerriot = FH * 100,
      FH_RBench = FH_RBench * 100,
      Gamma
    )
)


abcdef <- estimacionesBench %>%
  transmute(
    Domain,
    Directo = Direct * 100,
    FayHerriot = FH * 100,
    FH_RBench = FH_RBench * 100,
    Gamma)


#--- Comparación gráfica entre estimación FH y FH con Benchmark por comuna ---#

dev.off()
theme_set(theme_bw())

ggplot(estimacionesBench, aes(FH_RBench, FH)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1) + 
  labs(y = "Estimación Fay-Herriot",
       x = "Estimación Fay-Herriot con Benchmark")

################################################################################
#------------------------- Tabla final de estimaciones ------------------------#
################################################################################

# Consolidación de base de datos con Código, nombre, n_muestral, Dir, ee_dir,  #
#          CV_dir, FH, MSE_FH, RRMSE, Gamma, Sintetico, FH_benchmark           #
#Función ICL Custom ##
ICL <- function(p, mse, alpha = 0.05, student = FALSE, nu = NULL) {
  if (student == TRUE) {
    q <- qt(1 - alpha/2, nu)
  } else {
    q <- qnorm(1 - alpha/2)
  }
  CL <- log(p/(1 - p)) - (q * sqrt(mse))/(p * (1 - p))
  CU <- log(p/(1 - p)) + (q * sqrt(mse))/(p * (1 - p))
  IC_1 <- exp(CL)/(1 + exp(CL))
  IC_2 <- exp(CU)/(1 + exp(CU))
  
  return(data.frame(L.I = IC_1, L.S = IC_2))
}

TablaFinal <- estimacionesBench %>%
  dplyr::select(-c(grupo_region)) %>%
  mutate(
    sintetico = as.matrix(base_FH %>% data.frame() %>% 
                            dplyr::select(rownames(
                              fh_arcsin$model$coefficients
                            ))) %*%
      fh_arcsin$model$coefficients[, 1],
    sintetico_back = sin(sintetico) ^ 2
  ) %>%
  transmute(
    Codigo = Domain,
    n_muestral = n,
    Directo = Direct,
    ee_directo = sqrt(Direct_MSE),
    CV_directo = Direct_CV,
    FayHerriot = FH,
    rmse_FH = sqrt(FH_MSE),
    rrmse_FH = rmse_FH / FayHerriot,
    Gamma,
    sintetico,
    sintetico_back,
    FH_RBench,
    LI_normal = FH_RBench - 1.96 * sqrt(FH_MSE),
    LS_normal = FH_RBench + 1.96 * sqrt(FH_MSE),
    LI_logit = ICL(FH_RBench, FH_MSE)[, 1],
    LS_logit = ICL(FH_RBench, FH_MSE)[, 2],
    LI_final = ifelse(LI_normal < 0, LI_logit, LI_normal),
    LS_final = ifelse(LS_normal > 1, LS_logit, LS_normal)
  )


###--------------------------- Comparación gráfica --------------------------###

a1 <- ggplot(TablaFinal, aes(x = LI_normal, y = LI_logit)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1), col = 2)  +
  labs(y = "LI_logit", x = "LI")

a2 <- ggplot(TablaFinal, aes(x = LS_normal, y = LS_logit)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1), col = 2)  +
  labs(y = "LS_logit", x = "LS")

###-------------------------- Gráfico con patchwork -------------------------###

a1 | a2

#----------- Exportando salidas: estimaciones del modelo FH ajustado ----------#

saveRDS(TablaFinal, 
        '\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script4\\TablaFinal.Rds')
saveRDS(estimacionesBench, 
        '\\\\10.0.2.93\\Monitoreo y Evaluación\\Proyecto Estimación de Áreas Pequeñas\\Informalidad\\Script4\\estimacionesBench.Rds')



poligonos_municipio <- poligonos_comuna
poligonos_municipio$ENLACE <- as.numeric(poligonos_municipio$ENLACE) 
abcdef$Domain <- as.numeric(abcdef$Domain)
poligonos_municipio@data <- poligonos_municipio@data %>%
  left_join( abcdef, by = c( "ENLACE"="Domain" ) ) %>%
  mutate( fh_porc = FH_RBench )

mapa <- tm_shape( poligonos_municipio ) +
  tm_fill( "fh_porc", style = "quantile", title="Tasa de informalidad" ) +
  tm_borders( col = "black", lwd=1, lty = "solid") +
  tm_layout( #"Wealth (or so)",
    legend.title.size = 1,
    legend.text.size = 0.6,
    legend.position = c( "center","bottom" ),
    legend.bg.color = "white",
    legend.stack = "horizontal",
    #legend.digits = 5,
    legend.bg.alpha = 0.1) +
  tm_shape( poligonos_provincia ) +
  tm_borders( col = "black", lwd = 3, lty = "solid") 
tm_legend(position=c("left", "bottom"), bg.color="grey95", frame=TRUE)

