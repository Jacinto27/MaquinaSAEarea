################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Author:       Stalyn Guerrero - Joel Mendez - Carlos Pena - Andrés Gutiérrez##
## Date:         01-2023                                                      ##
################################################################################

###--- Limpieza de memoria ---###
# .libPaths('C://R pack')
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

#--------------- Variables auxiliares + estimación directa + FGV --------------

poligonos_dominios <- read_sf( "shapefiles2010/DOM.shp") 
id_dominio <- "id_dominio"
#--------------------------------------------#
base_completa <- readRDS('Data/base_completa.Rds')
estimacion <- readRDS('Data/estimaciones.Rds')
base_FH <- left_join(base_completa, estimacion,   by = id_dominio)
encuesta <- readRDS("Data/encuestaDOM.Rds")


#------ Estimaciones del modelo ajustado: FH con transformación arcoseno ------#

estimacionesPre <- readRDS("Data/estimaciones.Rds")
fh_arcsin <- readRDS("Data/fh_arcsin.Rds")

#------------------------ Tamaño poblacional por municipio -----------------------#
personas_dominio <- readRDS('Data/agregado_persona_dominio.rds') 
personas_dominio_agregado <- readRDS('Data/agregado_persona_region.rds') 

################################################################################
#----- Benchmark regional para las estimaciones SAE del modelo Fay-Herriot ----#
################################################################################
#--- Estimación directa por depto ---#
encuesta <-
  encuesta %>% 
  mutate(
    upm = str_pad(string = upm,width = 9,pad = "0"),
    estrato = str_pad(string = estrato,width = 5,pad = "0"),
    factor_anual = factor_expansion / 4
  ) %>% data.frame()

disenoDOM <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = factor_anual,
    nest=T
  )

directoDepto <- disenoDOM %>%
  group_by(id_region) %>% 
  filter(ocupado == 1 & pet == 1) %>%
  summarise(Rd = survey_ratio(
    numerator = orden_sector == 2 ,
    denominator = 1,
    vartype = c("se", "ci", "var", "cv"),
    deff = T
  )) %>% ##ARREGLAR VALOR AQUI
  transmute(id_region, theta_region = Rd)

#-- Consolidación BD: Región, Comuna, estimación región, estimación FH comuna -#

R_mpio <- directoDepto %>% 
  left_join(personas_dominio, by = 'id_region') %>%
  left_join(personas_dominio_agregado, by = "id_region") %>%
  left_join(estimacionesPre %>% 
              transmute(id_dominio, FayHerriot = FH),
            by = id_dominio) %>% 
  data.frame()

#------------------------------- Pesos Benchmark ------------------------------#

R_mpio2 <- R_mpio %>% 
  group_by(id_region) %>% 
  summarise(
    R_region_RB = unique(theta_region) / sum((pp_dominio  / pp_region ) * FayHerriot),
    R_region_DB = unique(theta_region) - sum((pp_dominio  / pp_region ) * FayHerriot)
  ) %>%
  left_join(directoDepto, by = "id_region")

pesos <- R_mpio %>%ungroup() %>% 
  mutate(W_i = pp_dominio / pp_region) %>% 
  dplyr::select(id_dominio, W_i)

#--------------------------- Estimación FH Benchmark --------------------------#

estimacionesBench <- estimacionesPre %>% 
  left_join(R_mpio %>% 
              dplyr::select(id_region, id_dominio), 
            by = id_dominio) %>%
  left_join(R_mpio2, by = c("id_region")) %>% ungroup() %>% 
  mutate(FH_RBench = R_region_RB * FH) %>%
  left_join(pesos, by = id_dominio)

#------------------- Validación: Estimación FH con Benchmark ------------------#

#--- Comparación entre el valor reportado y el valor estimado FH Benchmark ---#

estimacionesBench %>% group_by(id_region) %>%
  summarise(theta_reg_RB = sum(W_i * FH_RBench)) %>%
  left_join(directoDepto, by = "id_region") %>% 
  View()

#--- Comparación entre estimación FH y FH con Benchmark por comuna ---#

View(
  estimacionesBench %>%
    transmute(
      id_dominio,
      Directo = Direct * 100,
      FayHerriot = FH * 100,
      FH_RBench = FH_RBench * 100,
      Gamma
    )
)


temp_Bench <- estimacionesBench %>%
  transmute(
    id_dominio,
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
  mutate(
    sintetico = as.matrix(base_FH %>% data.frame() %>% 
                            dplyr::select(rownames(
                              fh_arcsin$model$coefficients
                            ))) %*%
      fh_arcsin$model$coefficients[, 1],
    sintetico_back = sin(sintetico) ^ 2
  ) %>%
  transmute(
    id_dominio,
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

saveRDS(TablaFinal, 'Data/TablaFinal.Rds')
saveRDS(estimacionesBench, 'Data/estimacionesBench.Rds')

poligonos_dominios <- poligonos_dominios %>%
  left_join( temp_Bench, by = id_dominio ) %>%
  mutate( fh_porc = FH_RBench )

plot(poligonos_dominios["fh_porc"])


tmap_options(check.and.fix = TRUE)
mapa <- tm_shape( poligonos_dominios ) +
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
  tm_shape( poligonos_provincia )
