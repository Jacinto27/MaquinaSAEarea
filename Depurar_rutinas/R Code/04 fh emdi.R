################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Returns:      Estimación de Horvitz Thompson para los dominios             ##
## Author:       Stalyn Guerrero - Joel Mendez - Carlos Pena - Andrés Gutiérrez##
## Date:         01-2023                                                      ##
################################################################################


###--- Limpieza de memoria ---###
.libPaths('C:/R packs')
rm(list = ls())
gc()

#######################
###--- Librerías ---###
#######################
library(tidyverse)
library(survey)
library(srvyr)
library(TeachingSampling)
library(stringr)
library(magrittr)
library(sae)
library(ggplot2)
library(emdi)
library(patchwork)
library(reshape2)
library(haven)
library(BayesSAE)
library(mice)
library(rgdal)
library(spdep)
library(tmap)
library(sp)
library(sf)
library(dplyr)
library(magrittr)
#select <- dplyr::select

###----------------- Cargue de función de recorte de Potter -----------------###

########################
### Shape municipios ###
########################

poligonos_comuna <- read_sf( "shapefiles2010/MUNCenso2010.shp" )

###    Se excluyen algunas comunas que no tienen proximidad para el modelo   ###
###                    (Islas: Pascua y Juan Fernandez)                      ###

poligonos_comuna %<>% mutate(
  ENLACE = substring(ENLACE,3),
  ENLACE = as.numeric(ENLACE)
) 

################################################################################
#      Bases de datos: Variables auxiliares + estimación modelo FH      #
################################################################################
base_completa <- readRDS('Data/base_completa.Rds')

#-------------------- Modelo FH con transformación Arcoseno -------------------#
fh_arcsin <-  readRDS("Data/fh_arcsin.Rds")

print(fh_arcsin)
compare(fh_arcsin)

#----------- Estimaciones del modelo FH con transformación Arcoseno -----------#

estimaciones <- readRDS('Data/estimaciones.Rds') %>%
  mutate(rrmse_FH = sqrt(FH_MSE) / FH,
         rmse_FH = sqrt(FH_MSE),
         Direct_ee = sqrt(Direct_MSE))
################################################################################
###--------------- Análisis de residuales y efectos aleatorios --------------###
################################################################################

#--- Residuales estandarizados ---#

residuals <- fh_arcsin$model$real_residuals
std_residuals <- (residuals - mean(residuals)) / sd(residuals)

#--- Residuales estandarizados de los efectos aleatorios ---#

rand.eff <- fh_arcsin$model$random_effects
srand.eff <- (rand.eff - mean(rand.eff)) / sd(rand.eff)

#--- Gráfico de residuales estandarizados ---#

ggplot(data.frame(Residuals = fh_arcsin$model$std_real_residuals)) +
  geom_point(aes(y = Residuals, x = 1:length(Residuals))) +
  labs(y = "Residuales estandarizados", x = "") +
  geom_hline(yintercept = 0, col = "blue")

###-------------------------------- QQ Plots --------------------------------###

dev.off()
theme_set(theme_bw())

p1 <- ggplot(data.frame(Residuals = std_residuals),
             aes(sample = Residuals)) +
  stat_qq() + stat_qq_line(col = "blue") +
  ggtitle("qqplot - residuales estandarizados")

p2 <- ggplot(data.frame(Residuals = srand.eff),
             aes(sample = Residuals)) +
  stat_qq() + stat_qq_line(col = "blue") +
  ggtitle("qqplot - efectos aleatorios")

###--- Gráfico con patchwork ---###

p1 | p2

###------------------------------- Densidades -------------------------------###

color = c("blue", "lightblue3")

p3 <- ggplot(
  data.frame(Residuals = std_residuals),
  aes(x = Residuals),
  fill = color[2],
  color = color[2]
) +
  geom_density(fill = color[2],
               color = color[2],
               alpha = 0.4) +
  stat_function(fun = dnorm) + xlim(-4, 4) +
  ggtitle("Densidad - residuales estandarizados")

p4 <- ggplot(
  data.frame(Residuals = srand.eff),
  aes(x = Residuals),
  fill = color[2],
  color = color[2]
) +
  geom_density(fill = color[2],
               color = color[2],
               alpha = 0.4) +
  stat_function(fun = dnorm) + xlim(-4, 4) +
  ggtitle("Densidad - efectos aleatorios")

###--- Gráfico con patchwork ---###

p3 | p4

# Validaciones 2 ----------------------------------------------------------

summary(fh_arcsin)
#EJEMPLO
# Residual diagnostics:
#                          Skewness Kurtosis Shapiro_W   Shapiro_p
#Standardized_Residuals -0.07589517 3.316395 0.9919786 0.178318868
#Random_effects         -0.07058570 3.961703 0.9815662 0.002118452
#NUESTRO 
# Residual diagnostics:
#   Skewness Kurtosis Shapiro_W   Shapiro_p
# Standardized_Residuals 0.09072178 3.678806 0.9885634 0.319237096
# Random_effects         0.62344521 5.004862 0.9661612 0.001764755

#------------------------ Coeficiente de determinación ------------------------#

base_dir <- base_completa %>% filter(!is.na(Rd))

#--- Coeficientes del modelo ajustado ---#

Betas <-  as.matrix(fh_arcsin$model$coefficients[, 1], ncol = 1)
rownames(Betas) <- rownames(fh_arcsin$model$coefficients)
XS <- cbind(as.matrix(base_dir %>% 
                        dplyr::select(rownames(Betas))))

residuos <- XS %*% Betas - c(colMeans(XS) %*% Betas)
D <- dim(XS)[1]
q <- dim(XS)[2]
S2Beta <- sum(residuos ^ 2) / (D - 1)
su2 <- fh_arcsin$model$variance
(R2 <- 1 - (su2 / (((D - q) / (D - 1)) * su2 + S2Beta)))
# > R2
#EJEMPLO
#[1] 0.9314482

#NOSOTROS
#[1] 0.5015053
#------------------------------------------------------------------------------#
#-- Estimación SAE como función del tamaño muestral (Directo, FH y sintético) -#
#------------------------------------------------------------------------------#

Ncomunas <- nrow(base_completa)

ggplot(estimaciones[order(estimaciones$n),],
       aes(x = 1:Ncomunas)) +
  geom_line(aes(y = Direct, color = "Directo")) +
  geom_line(aes(y = FH, color = "Fay Herriot")) +
  labs(y = "Formalidad",
       x = "Tamaño muestral", color = "") +
  scale_x_continuous(breaks = seq(1, Ncomunas, by = 10),
                     labels = estimaciones$n[order(estimaciones$n)][seq(1, Ncomunas, by = 10)]) +
  scale_color_manual(values = c("Directo" = "Blue", "Fay Herriot" = "red"))

#---------------------- Estimacion directa vs Fay-Herriot ---------------------#

ggplot(estimaciones, aes(Direct, FH)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ggtitle("Comparación de estimaciones")

#----------------------------- Distancias de Cook -----------------------------#

D <- dim(base_completa)[1]
CD = numeric(D)
Betas <-  as.matrix(fh_arcsin$model$coefficients[, 1], ncol = 1)
V <- diag(1 / (su2 + 1 / (4 * base_dir$n_eff_FGV)))

#--- Ciclo para DC ---#

for (i in 1:D) {
  print(i)
  
  BetaModelo <-
    fh(
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
      combined_data = base_completa[-i, ] %>% data.frame(),
      domains = "id_municipio",
      method = "reml",
      transformation = "arcsin",
      backtransformation = "bc",
      eff_smpsize = "n_eff_FGV",
      MSE = FALSE,
      mse_type = "NULL"
    )
  #--- Coeficientes del modelo ajustado ---#
  
  Betas_i = as.matrix(BetaModelo$model$coefficients[, 1], ncol = 1)
  betaDiff <- Betas - Betas_i
  CD[i] = (1 / (q - 1)) * t(betaDiff) %*% (t(XS) %*% V %*% XS) %*% betaDiff
}

# TODO: Extraer los municipios que son excluidos del modelo
# dist_cook <- data.frame(cookDis = CD, 
#            comuna = base_completa$id_municipio)
# 
# dist_cook$cook_sd<- scale(dist_cook$cookDis)
# 
# excl_cook_dist <- left_join(base_completa,dist_cook, by=c('id_municipio'='comuna')) %>% 
#   mutate(cook_dist_3sd=case_when(cook_sd>=3~1, T~0) )
# 
# excl_cook_dist <- excl_cook_dist %>% filter(cook_sd>=3)
# 
# Obs_excluidas <- plyr::rbind.fill(Obs_excluidas_et1
#                                   ,excl_cook_dist)


#------------------ Gráfico de Distancias de Cook por comuna ------------------#

data.frame(cookDis = CD, comuna = base_completa$des_municipio) %>%
  ggplot(aes(y = cookDis, x = 1:Ncomunas)) +
  geom_point(col = "blue") + 
  geom_text(aes(label = ifelse(cookDis > 0.10,
                               as.character(comuna), '')), 
            hjust = 0, vjust = 0) +
  labs(y = "Distancia de Cook", x = "Comunas")

#----------------------- Exportando: Distancias de Cook -----------------------#

saveRDS(CD, file = "Data/cookDis_2021.rds")

#---------------------- Coeficiente de variación y RRMSE ----------------------#

ggplot(estimaciones %>% 
         arrange(n), aes(x = 1:Ncomunas)) +
  geom_line(aes(y = Direct_CV, color = "CV")) +
  geom_line(aes(y = rrmse_FH, color = "RRMSE")) +
  labs(y = "Coeficientes de variación", 
       x = "Tamaño muestral", color = "") +
  scale_x_continuous(breaks = seq(1, Ncomunas, by = 50)) +
  scale_color_manual(values = c("CV" = "Blue", "RRMSE" = "red"))

#--------------------------- Error estándar y RRMSE ---------------------------#

ggplot(estimaciones %>% 
         arrange(n), aes(x = 1:Ncomunas)) +
  geom_line(aes(y = Direct_ee, color = "Directo")) +
  geom_line(aes(y = rmse_FH, color = "Fay-Herriot")) +
  labs(y = "Error estándar", 
       x = "Tamaño muestral", color = "") +
  scale_x_continuous(breaks = seq(1, Ncomunas, by = 50),
                     labels = estimaciones$n[order(estimaciones$n)][seq(1, Ncomunas, by = 50)]) +
  scale_color_manual(values = c("Directo" = "Blue", "Fay-Herriot" = "red"))

#----------------------------- Boxplot: CV Directo ----------------------------#

estimaciones %>% 
  ggplot(aes(Direct_CV)) + 
  geom_boxplot()

#--- Boxplot: CV Directo y RRMSE FH ---#

melted <- estimaciones %>% 
  dplyr::select(Direct_CV, rrmse_FH) %>% 
  melt()

ggplot(melted, aes(factor(variable), value)) + 
  geom_boxplot()  +
  labs(y = "Coeficiente de variación", x = "")

### Boxplot: Error estándar Directo y RMSE FH ###

melted <- estimaciones %>% 
  dplyr::select(Direct_ee, rmse_FH) %>% 
  melt()

ggplot(melted, 
       aes(factor(variable), value)) + 
  geom_boxplot() +
  labs(y = "Errores estándar", x = "")


################################################################################
### Matriz vecinos cercanos ###
################################################################################
# st_is_longlat(poligonos_comuna)
# #--- Construyendo la matriz de vecinos de una lista de polígonos ---#
# 
# lista_comvecinos <-
#   spdep::poly2nb(pl = poligonos_comuna, queen = FALSE)
# 
# #--- Resumen de la matriz de vecinos ---#
# 
# summary(lista_comvecinos)
# 
# #--------------------- Exportando salidas: matriz de vecinos ------------------#
# 
# saveRDS(lista_comvecinos,
#         file = "Data\\Script3\\Data\\lista_comvecinos.rds")
# 
# #-------------------------- Cargando: Matriz de vecinos -----------------------#
# 
# lista_comvecinos <-
#   readRDS("Data\\Script3\\Data\\lista_comvecinos.rds")
# 
# #--- Matrices de ponderaciones espaciales para matriz de vecinos ---#
# 
# W <- nb2mat(lista_comvecinos, zero.policy = T)
# 
# #--- Nombrando las filas y columnas de la matriz de ponderaciones espaciales --#
# 
# row.names(W) <- paste0(poligonos_comuna$ENLACE)
# colnames(W) <- paste0(poligonos_comuna$ENLACE)
# 
# #--- Ordenando las filas y columnas de la matriz de ponderaciones espaciales --#
# 
# W <- W[order(row.names(W)),]
# W <- W[, order(colnames(W))]
# 
# #----  Comunas que se encuentran en la matriz de pesos W y de estimaciones  ---#
# 
# fil <- row.names(W) %in% 
#   estimaciones$Domain[!is.na(estimaciones$FH)]
# W2 <- W[fil, fil]
# 
# #--- Pruebas de correlación espacial de Moran's I y Geary's C ---#
# 
# # Moran I está entre -1 (indicando dispersión perfecta) a 1 (correlación perfecta).
# # Geary C está entre 0 y 2 (0 correlación positiva; 1 sin correlación; 2 negativa )
# # H0: hay un patrón aleatorio (no espacial) en los datos
# 
# spatialcor.tests(estimaciones$FH[fil], W2)
# #TODO: Crear los valores del FH_BENCH para comparar los datos ajustados ESTÉTICAMENTE
# #spatialcor.tests(estimaciones$FH_MSE[fil], W2) #preguntar sobre esto
# 
