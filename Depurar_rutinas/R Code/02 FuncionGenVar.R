################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Returns:      Estimación de Horvitz Thompson para los dominios             ##
## Author:       Joel Mendez - Carlos Pena - Stalyn Guerrero- Andrés Gutiérrez##
## Date:         01-2023                                                      ##
## Este código intenta estimar la varianza para los valores cuya estimación   ##
## directa tiene varianza 0 (incorrectamente)                                 ##
################################################################################

rm(list = ls())
.libPaths('C:/R packs')
library(ggplot2)
library(dplyr)
library(patchwork)

id_dominio <- "id_municipio"

## Lectura de indicadores estimados en el paso anterior. 

indicador_dom <- readRDS('Data/indicador_dom.Rds')

## Leer información auxiliar
auxiliar_org <- readRDS('Data/auxiliar_org.Rds') %>% 
  mutate_at(.vars = id_dominio, as.character)

## Uniendo las bases de datos 
indicador_dom <-
  left_join(indicador_dom, (
    auxiliar_org %>% select(all_of(id_dominio), 'Area_km',
                            'ZONA_Rur', 'Densidad_Pob')
  ), by = id_dominio)

#Leer data

# Filtrar valores para obtener solo aquellos que puedan 
# estimar bién la varianza --------

# Se filtran los valores, esto es subjetivo, pero se excluyen todos los tengan 
# varianza 0, y todos los que tengan un deff mayor que 1

indicador_dom1 <- indicador_dom %>% 
  filter(Rd_var>0 & Rd_deff>=1) 

############Plots de la data#########

baseFGV <-  indicador_dom1 %>%  
  dplyr::select(id_dominio , Rd, n, Rd_var, Area_km) %>%
  mutate(ln_sigma2 = log(Rd_var))

p1 <- ggplot(baseFGV, aes(x = Rd, y = ln_sigma2)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Formal")

p2 <- ggplot(baseFGV, aes(x = n, y = ln_sigma2)) + 
  geom_point() +
  geom_smooth(method = "loess") + 
  xlab("Tamaño de muestra")

p3 <- ggplot(baseFGV, 
             aes(x = Rd * n, y = ln_sigma2)) + 
  geom_point() +
  geom_smooth(method = "loess") + 
  xlab("Número de Formales")

p4 <- ggplot(baseFGV, 
             aes(x = sqrt(Rd), y = ln_sigma2)) + 
  geom_point() +
  geom_smooth(method = "loess") + 
  xlab("Raiz cuadrada de Porcentaje de formalidad")


(p1 | p2) / (p3 | p4)
rm('p1','p2','p3','p4')
#Plots de CEPAL


#-----------------Función de Varianza-----------------
FGV1 <- lm(ln_sigma2 ~ 1 + Rd + 
             n + I(n ^ 2) + I(Rd * n) +
             I(sqrt(Rd)) + I(sqrt(n)) + 
             I(sqrt(Rd * n))+
           I(Area_km)
           ,
           data = baseFGV)
summary(FGV1)
#Esto es una función lineal con parametros recomendados por CEPAL
##Resultados del summary

# Residual standard error: 0.5994 on 80 degrees of freedom
# Multiple R-squared:  0.6985,	Adjusted R-squared:  0.6684 
# F-statistic: 23.17 on 8 and 80 DF,  p-value: < 2.2e-16

#-----------Estimación de valores excluidos--------------
## Determinar el valor de la constante delta. 

delta.hat = sum(baseFGV$Rd_var) / sum(exp(fitted.values(FGV1)))
delta.hat
#1.22868
hat.sigma <- data.frame(id_municipio = baseFGV$id_municipio,
                        hat_var = delta.hat * exp(fitted.values(FGV1)))
baseFGV$hat_var <- hat.sigma$hat_var

# =============Plot de las estimaciones=================
X11()
par(mfrow = c(2, 2))

#Plot1
plot(FGV1)


#Plot2
ggplot(baseFGV, 
       aes(x = Rd_var, y = hat_var)) + 
  geom_point() +
  geom_smooth(method = "loess")


#------------Unir las estimaciones con la data original-------------
indicador_dom$prediccion_ln_0 = predict(FGV1, newdata = indicador_dom)

base_sae <- indicador_dom %>% 
  left_join(hat.sigma, by = id_dominio)

base_sae$hat_var <-
  delta.hat * exp(indicador_dom$prediccion_ln_0)

#----------Transformación de la data para consumo final-------------------


##
base_FH <- base_sae %>%
  mutate(
    Rd_deff = ifelse(is.nan(Rd_deff), 1, Rd_deff),
    deff_FGV = ifelse(Rd_var == 0 ,
      1,
      hat_var / (Rd_var / Rd_deff) #Fórmula del nuevo DEFF
    ),
    # Criterio MDS para regularizar el DeffFGV
    deff_FGV = ifelse(deff_FGV <= 1, NA_real_, deff_FGV), #Deff estimado
    n_eff_FGV = n / deff_FGV, #Número efectivo de personas encuestadas
    hat_var=ifelse(deff_FGV <= 1, NA_real_, hat_var), #Si no se estimó varianza para ese municipio, también excluir la estimación directa de este municipio, esto es relevante para el modelo FH 
    Rd=ifelse(is.na(hat_var), NA_real_, Rd) 
  )


saveRDS(object = base_FH, "Data/base_FH.Rds")


##### Análisis gráfico#####


ggplot(base_FH %>% filter(!is.na(hat_var)) %>% 
         arrange(n), aes(x = hat_var, y = Rd_var)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = 2) + 
  labs(x = "FGV", y = "VaRdirEst") +
  ylab("Varianza del Estimador Directo")

