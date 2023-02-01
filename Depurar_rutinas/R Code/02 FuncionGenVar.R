################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Returns:      Estimación de Horvitz Thompson para los dominios             ##
## Author:       Stalyn Guerrero - Joel Mendez - Carlos Pena - Andrés Gutiérrez##
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


indicador_dom <- readRDS('Data/indicador_dom.Rds')
auxiliar_org <- readRDS('Data/auxiliar_org.Rds') %>% 
  mutate_at(.vars = id_dominio, as.character)


indicador_dom <-
  left_join(indicador_dom, (
    auxiliar_org %>% select('id_municipio', 'Area_km', 'ZONA_Rur', 'Densidad_Pob')
  ), by = 'id_municipio')

#Leer data

# Filtrar valores para obtener solo aquellos que puedan 
# estimar bién la varianza --------


# d1 <- d %>% filter(Rd_var>0 & Rd_deff>1 & Rd_var<20)
# d1 <- d 
# d1 <- d %>% filter( Rd_deff>1 )
indicador_dom1 <- indicador_dom %>% 
  filter(Rd_var>0 & Rd_deff>=1) #Se filtran los valores, esto es subjetivo, pero se excluyen todos los tengan varianza 0, y todos los que tengan un deff mayor que 1
# d1 <- d %>% filter(Rd_var>0)
#d1 <- d %>% filter((Rd_var>0 & Rd_deff>1) | Rd_cv<20 )
#d1 <- d %>% filter(Rd_var>0 & Rd_cv<1 &Rd_deff>1)
# Agregar 

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

# Residual standard error: 0.6269 on 81 degrees of freedom
# Multiple R-squared:  0.6477,	Adjusted R-squared:  0.6173 
# F-statistic: 21.28 on 7 and 81 DF,  p-value: 5.432e-16

#-----------Estimación de valores excluidos--------------
delta.hat = sum(baseFGV$Rd_var) / 
  sum(exp(fitted.values(FGV1)))
delta.hat
#1.236742
hat.sigma <- data.frame(id_municipio = baseFGV$id_municipio,
                        hat_var = delta.hat * exp(fitted.values(FGV1)))
baseFGV$ln_sigma2_pred <- hat.sigma$hat_var

# =============Plot de las estimaciones=================

par(mfrow = c(2, 2))

#Plot1
plot(FGV1)


#Plot2
ggplot(baseFGV, 
       aes(x = ln_sigma2, y = ln_sigma2_pred)) + 
  geom_point() +
  geom_smooth(method = "loess")


#------------Unir las estimaciones con la data original-------------
indicador_dom$prediccion_ln_0 = 
  predict(FGV1, newdata = indicador_dom %>%
                              filter(
                                # (Rd_var == 0 & Rd_deff<=1 ) &
                                !is.na(Rd))) 

base_sae <- indicador_dom %>% 
  left_join(hat.sigma, by = id_dominio)

base_sae$hat_var <-
  delta.hat * exp(indicador_dom$prediccion_ln_0)

#----------Transformación de la data para consumo final-------------------


##
base_FH <- base_sae %>%
  mutate(
    Rd_deff = ifelse(is.nan(Rd_deff), 1,
                     Rd_deff),
    deff_FGV = ifelse(
      Rd_var == 0 ,
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

