################################################################################
## Title:        Modelo Fay Herriot para estimaciones directas utilizando     ##
##               transformación arcoseno y FGV                                ##
## Returns:      Estimación de Horvitz Thompson para los dominios             ##
## Author:       Stalyn Guerrero - Joel Mendez - Carlos Pena - Andrés Gutiérrez##
## Date:         01-2023                                                      ##
################################################################################
rm(list = ls())
library(plotly)
library(dplyr)
library(tidyr)
library(forcats)
library(survey)
library(srvyr)

encuestaDOM <-  readRDS("Data/encuestaDOM.Rds")
estimacionesPre <- readRDS('Data/estimacionesBench.Rds') %>% 
  dplyr::select(id_region, id_dominio, W_i)

TablaFinal <- readRDS('Data/TablaFinal.Rds') %>% 
  inner_join(estimacionesPre, by = "id_dominio")

base_completa <- readRDS('Data/base_completa.Rds')
############## Estimación agregada ######################
estimaciones_agregada <- TablaFinal %>% data.frame() %>% 
  group_by(id_region) %>% 
  summarize(
    Sintetico=sum(W_i*sintetico_back),
    FH_bench=sum(W_i*FH_RBench),
    FH = sum(W_i*FayHerriot)
  )

options(survey.lonely.psu= 'adjust' )
#Población dividida entre cuatro, ESTA FORMA PORQUE LA ENCUESTA ES TRIMESTRAL, 
# Y SU AGRUPACIÓN ANUAL IMPLICA QUE EL FACTOR EXPANSIÓN SEA DIVIDIDO ENTRE 4 
encuestaDOM <-
  encuestaDOM %>%
  mutate(
    upm = as.character(upm),
    estrato = as.character(estrato),
    factor_anual = factor_expansion / 4
  )

#Creación de objeto diseno--------------------------------------- 
#Diseños muestrales encuestas complejas TRIMESTRALES
disenoDOM <- encuestaDOM %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = factor_anual,
    nest=T
  )

#Cálculo del indicador ----------------
indicador_dir <-
  disenoDOM %>% group_by(id_region) %>%
  filter(ocupado == 1 & pet == 1) %>%
  summarise(
    n = unweighted(n()),
    Rd = survey_ratio(
      numerator = orden_sector == 2 ,
      denominator = 1,
      vartype = c("se", "ci"),
      deff = F
    )
  )



data_plot <- left_join(estimaciones_agregada, 
                       indicador_dir, by = 'id_region')  %>% data.frame()

temp <- data_plot %>% select(-Rd_low, -Rd_upp,-Rd_se) %>%
  gather(key = "Estimacion",value = "value", -n,-id_region) %>% 
  mutate(Estimacion = case_when(Estimacion == "FH" ~ "Fay Harriot",
                                Estimacion == "FH_bench" ~ "FH bench",
                                Estimacion == "Rd"~ "Directo",
                                      TRUE ~ Estimacion))
lims_IC <-  data_plot %>%
  select(n,id_region,value = Rd, Rd_low, Rd_upp) %>% 
  mutate(Estimacion = "Directo")

p <- ggplot(temp,
            aes(
              x = fct_reorder2(id_region, id_region, n),
              y = value,
              shape = Estimacion,
              color = Estimacion
            )) +
  geom_errorbar(
    data = lims_IC,
    aes(ymin = Rd_low ,
        ymax = Rd_upp, x = id_region),
    width = 0.2,
    size = 1
  )  +
  geom_jitter(size = 3)+
  labs(x = "Región")
ggplotly(p)

### opción 2 para el gráfico

ggplot(data = temp, aes(x = id_region, y = value, shape = Estimacion)) +
  geom_point(aes(color = Estimacion), size = 4) +
  geom_line(data = lims_IC,
            aes(x =  as.numeric(id_region), y = Rd_low),
            linetype  = 2) +
  geom_line(data = lims_IC,
            aes(x =  as.numeric(id_region), y = Rd_upp),
            linetype  = 2)  +
  theme_bw(10) +
  labs(y = "", x = "Orden por tamaño de la muestra")
