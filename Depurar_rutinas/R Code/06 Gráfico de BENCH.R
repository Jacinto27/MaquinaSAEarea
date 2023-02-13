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
encuestaDOM <-  readRDS("Data/encuestaDOM.Rds") %>% 
  mutate( orden_region = str_pad(string = orden_region, width = 2, pad = "0"))
tablafinal <- readRDS('Data/TablaFinal.Rds')
estimacionesBench <- readRDS('Data/estimacionesBench.Rds') %>% 
  mutate( orden_region = str_pad(string = orden_region, width = 2, pad = "0"))


estimaciones_agregada <- estimacionesBench %>% 
  group_by(orden_region) %>% 
  summarize(
    FH_mean=mean(FH, na.rm=T),
    Sintetico=mean(
      (FH-Gamma*Direct)/(1-Gamma)
      , na.rm=T),
    FH_bench=sum(W_i*FH_RBench)
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
  disenoDOM %>% group_by(orden_region) %>%
  filter(ocupado == 1 & pet == 1) %>%
  summarise(
    n = unweighted(n()),
    Rd = survey_ratio(
      numerator = orden_sector == 2 ,
      denominator = 1,
      vartype = c("se", "ci"),
      deff = T
    )
  )
indicador_dominio <-
  disenoDOM %>% group_by(id_municipio) %>% 
  filter(ocupado == 1 &  pet == 1) %>%
  summarise(
    n = unweighted(n()),
    Rd = survey_ratio(
      numerator = orden_sector == 2 ,
      denominator = 1,
      vartype = c("se", "ci"),
      deff = T
    )
  )
indicador_dominio <- encuestaDOM %>%
  distinct(id_municipio, orden_region) %>%
  right_join(., indicador_dominio, by = 'id_municipio')

estimaciones_agregada <-
  indicador_dominio %>% group_by(orden_region) %>%
  summarise(Direct_mean_mun = mean(Rd)) %>%
  mutate(orden_region = str_pad(orden_region,width = 2,pad = "0"))%>% 
  left_join(., estimaciones_agregada, by = 'orden_region')

data_plot <- left_join(estimaciones_agregada, 
                       indicador_dir, by = 'orden_region') %>%
  dplyr::select(orden_region,
                FH_mean,
                FH_bench,
                Sintetico,
                Rd,
                Direct_mean_mun,
                Rd_low,
                Rd_upp,
                n) %>% data.frame()

temp <- data_plot %>% select(-Rd_low, -Rd_upp) %>%
  gather(key = "Estimacion",value = "value", -n,-orden_region) %>% 
  mutate(Estimacion = case_when(Estimacion == "FH_mean" ~ "Fay Harriot",
                                Estimacion == "FH_bench" ~ "FH bench",
                                Estimacion == "Rd"~ "Directo",
                                Estimacion == "Direct_mean_mun" ~ "Media directo",
                                TRUE ~ Estimacion))
lims_IC <-  data_plot %>%
  select(n,orden_region,value = Rd, Rd_low, Rd_upp) %>% 
  mutate(Estimacion = "Directo")

p <- ggplot(temp,
            aes(
              x = fct_reorder2(orden_region, orden_region, n),
              y = value,
              shape = Estimacion,
              color = Estimacion
            )) +
  geom_errorbar(
    data = lims_IC,
    aes(ymin = Rd_low ,
        ymax = Rd_upp, x = orden_region),
    width = 0.2,
    size = 1
  )  +
  geom_jitter(size = 3)+
  labs(x = "Región")
ggplotly(p)

