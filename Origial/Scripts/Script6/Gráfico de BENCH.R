library(plotly)
library(dplyr)
encuestaDOM <-  readRDS("Data\\Script1\\Data\\encuestaDOM.Rds")
tablafinal <- readRDS('Data\\Script4\\TablaFinal.Rds')
estimacionesBench <- readRDS('Data\\Script4\\estimacionesBench.Rds')


a <- estimacionesBench %>% 
  group_by(orden_region) %>% 
  summarize(
  FH_mean=mean(FH, na.rm=T),
  Sintetico=mean(
    (FH-Gamma*Direct)/(1-Gamma)
    , na.rm=T),
  FH_bench=sum(W_i*FH_RBench)
  # ,
  # Direct_mean=mean(Direct,na.rm=T)
)

options(survey.lonely.psu= 'adjust' )
#Población dividida entre cuatro, ESTA FORMA PORQUE LA ENCUESTA ES TRIMESTRAL, Y SU AGRUPACIÓN ANUAL IMPLICA QUE EL FACTOR EXPANSIÓN SEA DIVIDIDO ENTRE 4 
encuestaDOM <-
  encuestaDOM %>% 
  mutate(upm = as.character(upm), estrato = as.character(estrato),factor_anual=factor_expansion/4)

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
d <-
  disenoDOM %>% group_by(orden_region) %>%filter(ocupado == 1 & pet == 1)%>%
  summarise(n = unweighted(n()), Rd = survey_ratio(numerator = orden_sector==2 ,
                                                   denominator = 1,
                                                   vartype = c("se", "ci", "var", "cv"),deff=T))
c <-disenoDOM %>% group_by(id_municipio) %>%filter(ocupado == 1 & pet == 1)%>%
  summarise(n = unweighted(n()), Rd = survey_ratio(numerator = orden_sector==2 ,
                                                   denominator = 1,
                                                   vartype = c("se", "ci", "var", "cv"),deff=T))
c <- encuestaDOM %>% dplyr::select(id_municipio,orden_region) %>% right_join(.,c, by ='id_municipio')

a <- c %>% group_by(orden_region) %>% summarise(Direct_mean_mun=mean(Rd)) %>% left_join(.,a, by='orden_region')


b <- left_join(a,d,by='orden_region') %>% dplyr::select(orden_region, FH_mean,FH_bench, Sintetico, Rd,Direct_mean_mun,Rd_low, Rd_upp,n)

# b <- arrange(b,desc(n))
fig1 <- plot_ly()
fig1 <- add_trace(
  fig1,
  type='scatter',
  mode='markers',
  x=b$orden_region,
  y=arrange(b,desc(n))$Rd,
  marker=list(symbol='x', size=(8)),
  name='Directo'
)
fig1 <- add_trace(
  fig1,
  type='scatter',
  mode='lines',
  x=b$orden_region,
  y=arrange(b,desc(n))$Rd_upp,
  name='Direct_upp',
  color=I('black'),
  line=list(dash='dash')
)
fig1 <- add_trace(
  fig1,
  type='scatter',
  mode='lines',
  x=b$orden_region,
  y=arrange(b,desc(n))$Rd_low,
  name='Direct_low',
  color=I('black'),
  line=list(dash='dash')
  
  )
fig1 <- add_trace(
  fig1,
  type='scatter',
  mode='markers',
  x=b$orden_region,
  y=arrange(b,desc(n))$FH_mean,
  marker=list(symbol='star', size=(8)),
  name='FayHarriot'
)
fig1 <- add_trace(
  fig1,
  type='scatter',
  mode='markers',
  x=b$orden_region,
  y=arrange(b,desc(n))$Sintetico,
  marker=list(symbol='square', size=(8)),
  name='Sintético'
)
fig1 <- add_trace(
  fig1,
  type='scatter',
  mode='markers',
  x=b$orden_region,
  y=arrange(b,desc(n))$FH_bench,
  marker=list(symbol='x', size=(8)),
  name='FH_bench'
)
fig1 <- add_trace(
  fig1,
  type='scatter',
  mode='markers',
  x=b$orden_region,
  y=arrange(b,desc(n))$Direct_mean_mun,
  marker=list(symbol='triangle-up', size=(8)),
  name='Direct_mean'
)
fig1  

