#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)
library(reticulate) # Conexión con Python
library(rgee) # Conexión con Google Earth Engine
library(sf) # Paquete para manejar datos geográficos
library(concaveman)
library(geojsonio)
library(magrittr)

#######################################
### configuración inicial de Python ###
#######################################

rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
# Configurar python (Algunas veces no es detectado y se debe reiniciar R)
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)
rgee::ee_Initialize(drive = T)

##############################################################################
## Descarga de información Satélite a nivel distrito. 
##############################################################################

poligonos_distrito <- read_sf( "shapefiles2010/DMCenso2010.shp")

luces <- ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
#ee_print(luces)


DOM_luces_distrito <- map(unique(poligonos_distrito$ENLACE),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = poligonos_distrito["ENLACE"] %>% filter(ENLACE == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(ENLACE = .x),
                 error = function(e)data.frame(ENLACE = .x)))

DOM_luces_distrito %<>% bind_rows() 
poligonos_distrito <- inner_join(poligonos_distrito, DOM_luces_distrito)

map_dist <- tm_shape(poligonos_distrito)

map_dist <-
  map_dist + tm_polygons(
    "F182013_stable_lights",
    title = "Luces nocturnas(media)",
    palette = "YlOrRd"
  )
tmap_save(map_dist,
          "map_temp/Luces nocturna distr.jpg",
          width = 3000,
          height = 2000,
          asp = 0
)

##############################################################################
## Descarga de información Satélite a nivel municipal 
##############################################################################

poligonos_minucipio <- read_sf( "shapefiles2010/MUNCenso2010.shp")

luces <- ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
#ee_print(luces)

# Medida de resumen promedio 

DOM_luces <- map(unique(poligonos_minucipio$ENLACE),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = poligonos_minucipio["ENLACE"] %>% filter(ENLACE == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(ENLACE = .x),
                 error = function(e)data.frame(ENLACE = .x)))

DOM_luces %<>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_luces_mean.rds")

# Medida de resumen suma 

DOM_luces <- map(unique(poligonos_minucipio$ENLACE),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = poligonos_minucipio["ENLACE"] %>% filter(ENLACE == .x),
                   ee$Reducer$sum(),
                   sf = FALSE
                 ) %>% mutate(ENLACE = .x),
                 error = function(e)data.frame(ENLACE = .x)))

DOM_luces %<>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_luces_sum.rds")


################################################################################
### Cubrimiento urbano y cubrimiento cultivos
################################################################################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()

# Medida de resumen promedio 
DOM_suelo <- map(unique(poligonos_minucipio$ENLACE),
                 ~tryCatch(ee_extract(
                   x = tiposuelo,
                   y = poligonos_minucipio["ENLACE"] %>% 
                     filter(ENLACE == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(ENLACE = .x),
                 error = function(e)data.frame(ENLACE = .x)))

DOM_suelo %>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_suelo_mean.rds")
# Medida de resumen suma 
DOM_suelo <- map(unique(poligonos_minucipio$ENLACE),
                 ~tryCatch(ee_extract(
                   x = tiposuelo,
                   y = poligonos_minucipio["ENLACE"] %>% 
                     filter(ENLACE == .x),
                   ee$Reducer$sum(),
                   sf = FALSE
                 ) %>% mutate(ENLACE = .x),
                 error = function(e)data.frame(ENLACE = .x)))

DOM_suelo %>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_suelo_suma.rds")

################################################################################
### Distancia a hospitales ###
################################################################################
dist_salud = ee$Image('Oxford/MAP/accessibility_to_healthcare_2019') 

# Medida de resumen promedio 

DOM_dist_salud <- map(unique(poligonos_minucipio$ENLACE),
                 ~tryCatch(ee_extract(
                   x = dist_salud,
                   y = poligonos_minucipio["ENLACE"] %>% filter(ENLACE == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(ENLACE = .x),
                 error = function(e)data.frame(ENLACE = .x)))

DOM_dist_salud %>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_salud_mean.rds")
# Medida de resumen suma 
DOM_dist_salud <- map(unique(poligonos_minucipio$ENLACE),
                      ~tryCatch(ee_extract(
                        x = dist_salud,
                        y = poligonos_minucipio["ENLACE"] %>% filter(ENLACE == .x),
                        ee$Reducer$sum(),
                        sf = FALSE
                      ) %>% mutate(ENLACE = .x),
                      error = function(e)data.frame(ENLACE = .x)))

DOM_dist_salud %>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_salud_suma.rds")

################################################################################
# CSP gHM: Global Human Modification
################################################################################

CSP_gHM = ee$ImageCollection('CSP/HM/GlobalHumanModification') 
# Medida de resumen promedio 
DOM_CSP_gHM <- map(unique(poligonos_minucipio$ENLACE),
                      ~tryCatch(ee_extract(
                        x = CSP_gHM,
                        y = poligonos_minucipio["ENLACE"] %>% filter(ENLACE == .x),
                        ee$Reducer$mean(),
                        sf = FALSE
                      ) %>% mutate(ENLACE = .x),
                      error = function(e)data.frame(id_municipio = .x)))

DOM_CSP_gHM %>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_gHM_mean.rds")
# Medida de resumen suma 
DOM_CSP_gHM <- map(unique(poligonos_minucipio$ENLACE),
                   ~tryCatch(ee_extract(
                     x = CSP_gHM,
                     y = poligonos_minucipio["ENLACE"] %>% filter(ENLACE == .x),
                     ee$Reducer$sum(),
                     sf = FALSE
                   ) %>% mutate(ENLACE = .x),
                   error = function(e)data.frame(id_municipio = .x)))

DOM_CSP_gHM %>% bind_rows() %>% 
  saveRDS("map_temp/data/temp_gHM_suma.rds")


################################################################################
# Medida de resumen promedio 

satelital_promedio <- reduce(list(
readRDS("map_temp/data/temp_gHM_mean.rds"),
readRDS("map_temp/data/temp_salud_mean.rds"),
readRDS("map_temp/data/temp_suelo_mean.rds"),
readRDS("map_temp/data/temp_luces_mean.rds")), inner_join) %>% 
  rename(
    luces_nocturnas = F182013_stable_lights,
    cubrimiento_cultivo = X2016_crops.coverfraction,
    cubrimiento_urbano = X2016_urban.coverfraction,
    accesibilidad_hospitales = accessibility,
    accesibilidad_hosp_caminado = accessibility_walking_only,
    modificacion_humana = X2016_gHM)

# Medida de resumen suma 

satelital_suma <- reduce(list(
  readRDS("map_temp/data/temp_gHM_suma.rds"),
  readRDS("map_temp/data/temp_salud_suma.rds"),
  readRDS("map_temp/data/temp_suelo_suma.rds"),
  readRDS("map_temp/data/temp_luces_sum.rds")), inner_join) %>% 
  rename(
    luces_nocturnas = F182013_stable_lights,
    cubrimiento_cultivo = X2016_crops.coverfraction,
    cubrimiento_urbano = X2016_urban.coverfraction,
    accesibilidad_hospitales = accessibility,
    accesibilidad_hosp_caminado = accessibility_walking_only,
    modificacion_humana = X2016_gHM)

inner_join(
  satelital_promedio,
  satelital_suma,
  by = "ENLACE",
  suffix = c("_promedio", "_suma")
) %>%
  saveRDS("Data/auxiliar_satelital.Rds")

################################################################################
### Creando polinomios 
poligonos_promedio <- read_sf( "shapefiles2010/MUNCenso2010.shp")%>%
  select(ENLACE) %>% 
  inner_join(satelital_promedio) 

poligonos_suma <- read_sf( "shapefiles2010/MUNCenso2010.shp") %>%
 select(ENLACE) %>% 
  inner_join(satelital_suma) 

################################################################################
# Mapas de las variables satelitales 
################################################################################
tmap_options(check.and.fix = TRUE)

m1 <- tm_shape(poligonos_suma)
m2 <- tm_shape(poligonos_promedio)

# Modificacion humana

m1_modificacion_humana<-
  m1 + tm_polygons(
    "modificacion_humana",
    title = "modificacion_humana(suma)",
    palette = "YlOrRd"
  )

m2_modificacion_humana <-
  m2 + tm_polygons(
    "modificacion_humana",
    title = "modificacion_humana(media)",
    palette = "YlOrRd"
  )

map_modificacion_humana <- tmap_arrange(list(m1_modificacion_humana,
                                             m2_modificacion_humana),
                                                ncol = 2,
                                                norw = 1)

tmap_save(map_modificacion_humana,
          "map_temp/modificacion_humana.jpg",
          width = 3000,
          height = 2000,
          asp = 0
)

# Accesibilidad hospitales caminado

m1_accesibilidad_hosp_caminado<-
  m1 + tm_polygons(
    "accesibilidad_hosp_caminado",
    title = "accesibilidad_hosp_caminado(suma)",
    palette = "YlOrRd"
  )

m2_accesibilidad_hosp_caminado <-
  m2 + tm_polygons(
    "accesibilidad_hosp_caminado",
    title = "accesibilidad_hosp_caminado(media)",
    palette = "YlOrRd"
  )

map_accesibilidad_hosp_caminado <-
  tmap_arrange(
    list(
      m1_accesibilidad_hosp_caminado,
      m2_accesibilidad_hosp_caminado
    ),
    ncol = 2,
    norw = 1
  )

tmap_save(map_accesibilidad_hosp_caminado,
          "map_temp/accesibilidad_hosp_caminado.jpg",
          width = 3000,
          height = 2000,
          asp = 0
)

# Accesibilidad hospitales

m1_accesibilidad_hospitales <-
  m1 + tm_polygons(
    "accesibilidad_hospitales",
    title = "accesibilidad_hospitales(suma)",
    palette = "YlOrRd"
  )

m2_accesibilidad_hospitales <-
  m2 + tm_polygons(
    "accesibilidad_hospitales",
    title = "accesibilidad_hospitales(media)",
    palette = "YlOrRd"
  )


map_accesibilidad_hospitales <-
  tmap_arrange(
    list(m1_accesibilidad_hospitales,
         m2_accesibilidad_hospitales),
    ncol = 2,
    norw = 1
  )

tmap_save(map_accesibilidad_hospitales,
          "map_temp/accesibilidad_hospitales.jpg",
          width = 3000,
          height = 2000,
          asp = 0
)

# Cubrimiento urbano

m1_cubrimiento_urbano <-
  m1 + tm_polygons(
    "cubrimiento_urbano",
    title = "cubrimiento_urbano(suma)",
    palette = "YlOrRd"
  )

m2_cubrimiento_urbano <-
  m2 + tm_polygons(
    "cubrimiento_urbano",
    title = "cubrimiento_urbano(media)",
    palette = "YlOrRd"
  )

map_cubrimiento_urbano <- tmap_arrange(
  list(m1_cubrimiento_urbano,
       m2_cubrimiento_urbano),
  ncol = 2,
  norw = 1
)

tmap_save(map_cubrimiento_urbano,
          "map_temp/cubrimiento_urbano.jpg",
          width = 3000,
          height = 2000,
          asp = 0
)

# Cubrimiento cultivo

m1_cubrimiento_cultivo <-
  m1 + tm_polygons(
    "cubrimiento_cultivo",
    title = "cubrimiento_cultivo(suma)",
    palette = "YlOrRd"
  )

m2_cubrimiento_cultivo <-
  m2 + tm_polygons(
    "cubrimiento_cultivo",
    title = "cubrimiento_cultivo(media)",
    palette = "YlOrRd"
  )

map_cubrimiento_cultivo <-
  tmap_arrange(
    list(m1_cubrimiento_cultivo, m2_cubrimiento_cultivo),
    ncol = 2,
    norw = 1
  )

tmap_save(map_cubrimiento_cultivo,
          "map_temp/cubrimiento_cultivo.jpg",
          width = 3000,
          height = 2000,
          asp = 0
)

# Luces nocturnas 

m1_luces <-
  m1 + tm_polygons(
    "luces_nocturnas",
    title = "luces_nocturnas(suma)",
    palette = "YlOrRd"
  )

m2_luces <-
  m2 + tm_polygons(
    "luces_nocturnas",
    title = "luces_nocturnas(media)",
    palette = "YlOrRd"
  )

map_luces <- tmap_arrange(list(m1_luces,m2_luces),
                          ncol = 2,
                          norw = 1)

tmap_save(map_luces,
          "map_temp/luces.jpg",
          width = 6920,
          height = 4080,
          asp = 0
)


####################################################
## Correlación estimación directa 
####################################################

satelital_suma %<>% mutate(
  id_municipio = substring(ENLACE,3),
  id_municipio = as.numeric(id_municipio)
) 

satelital_promedio %<>% mutate(
  id_municipio = substring(ENLACE,3),
  id_municipio = as.numeric(id_municipio)
)

# Estimaciones directas 

estimacion_dir <- readRDS("Data/base_FH.Rds") %>% 
  filter(!is.na(hat_var)) %>% 
  select(id_municipio, Rd) %>% 
  mutate(id_municipio = as.numeric(id_municipio))

estimacion_dir_suma <- inner_join(estimacion_dir, satelital_suma)
estimacion_dir_promedio <- inner_join(estimacion_dir, satelital_promedio) 

## Correlación variables satelital Vs estimación directa 

cor_satelite <- rbind(
  cor(
    estimacion_dir_suma$Rd,
    estimacion_dir_suma %>% select(-ENLACE,-id_municipio, -Rd)
  ),
  cor(
    estimacion_dir_promedio$Rd,
    estimacion_dir_promedio %>% select(-ENLACE,-id_municipio, -Rd)
  )
)

rownames(cor_satelite) <- c("Suma", "Media")
t(cor_satelite) %>% kableExtra::kable() %>% kableExtra::kable_classic()

################################################################################
## Información auxiliar en el modelo 
################################################################################

auxiliar <- readRDS("Data/auxiliar_org.Rds")  %>% 
  select(
     -F182013_stable_lights,
    -X2016_crops.coverfraction,
   -X2016_urban.coverfraction,
    -accessibility,
  -accessibility_walking_only,
  -ID_PROVINCIA) %>% 
  select_if(is.numeric)

poligonos_auxiliar <- read_sf( "shapefiles2010/MUNCenso2010.shp") %>%
  mutate(
    ENLACE = substring(ENLACE,3),
    id_municipio = as.numeric(ENLACE)
  ) %>% select(id_municipio,TOPONIMIA) %>% 
  inner_join(auxiliar) 


map_auxiliar <- tm_shape(poligonos_auxiliar)

for(ii in names(auxiliar)[-1]){
  m1_auxiliar <-
    map_auxiliar + tm_polygons(
      ii,
      title = ii,
      palette = "YlOrRd"
    )
  tmap_save(m1_auxiliar,
           paste0("map_temp/",ii,".jpg"),
            width = 3000,
            height = 2000,
            asp = 0
  )
  
}



