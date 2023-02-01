## Listado de librerias 

library(tidyverse)
library(magrittr)
library(stringr)
library(readxl)
library(tmap)
library(sp)
library(sf)
library(survey)
library(tidyverse)
library(srvyr)
library(TeachingSampling)
library(haven)
library(readxl)
library(dplyr)
library(DBI)
library(odbc)
library(ggplot2)
library(dplyr)
library(patchwork)
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
library(plotly)
library(dplyr)
select <- dplyr::select

lista_paquetes <-
  c(
    "survey",
    "srvyr",
    "TeachingSampling",
    "stringr",
    "magrittr",
    "sae",
    "ggplot2",
    "emdi",
    "patchwork",
    "readxl",
    "dplyr"
  )

lapply(
  lista_paquetes,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

