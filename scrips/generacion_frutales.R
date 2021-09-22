
library(readxl)
library(tidyverse)
library(lubridate)
library(plotly)

rutas<-"datos/excel"
out.rds<-"output/rds"


## Datos de superficie ----
dir(rutas, pattern = "frutales", full.names = T) %>% 
  read_excel(sheet = 2, skip=2) %>%
  rename("Superficie_ha"=`Superficie (ha)`) %>%
  rename("nom_region"="Región") %>%
  mutate_if(is.character, tolower)->superficie 

names(superficie)<-tolower(names(superficie))
superficie[c("año", "nom_region","comuna","especie","superficie_ha")]->superficie

# revisar
superficie$especie %>% unique()



## Datos de rendimento ----
dir(rutas, pattern = "frutales", full.names = T) %>% 
  read_excel(sheet = 1, skip=2) %>%
  rename("nom_region"="Región") %>%
  rename("rendimiento_ton_ha"=`Promedio de kilos/hectárea`) %>%
  mutate("rendimiento_ton_ha"=`rendimiento_ton_ha`/1000) %>% #los valores estan en kg/ha
  mutate_if(is.character, tolower) ->rendimiento

names(rendimiento)<-tolower(names(rendimiento))
rendimiento[c("año", "nom_region","comuna","especie","rendimiento_ton_ha")]->rendimiento

#revisar
rendimiento$especie %>% unique()


##Exportar

setwd(out.rds)
write_rds(superficie , "frutales_superficie.rds")
write_rds(rendimiento , "frutales_rendimiento.rds")


