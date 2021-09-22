
library(readxl)
library(tidyverse)
library(lubridate)
library(plotly)



rutas<-"E:/ALBERtO/HEMERA/PROYECtO/CAtAStRO_AGRICOLA/datos/excel"
out.rds<-"E:/ALBERtO/HEMERA/PROYECtO/CAtAStRO_AGRICOLA/output/rds"


## Datos de Superficie ----
#leer datos de superfice
dir(rutas, pattern = "cultivos", full.names = T)%>% 
  read_excel(sheet = 3, skip=4) ->superficie 

names(superficie)<-tolower(names(superficie))
names(superficie)<-str_replace(names(superficie), " ", "_")

stop.col<-grep("industrial_otros", names(superficie))
grep(pattern = "total", names(superficie), value=T) # especies con varriedades



#organizacion y homogeneizacion 
superficie[1:357,] %>% 
  filter(región!="") %>% # filtrar las filas vacias y los comentarios extras al final de del campo Año s
  select(c(1:stop.col), -total) %>%
  separate(., col=región,into = c("cod_region", "nom_region"), sep=2 ) %>%
  mutate_at(vars(4:ncol(.)), extract_numeric) %>% 
  mutate_if(is.character, tolower)%>%
  mutate_if(is.character, str_trim) %>%
  mutate(año=parse_number(año_agrícola)) %>%
  mutate(año=year(parse_date(as.character(año), format="%Y"))) %>% 
  
  #identificacion de las especies de cultivos clasificados con variedades y las que no, se indica como "noidentificada" a las especies donde su variedad no es informada
  rename_at(., vars(!contains(c("trigo", "cebada", "maíz", "lupino")), -(c(año_agrícola:nom_region, año ))),  list( ~paste0(., "_noidentificada") )) %>%
  mutate(trigo_noidentificada=if_else(is.na(trigo_harinero)==TRUE & is.na(trigo_candeal)==TRUE,trigo_total, 0 )) %>% 
  mutate(cebada_noidentificada=if_else(is.na(cebada_cervecera)==TRUE & is.na(cebada_forrajera)==TRUE,cebada_total, 0 )) %>%
  mutate(maíz_noidentificada=if_else(is.na(maíz_consumo)==TRUE & is.na(maíz_semilla)==TRUE,maíz_total, 0 )) %>%
  mutate(lupino_noidentificada=if_else(is.na(lupino_amargo)==TRUE & is.na(lupino_australiano)==TRUE & is.na(lupino_dulce)==TRUE & is.na(lupinos_otros)==TRUE  ,lupino_total, 0 )) %>%
  select_at(., vars(!contains("total"))) %>%
  gather(variedad, superficie_ha, -año ,-año_agrícola, -cod_region, -nom_region) %>%
  separate(., col=variedad, into = c("especie", "variedad"), sep="_")-> superficie_variedad

superfice_variedad[c("año", "año_agrícola","cod_region", "nom_region", "especie", "variedad", "superficie_ha")]-> superficie_variedad

superficie_variedad %>% 
  group_by(año, año_agrícola, cod_region, nom_region, especie) %>%
  summarise(superficie_ha=sum(superficie_ha, na.rm = T)) ->superficie_especie

#revisar
superficie_variedad$especie %>% unique()
rendimiento_variedad$variedad %>% unique()

superficie_variedad%>%
  filter(especie=="tomate")


#exportar
setwd(out.rds)
write_rds(superficie_especie , "cultivos_superficie_especie.rds")
write_rds(superficie_variedad ,"cultivos_superficie_variedad.rds")




#Datos de Rendimiento ----
#leer datos de rendimiento
dir(rutas, pattern = "cultivos", full.names = T)%>% 
  read_excel(sheet = 5, skip=3) ->rendimiento

names(rendimiento)<-tolower(names(rendimiento))
names(rendimiento)<-str_replace(names(rendimiento), "\n", "_")
names(rendimiento)<-str_replace(names(rendimiento), " ", "_")
stop.col<-grep("achicoria_industrial", names(rendimiento))

grep(pattern = "total", names(rendimiento), value=T) # especies con varriedades


#organizacion y homogeneizacion 
rendimiento[1:355,] %>% 
  filter(región!="") %>% # filtrar las filas vacias y los comentarios extras al final de del campo Año s
  select(c(1:stop.col)) %>%
  separate(., col=región,into = c("cod_region", "nom_region"), sep=2 ) %>%
  mutate_at(vars(4:ncol(.)), extract_numeric) %>% 
  mutate_if(is.character, tolower)%>%
  mutate_if(is.character, str_trim) %>%
  mutate(año=parse_number(año_agrícola)) %>%
  mutate(año=year(parse_date(as.character(año), format="%Y"))) %>% 
  
  #identificacion de las especies de cultivos clasificados con variedades y las que no, se indica como "noidentificada" a las especies donde su variedad no es informada
  rename_at(., vars(!contains(c("trigo", "cebada", "maíz", "poroto","lupino")), -(c(año_agrícola:nom_region, año ))),  list( ~paste0(., "_noidentificada") )) %>%
  mutate(trigo_noidentificada=if_else(is.na(trigo_harinero)==TRUE & is.na(trigo_candeal)==TRUE,trigo_total, 0 )) %>% 
  mutate(cebada_noidentificada=if_else(is.na(cebada_cervecera)==TRUE & is.na(cebada_forrajera)==TRUE,cebada_total, 0 )) %>%
  mutate(maíz_noidentificada=if_else(is.na(maíz_consumo)==TRUE & is.na(maíz_semilla)==TRUE,maíz_total, 0 )) %>%
  mutate(lupino_noidentificada=if_else(is.na(lupino_amargo)==TRUE & is.na(lupino_australiano)==TRUE & is.na('lupino_australiano y dulce')==TRUE  ,lupino_total, 0 )) %>%
  mutate(poroto_noidentificada=if_else(is.na(poroto_consumo)==TRUE & is.na(poroto_exportación)==TRUE,poroto_total, 0 )) %>%
  select_at(., vars(!contains("total"))) %>%
  gather(variedad, rendimiento_ton_ha, -año ,-año_agrícola, -cod_region, -nom_region) %>%
  mutate(rendimiento_ton_ha=rendimiento_ton_ha/10) %>% #los valores se encuntran en quintales: 1qq=100 kg
  separate(., col=variedad, into = c("especie", "variedad"), sep="_")-> rendimiento_variedad

rendimiento_variedad[c("año", "año_agrícola","cod_region", "nom_region", "especie", "variedad", "rendimiento_ton_ha")]-> rendimiento_variedad

rendimiento_variedad %>% 
  group_by(año, año_agrícola, cod_region, nom_region, especie) %>%
  summarise(rendimiento_ton_ha=sum(rendimiento_ton_ha, na.rm = T)) ->rendimiento_especie


#revisar
rendimiento_variedad$especie %>% unique()
rendimiento_variedad$variedad %>% unique()

rendimiento_variedad %>%
  filter(especie=="tomate")

#exportar
setwd(out.rds)
write_rds(rendimiento_especie , "cultivos_rendimiento_especie.rds")
write_rds(rendimiento_variedad ,"cultivos_rendimiento_variedad.rds")

 

#Datos de Productividad ----
dir(rutas, pattern = "cultivos", full.names = T)%>% 
  read_excel(sheet = 4, skip=3) ->productividad

names(productividad)<-tolower(names(productividad))
names(productividad)<-str_replace(names(productividad), "\n", "_")
names(productividad)<-str_replace(names(productividad), " ", "_")
stop.col<-grep("achicoria_industrial", names(productividad))

grep(pattern = "total", names(productividad), value=T) # especies con varriedades


#organizacion y homogeneizacion 
tail(productividad[1:355,])
productividad[1:355,] %>% 
  filter(región!="") %>% # filtrar las filas vacias y los comentarios extras al final de del campo Año s
  select(c(1:stop.col)) %>%
  separate(., col=región,into = c("cod_region", "nom_region"), sep=2 ) %>%
  mutate_at(vars(4:ncol(.)), extract_numeric) %>% 
  mutate_if(is.character, tolower)%>%
  mutate_if(is.character, str_trim) %>%
  mutate(año=parse_number(año_agrícola)) %>%
  mutate(año=year(parse_date(as.character(año), format="%Y"))) %>% 
  
  #identificacion de las especies de cultivos clasificados con variedades y las que no, se indica como "noidentificada" a las especies donde su variedad no es informada
  rename_at(., vars(!contains(c("trigo", "cebada", "maíz", "poroto","lupino")), -(c(año_agrícola:nom_region, año ))),  list( ~paste0(., "_noidentificada") )) %>%
  mutate(trigo_noidentificada=if_else(is.na(trigo_harinero)==TRUE & is.na(trigo_candeal)==TRUE,trigo_total, 0 )) %>% 
  mutate(cebada_noidentificada=if_else(is.na(cebada_cervecera)==TRUE & is.na(cebada_forrajera)==TRUE,cebada_total, 0 )) %>%
  mutate(maíz_noidentificada=if_else(is.na(maíz_consumo)==TRUE & is.na(maíz_semilla)==TRUE,maíz_total, 0 )) %>%
  mutate(lupino_noidentificada=if_else(is.na(lupino_amargo)==TRUE & is.na('lupino_australiano y dulce')==TRUE  ,lupino_total, 0 )) %>%
  mutate(poroto_noidentificada=if_else(is.na(poroto_consumo)==TRUE & is.na(poroto_exportación)==TRUE,poroto_total, 0 ))  %>%
  select_at(., vars(!contains("total"))) %>%
  gather(variedad, productividad_ton_ha, -año ,-año_agrícola, -cod_region, -nom_region) %>%
  separate(., col=variedad, into = c("especie", "variedad"), sep="_")-> productividad_variedad

productividad_variedad[c("año", "año_agrícola","cod_region", "nom_region", "especie", "variedad", "productividad_ton_ha")]-> productividad_variedad

productividad_variedad %>% 
  group_by(año, año_agrícola, cod_region, nom_region, especie) %>%
  summarise(productividad_ton_ha=sum(productividad_ton_ha, na.rm = T)) ->productividad_especie


#revisar
productividad_variedad$especie %>% unique()
productividad_variedad$variedad %>% unique()

productividad_variedad %>%
  filter(especie=="tomate")

#exportar
setwd(out.rds)
write_rds(productividad_especie , "cultivos_productividad_especie.rds")
write_rds(productividad_variedad ,"cultivos_productividad_variedad.rds")