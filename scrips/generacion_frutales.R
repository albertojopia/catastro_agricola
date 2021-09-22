
library(readxl)
library(tidyverse)
library(lubridate)
library(plotly)

rutas<-"E:/ALBERTO/HEMERA/PROYECTO/CATASTRO_AGRICOLA/datos/excel"
out.rds<-"E:/ALBERTO/HEMERA/PROYECTO/CATASTRO_AGRICOLA/output/rds"


## Datos de superficie ----
dir(rutas, pattern = "frutales", full.names = T) %>% 
  read_excel(sheet = 2, skip=2) %>%
  rename("Superficie_ha"=`Superficie (ha)`) %>%
  mutate_if(is.character, tolower)->superficie 

names(superficie)<-tolower(names(superficie))
superficie[c("año", "región","comuna","especie","superficie_ha")]->superficie

# revisar
superficie$especie %>% unique()



## Datos de rendimento ----
dir(rutas, pattern = "frutales", full.names = T) %>% 
  read_excel(sheet = 1, skip=2) %>%
  rename("rendimiento_ton_ha"=`Promedio de kilos/hectárea`) %>%
  mutate("rendimiento_ton_ha"=`rendimiento_ton_ha`/1000) %>% #los valores estan en kg/ha
  mutate_if(is.character, tolower) ->rendimiento

names(rendimiento)<-tolower(names(rendimiento))
rendimiento[c("año", "región","comuna","especie","rendimiento_ton_ha")]->rendimiento

#revisar
rendimiento$especie %>% unique()


##Exportar

setwd(out.rds)
write_rds(superficie , "frutales_superficie.rds")
write_rds(rendimiento , "frutales_rendimiento.rds")



## Vizualizacion
## regiones
superficie %>%
  group_by(Región, Año, Especie) %>%
  summarise(Superficie_ha=sum(Superficie_ha, na.rm = T)) %>%
  filter(Región %in% c("Maule","Ñuble")) %>%
  ggplot(aes(Especie,Superficie_ha, fill=Especie)) +
  #scale_color_brewer(palette = 'Spectral')+
  geom_col()+ 
  facet_grid(Región~Año)+
  #theme_bw()+
  theme(axis.text.x =  element_blank(),
        legend.position = "bottom")->col.sup

plotly::ggplotly(col.sup)


rendimiento %>%
  group_by(Región, Año, Especie) %>%
  summarise(`ton/ha`=sum(`kg/ha`, na.rm=T)/1000) %>%
  filter(Región %in% c("Maule","Ñuble")) %>%
  ggplot(aes(Especie,`ton/ha`, fill=Especie)) +
  geom_col()+ 
  facet_grid(Región~Año)+
  theme(axis.text.x = element_text(angle=90,  hjust = 1),
        legend.position = "bottom")
  

## comunas

superficie %>%
  filter(Comuna=="CAUQUENES") %>%
  filter(Región %in% c("Maule")) %>%
  ggplot(aes(Especie,Superficie_ha, fill=Especie)) +
  #scale_color_brewer(palette = 'Spectral')+
  geom_col()+ 
  facet_grid(Comuna~Año)+
  #theme_bw()+
  theme(axis.text.x =  element_blank(),
        legend.position = "bottom")

##exportar

setwd(out.rds)
write_rds(superficie , "frutales_superficie.rds")
write_rds(rendimiento , "frutales_rendimiento.rds")


