source('./scripts/funciones_Camilo.R')

load.lib('dplyr', 'reshape2', 'readxl', 'ggmap', 'RCurl', 'rjson', 'stringr', 'progress',
         'rgeos', 'sp', 'maptools', 'car', 'geoR', 'gstat', 'gdata', 'geosphere', 'lubridate',
         'rgdal', 'htmlwidgets', 'rlist')

#***********************
## 1. Cargar datos #####
#***********************
Oficinas = readRDS(file = './data/BBVA/data_wrangling/oficinas/201907-Oficinas.RDS') %>% 
           filter(!is.na(lat)) %>%
           select(-(Num_Eventos))
cajeros = readRDS(file = './data/BBVA/data_wrangling/cajeros/201907_def_Cajeros.RDS') %>% 
         filter(!is.na(lat))
HEF_total = rbind(readRDS(file = './results/HEF_2015.RDS') %>% filter(!is.na(lat)),
                  readRDS(file = './results/HEF_2016.RDS') %>% filter(!is.na(lat)),
                  readRDS(file = './results/HEF_2017.RDS') %>% filter(!is.na(lat)),
                  readRDS(file = './results/HEF_2018.RDS') %>% filter(!is.na(lat)),
                  readRDS(file = './results/HEF_2019.RDS') %>% filter(!is.na(lat)))
HEF_total$Fecha = as.Date.numeric(as.numeric(HEF_total$Fecha), origin = '1899-12-30')


#********************************
## 2. Agregar información #####
#********************************

Tipo <- c(rep('Oficina', length(Oficinas$lat)), rep('ATM', length(cajeros$lat)))
Nombre <- c(Oficinas$`NOMBRE OFICINA`, cajeros$`NOMBRE DEL ATM`)
Lat <- c(Oficinas$lat, cajeros$lat)
Lon <- c(Oficinas$lon, cajeros$lon)
Direccion <- c(Oficinas$DIRECCIÓN, cajeros$`DIRECCION DEL ATM`)

total_bbva <- data.frame(Tipo=Tipo, Nombre=Nombre, Direccion=Direccion, lat=Lat, lon=Lon)

total_bbva[grepl('^[Bb][Aa][Rr][Rr][Aa][Nn]', total_bbva$Nombre), ]



#**************************
## Agrupamos eventos ####
#**************************
HEF_total = readRDS('./results/HEF_total_2015_2019.RDS')

HEF_total$Arma <- ifelse(HEF_total$`Arma empleada` %in% c('ARMA BLANCA / CORTOPUNZANTE', 'PUNZANTES'), 'knife',
                         ifelse(HEF_total$`Arma empleada` == 'ARTEFACTO EXPLOSIVO/CARGA DINAMITA', 'dinamite',
                                ifelse(HEF_total$`Arma empleada` == 'ARMA DE FUEGO', 'gun', 
                                       ifelse(HEF_total$`Arma empleada` == 'PALANCAS', 'lever', 
                                              ifelse(HEF_total$`Arma empleada` == 'CONTUNDENTES', 'blunt_weapon', 
                                                     ifelse(HEF_total$`Arma empleada` == 'SIN EMPLEO DE ARMAS',  
                                                            'disarmed','others'))))))

table(HEF_total$Arma)
table(HEF_total$Arma , HEF_total$`Arma empleada`)

#***********************************************************************
## calculamos número de eventos tipificados por arma para cada oficina
#***********************************************************************
Oficinas_eventos <- Oficinas
types = unique(HEF_total$Arma)
df_list = list()
for(type in types){
  BD <- HEF_total %>%
        filter(Arma == type)
  
  p_events <- centroids_finder(BD)
  Oficinas_eventos$acumulator <- events_acum(locations = Oficinas, events_points = p_events, r = 0.5)
  Oficinas_eventos$type = type
  df_list <- list.append(df_list, Oficinas_eventos)
}
length(df_list)

Oficinas_eventos <- bind_rows(df_list)

Oficinas_eventos <- Oficinas_eventos %>%
                    dcast(CIUDAD + `NOMBRE OFICINA` + DIRECCIÓN + lat + lon ~ type, 
                          value.var = 'acumulator',
                          fun.aggregate = sum) 

Oficinas_eventos$total_events <- rowSums(Oficinas_eventos[,tail(names(Oficinas_eventos), 
                                                                length(types))])

Oficinas_eventos = Oficinas_eventos %>% 
                   arrange(desc(total_events))

write.table(Oficinas_eventos, 
            file = './results/Seguridad oficinas//201907_Riesgo_Oficinas.csv', 
            sep = ';', row.names = F, dec = ',')

#***********************************************************************************************************
#***********************************************************************************************************
#***********************************************************************************************************


#*************************************************
## Agregación casos de vandalismo x ATM
#************************************************
## leerla >> C:\Users\c804324\Documents\GitHub\Seguridad\data\BBVA\data_wrangling\vandalismo
## 201907-geo_vandalismo.RDS
geo_vandalismo <- readRDS('./data/BBVA/data_wrangling/vandalismo/201907-geo_vandalismo.RDS')
month(geo_vandalismo$Mes)

agg_vandalismo <- geo_vandalismo %>%
                  group_by(`NOMBRE DEL ATM`) %>%
                  summarise(n=n(), lat = first(lat), lon=first(lon), date = max(Mes)) %>%
                  mutate(last_date = paste(year(date), 
                                           str_pad(month(date), width = 2, pad='0'),
                                           sep = '-'))


#*************************************************
## Hurtos EF + vandalismos BBVA
#*************************************************
vanda_Hurto_1 <- HEF_total %>% 
                 select(Fecha, Departamento, Municipio, Barrio, Clase=`Clase de sitio`, Arma=Arma,
                        lat, lon) %>%
                 mutate(Evento = 'Hurto Bancos')

names(geo_vandalismo)
vanda_Hurto_2 <- geo_vandalismo %>% 
                 select(Fecha=Mes, Departamento=`Nombre del \r\nDepartamento`,
                        Municipio=CIUDAD, Barrio=`DIRECCION DEL ATM`, Clase=Modulo, lat, lon) %>%
                 mutate(Arma = "vandalism", Evento = 'Hurto Bancos') %>%
                 select(Fecha, Departamento, Municipio, Barrio, Clase, Arma, lat, lon, Evento)

vanda_Hurto <- rbind(vanda_Hurto_1, vanda_Hurto_2)
rm(vanda_Hurto_1, vanda_Hurto_2)

#********************************************************************************************************
#********************************************    MAPA    ************************************************
#********************************************************************************************************

#*************************************************
## Comportamiento General del Hurto a Bancos
#*************************************************
library(leaflet)

labs_hurto <- lapply(seq(nrow(HEF_total)), function(i) {
  paste0( '<p>', paste0('Clase: ', HEF_total$`Clase de sitio`[i]), '</p>', 
          paste0('Arma empleada: ', HEF_total$`Arma empleada`[i]), '</p> ', 
          paste0('Lugar: ', HEF_total$Barrio[i], '-', HEF_total$Municipio[i]), '</p> ',
          paste0('Fecha: ', 
                 year(HEF_total$Fecha[i]), '-', 
                 str_pad(month(HEF_total$Fecha[i]), 2, pad = '0')),
          '</p><p>') 
})

labs_vandal <- lapply(seq(nrow(geo_vandalismo)), function(i) {
  paste0( '<p>', paste0('Módulo: ', geo_vandalismo$Modulo[i]), '</p>', 
          paste0('Ubicación: ', geo_vandalismo$`TIPO DE UBICACIÓN`[i]), '</p> ', 
          paste0('Lugar: ', geo_vandalismo$`DIRECCION DEL ATM`[i], '-', geo_vandalismo$CIUDAD[i]), '</p> ',
          paste0('Fecha: ', 
                 year(geo_vandalismo$Mes[i]), '-', 
                 str_pad(month(geo_vandalismo$Mes[i]), 2, pad = '0')),
          '</p><p>') 
})



# Make a list of icons. We'll index into it based on name.
size = 100
anchor = 0
BBVAIcons <- iconList(
  thief_1 = makeIcon("./icon/thief-1.png", "./icon/thief-1.png", size, size, iconAnchorX = anchor),
  thief_2 = makeIcon("./icon/thief-2.png", "./icon/thief-2.png", size, size, iconAnchorX = anchor),
  others = makeIcon("./icon/thief-3.png", "./icon/thief-3.png", size*0.7, size*0.7, iconAnchorX = anchor),
  knife = makeIcon("./icon/knife-512.png", "./icon/knife-512.png", size, size, iconAnchorX = anchor),
  gun = makeIcon("./icon/Revolver-512.png", "./icon/Revolver-512.png", size, size, iconAnchorX = anchor),
  blunt = makeIcon("./icon/blunt_weapon-512.png", "./icon/blunt_weapon-512.png", size, size, iconAnchorX = anchor),
  dinamite = makeIcon("./icon/dynamite-512.png", "./icon/dynamite-512.png", size, size, iconAnchorX = anchor),
  lever = makeIcon("./icon/lever-512.png", "./icon/lever-512.png", size, size, iconAnchorX = anchor),
  atm = makeIcon("./icon/atm_bbva.png", "./icon/atm_bbva.png", size*0.6, size*0.6, iconAnchorX = anchor),
  vandalism = makeIcon("./icon/vandalism-005.png", "./icon/vandalism-005.png", size*0.6, size*0.6, iconAnchorX = anchor)
)

# total_bbva$Tipo <- as.character(total_bbva$Tipo)
# Create a palette that maps factor levels to colors
pal <- colorFactor(c("navy", "red"), domain = c("Oficina", "ATM"))

map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  
  # Markers Oficinas
  addCircleMarkers(~lon, ~lat, popup = paste0('Tipo: ', total_bbva$Tipo, '-', total_bbva$Direccion), 
                   label = paste0('Servicio BBVA: ', total_bbva$Tipo, ' - ', total_bbva$Nombre),
                   weight = 5, stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~ifelse(Tipo == "ATM", 6, 10),
                   color = ~pal(Tipo), 
                   data = total_bbva)  %>%
  
  # Markers Hurtos 
  addMarkers(data = HEF_total,
    lng = HEF_total$lon, lat = HEF_total$lat,
    popup = ~as.character(`Clase de sitio`), 
    label = lapply(labs_hurto, htmltools::HTML),
    icon = ~BBVAIcons[Arma],
    clusterOptions = markerClusterOptions(),
    group = "Hurto_Bancos"
    ) %>%
  
  # Markers Vandalismo
  addMarkers(
    data = geo_vandalismo,
    lng = geo_vandalismo$lon, lat = geo_vandalismo$lat,
    #popup = ~as.character(geo_vandalismo$), 
    label = lapply(labs_vandal, htmltools::HTML),
    icon = ~BBVAIcons['vandalism'],
    clusterOptions = markerClusterOptions(),
    group = "Vandalismo"
  ) %>%
  
  
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner Lite", "Positron"),
    overlayGroups = c("Vandalismo", "Hurto_Bancos"),
    options = layersControlOptions(collapsed = FALSE)
  )

options("viewer" = function(url, ...) utils::browseURL(url))
map

# Export as HTML file
saveWidget(map, 'BBVA_map.html', selfcontained = FALSE)





