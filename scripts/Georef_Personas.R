source('./scripts/funciones_Camilo.R')
source('./scripts/fun.R')

load.lib('dplyr', 'reshape2', 'readxl', 'ggmap', 'RCurl', 'rjson', 'stringr', 'progress', 'readxl',
         'lubridate', 'data.table', 'stringi')
actual_year <- year(Sys.Date())


#***********************
## Lectura info ####
#***********************

ruta <- './data/Policia/Personas'
files <- list.files(ruta)
(files <- files[grepl('^hurto', files)])


HP_acum <- list()
pb <- progress_bar$new(total = length(files))
cuenta <- 1
for (file in files) {
  pb$tick()
  nombre <- paste0('HP_', stri_extract(file, regex = '[0-9][0-9][0-9][0-9]'))
  temp <- read_excel(paste(ruta, file, sep="/"), range = "A10:T600000")
  temp$Fecha<-as.Date.character(temp$Fecha)
  fin = which(is.na(temp$Fecha))[1] - 1
  temp = temp[1:fin, ]
  #assign(nombre, temp)
  HP_acum[[cuenta]] <- temp
  cuenta <- cuenta + 1
  rm(temp); gc
}

#******************
## Tabla única 
#******************
HP_acum <- rbindlist(HP_acum)
HP_acum$llave <- paste(HP_acum$Barrio, HP_acum$Municipio, 'Colombia', sep=" , ")

BD_Barrios_COL <- readRDS('./data/Policia/BD_Barrios_COL.RDS')
HP_acum <- merge(HP_acum, BD_Barrios_COL, by.x = 'llave', by.y = 'location1',
                 all.x = TRUE, all.y = FALSE) %>%
           select(-(llave))


faltan <- HP_acum %>% filter(is.na(lat)) %>%
          mutate(location1 = paste(Barrio, Municipio, 'Colombia', sep=" , "),
                 location2 = paste(Municipio, Departamento, 'Colombia', sep=" , ")) %>%
          group_by(location1) %>%
          summarise(location2 = first(location2), n = n())


#**************************************
## Inicia Geo referenciación ####
#**************************************
zipcode = c()
lat = c()
lon = c()

API_key = rjson::fromJSON(file='./key/api_key.json')
key = API_key$google
pb <- progress_bar$new(total = length(faltan$location1))
for(i in 1:length(faltan$location1)){
  pb$tick()
  p = geoBBVA_Zones(key = key, faltan$location1[i], faltan$location2[i])
  zipcode = c(zipcode, p$zipcode)
  lat = c(lat, p$geocode$lat)
  lon = c(lon, p$geocode$lng)
}
p$geocode

dim(faltan)[1]
length(zipcode)
faltan$zipcode = zipcode
faltan$lat = lat
faltan$lon = lon


BD_Barrios_COL <- rbind(BD_Barrios_COL,
                        faltan %>% select(location1, zipcode, lat, lon))

saveRDS(BD_Barrios_COL, file = './data/Policia/BD_Barrios_COL.RDS')

#**********************************
## Revisa lat lon ####
#**********************************
limits <- list(lat = c(-4.23, 13.40),
               lon = c(-66.855, -81.734))

revisar <- BD_Barrios_COL %>%
           filter(( !between(lat, limits$lat[1], limits$lat[2])  ) |
                  ( !between(lon, limits$lon[2], limits$lon[1])  ))

pa_cruzar <- HEC %>% mutate(location1 = paste(Barrio, Municipio, 'Colombia', sep=" , "),
                                location2 = paste(Municipio, Departamento, 'Colombia', sep=" , ")) %>%
             group_by(location1) %>%
             summarise(location2 = first(location2))
    
revisar <- merge(revisar, pa_cruzar,
                 by='location1', all.x = T, all.y = F)

zipcode = c()
lat = c()
lon = c()
pb <- progress_bar$new(total = length(revisar$location1))
for(i in 1:length(revisar$location1)){
  pb$tick()
  p = geoBBVA_Zones(key = key, revisar$location1[i], revisar$location2[i])
  zipcode = c(zipcode, p$zipcode)
  lat = c(lat, p$geocode$lat)
  lon = c(lon, p$geocode$lng)
}

dim(revisar)[1]
length(zipcode)
revisar$zipcode = zipcode
revisar$lat = lat
revisar$lon = lon

revisar_1 <- revisar %>% filter(!is.na(location2))
BD_Barrios_COL <- rbind(BD_Barrios_COL, revisar %>% select(-(location2)))



#**********************************
## Cruce con tabla Personas ####
#**********************************
names(HP_acum)
HP_acum$llave <- paste(HP_acum$Barrio, HP_acum$Municipio, 'Colombia', sep=" , ")

HP_acum_2 <- merge(HP_acum %>% select(-(lat), -(lon), -(zipcode)), BD_Barrios_COL, by.x = 'llave', by.y = 'location1', all.x = T, all.y = F) %>%
             select(-(llave))




