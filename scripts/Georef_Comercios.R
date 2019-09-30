source('./scripts/funciones_Camilo.R')
source('./scripts/fun.R')

load.lib('dplyr', 'reshape2', 'readxl', 'ggmap', 'RCurl', 'rjson', 'stringr', 'progress')


#https://maps.googleapis.com/maps/api/geocode/json?address=VDA%20ISAZA%20%2C%20BARBOSA%20%2C%20COLOMBIA&key=

#*********************************
## 1. Información Hurtos####
#*********************************

path <- './data/Policia'
(f <- list.files(path))
f[12]

excel_sheets(paste(path, f[12], sep="/"))

HEC <- read_excel(paste(path, f[12], sep="/"), sheet = 'Sheet1', range = "A9:M200000")
class(HEC$Fecha)
HEC$Fecha<-as.Date.character(HEC$Fecha)
fin = which(is.na(HEC$Fecha))[1] - 1
HEC = HEC[1:fin, ]
head(HEC)

#table(HEC$Municipio)
#HEC = filtro

location1 = paste(HEC$Barrio, HEC$Municipio, 'Colombia', sep=" , ")
location2 = paste(HEC$Municipio, HEC$Departamento, 'Colombia', sep=" , ")

## Tabla dinámica
BD_Georef <- data.frame(location1, location2) %>%
  group_by(location1, location2) %>%
  summarise(n=n())


API_key = rjson::fromJSON(file='./key/api_key.json')
key = API_key$google

location1[179]
geoClean(location1, type = "Zones")
geoClean(location2[1:6], type = "Zones")

#geoBBVA(key, location1[1], location2[1])

#**************************************
## Inicia Geo referenciación ####
#**************************************
zipcode = c()
lat = c()
lon = c()

pb <- progress_bar$new(total = length(BD_Georef$location1)-4473)
for(i in 4473:length(BD_Georef$location1)){
  pb$tick()
  p = geoBBVA_Zones(key = key, BD_Georef$location1[i], BD_Georef$location2[i])
  zipcode = c(zipcode, p$zipcode)
  lat = c(lat, p$geocode$lat)
  lon = c(lon, p$geocode$lng)
}

dim(BD_Georef)[1]
length(zipcode)
BD_Georef$zipcode = zipcode
BD_Georef$lat = lat
BD_Georef$lon = lon

#### resultados parciales del 1 al 179 #####

saveRDS(BD_Georef, file = './data/Policia/BD_Barrios_COL.RDS')


####pruebas####

locationf<- geoClean(BD_Georef$location2[181])
locationf<-str_replace_all(str_replace_all(locationf, pattern = " ", "%20"),
                           ",", "%2C")

saveRDS(BD_Georef, file = './data/Policia/BD_Georef.RDS')
get_json <- paste0(googleAPI_dir, locationf, '&key=', key)
j <- fromJSON(file = get_json)$results[[1]]$address_components
