source('./scripts/funciones_Camilo.R')
source('./scripts/fun.R')

load.lib('dplyr', 'reshape2', 'readxl', 'ggmap', 'RCurl', 'rjson', 'stringr', 'progress')


#https://maps.googleapis.com/maps/api/geocode/json?address=VDA%20ISAZA%20%2C%20BARBOSA%20%2C%20COLOMBIA&key=

#*********************************
## 1. Información Hurtos####
#*********************************

path <- './data/Policia'
(f <- list.files(path))

f[5]
HEF <- read_xls(paste(path, f[5], sep="/"), sheet = 'Sheet1', range = "A9:M1000")
fin = which(is.na(HEF$Fecha) | HEF$Fecha=='TOTAL')[1] - 1
HEF = HEF[1:fin, ]
head(HEF)

#table(HEF$Municipio)
#HEF = filtro

location1 = paste(HEF$Barrio, HEF$Municipio, 'Colombia', sep=" , ")
location2 = paste(HEF$Municipio, HEF$Departamento, 'Colombia', sep=" , ")

## Tabla dinámica
BD_Georef <- data.frame(location1, location2) %>%
             group_by(location1, location2) %>%
             summarise(n=n())


API_key = rjson::fromJSON(file='./key/api_key.json')
key = API_key$google

location1[1:6]
geoClean(location1, type = "Zones")
geoClean(location2[1:6], type = "Zones")

#geoBBVA(key, location1[1], location2[1])

#**************************************
## Inicia Geo referenciación ####
#**************************************
zipcode = c()
lat = c()
lon = c()

pb <- progress_bar$new(total = length(BD_Georef$location1))
for(i in 1:length(BD_Georef$location1)){
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

#*********************************
## Validación de los resultados
#*********************************

## Quitamos georeferencias iguales
limite = 5
revisar <- as.data.frame(table(as.character(HEF_total$lat))) %>%
  arrange(desc(Freq)) %>%
  filter(Freq>=limite)

i <- 9
unique(HEF_total[which(HEF_total$lat %in% revisar$Var1[i]), c('Barrio', 'Municipio', 'Departamento')])

#revisar_lat = c('4.6133379')
#unique(HEF_total[which(HEF_total$lat %in% revisar_lat),]$Barrio)
filtro = HEF_total %>% filter(as.character(lat) %in% revisar$Var1)

## Se corrigió corriendo proceso para las lat iguales
## Se pega el resultado de la corrección
HEF_total = HEF_total %>% filter(!(as.character(lat) %in% revisar$Var1))
HEF_total = rbind(HEF_total, HEF)

## Guardamos resultado
#saveRDS(HEF_total, './results/HEF_total_2015_2019.RDS')
HEF_total = readRDS('./results/HEF_total_2015_2019.RDS')


## Guardamos resultado
saveRDS(HEF, './results/HEF_2019.RDS')


#************************************************************************************************************
#************************************************************************************************************

#*********************************
## 2. Oficinas ####
#*********************************
path <- './data/BBVA'
(f <- list.files(path))

Oficinas <- read_xlsx(paste(path, f[2], sep="/"), sheet = 'Comercial', range = "A1:L1000")
fin = which(is.na(Oficinas$CIUDAD))[1] - 1
Oficinas = Oficinas[1:fin, ]
head(Oficinas)


location1 = paste(Oficinas$DIRECCIÓN, Oficinas$CIUDAD, 'Colombia', sep=" , ")

i <- which(grepl('SAN JOSÉ DEL GUAVIARE', Oficinas$`NOMBRE OFICINA`))
location1[i]
Oficinas$`NOMBRE OFICINA`[i]

Oficinas$lat[i] <- 2.5729269
Oficinas$lon[i] <- -72.644144


key = API_key$google

#p$results[[1]]$address_components[[8]]$long_name
#p$results[[1]]$formatted_address
#p$results[[1]]$geometry$location
#geoBBVA_Dir(key, location1[1])

zipcode = c()
lat = c()
lon = c()

pb <- progress_bar$new(total = length(location1))
for(i in 1:length(location1)){
  pb$tick()
  (p = geoBBVA_Dir(key = key, location1[i]))
  zipcode = c(zipcode, p$zipcode)
  lat = c(lat, p$geocode$lat)
  lon = c(lon, p$geocode$lng)
}

Oficinas$zipcode = zipcode
Oficinas$lat = lat
Oficinas$lon = lon

## Guardamos resultado
write.table(Oficinas, './results/Oficinas.csv', sep = ";", row.names = F, dec = ',')
saveRDS(Oficinas, './results/Oficinas.RDS')


faltan = Oficinas %>% filter(is.na(lat))

#************************************************************************************************************
#************************************************************************************************************

#*********************************
## 2. Cajeros ATM ####
#*********************************
path <- './data/BBVA'
(f <- list.files(path))

cajeros <- read_xlsx(paste(path, 'Listado ATMs 31122018.xlsx', sep="/"), 
                      sheet = 'Base BBVA', range = "A23:DC1500")
fin = which(is.na(cajeros$`COD DEL CAJERO`))[1] - 1
cajeros = cajeros[1:fin, ]
head(cajeros)

new_dir <- c()
for(j in 1:length(cajeros$`DIRECCION DEL ATM`)){
  ajuste <- fit.dir(cajeros$`DIRECCION DEL ATM`[j])
  new_dir[j] <- ifelse(!is.na(ajuste), ajuste, cajeros$`DIRECCION DEL ATM`[j])
}


location1 = paste(new_dir, cajeros$CIUDAD, 'Colombia', sep=" , ")
location1[i]
geoClean(location1[i], type = 'directions')
key = API_key$google

zipcode = c()
lat = c()
lon = c()

ini <- i
pb <- progress_bar$new(total = length(location1))
for(i in 1:length(location1)){
  pb$tick()
  (p = geoBBVA_Dir(key = key, location1[i]))
  zipcode[i] <- p$zipcode
  lat[i] <- p$geocode$lat
  lon[i] <- p$geocode$lng
}

zipcode[i] <- NA
lat[i] <- 4.7366726
lon[i] <- -74.1329722
cajeros$`NOMBRE DEL ATM`[i]

#View(table(paste0(lat, ',', lon)))

cajeros$zipcode = zipcode
cajeros$lat = lat
cajeros$lon = lon

which(is.na(cajeros$lat))
faltan = cajeros %>% filter(is.na(lat))

## Guardamos resultado
apply(cajeros, 2, class)
cajeros <- read.csv2('./results/def_Cajeros.csv', dec = ',', 
                      fill = T)
cajeros$lat[696]
cajeros$lat <- as.numeric(str_replace_all(cajeros$lat, ',', '.'))
cajeros$lon <- as.numeric(str_replace_all(cajeros$lon, ',', '.'))

names(cajeros) <- str_replace_all(names(cajeros), '[.]', ' ')

write.table(cajeros, './results/def_Cajeros.csv', sep = ";", row.names = F, dec = ',')
saveRDS(cajeros, './results/def_Cajeros.RDS')


## Geo_cajeros
## Con correcciones a mano
geo_cajeros <- read.table('./results/Cajeros.csv', sep = ";", dec = ',', header = T)
fin = which(is.na(geo_cajeros$lat))[1] - 1
geo_cajeros = geo_cajeros[1:fin, ]

def_cajeros <- base::merge(cajeros, geo_cajeros, by.x = 'NOMBRE DEL ATM', by.y='NOMBRE.DEL.ATM') %>%
               mutate(lat = lat.y, lon = lon.y) %>%
               select(-lat.x, -lat.y, -lon.x, -lon.y)


## Guardamos resultado
apply(def_cajeros, 2, class)
write.table(def_cajeros, './results/def_Cajeros.csv', sep = ";", row.names = F, dec = ',')
saveRDS(def_cajeros, './results/def_Cajeros.RDS')


#************************************************************************************************************
#************************************************************************************************************

#*********************************
## 3. Vandalismos ####
#********************************

vandalismo <- read_xlsx(paste(path, 'Detalle vandalismos BBVA.xlsx', sep="/"), 
                        sheet = 'Detalle vandalismos BBVA', range = "A1:AI15000")
fin = which(is.na(vandalismo$Mes))[1] - 1
vandalismo = vandalismo[1:fin, ]
head(vandalismo)

## Cruzamos con def_cajeros para georeferenciar
geo_vandalismo <- merge(vandalismo, 
                        def_cajeros[,c('COD DEL CAJERO', 'NOMBRE DEL ATM', 'lat', 'lon')],
                        by.x = 'Código ATM', by.y = 'COD DEL CAJERO', all.x=T, all.y=F)

## Cajeros que no cruzaron
unique(geo_vandalismo[which(is.na(geo_vandalismo$lat)), c('Código ATM')])

## Vandalismos con cajeros georeferenciados
geo_vandalismo <- geo_vandalismo %>% filter(!is.na(lat))
saveRDS(geo_vandalismo, file = './data/BBVA/data_wrangling/vandalismo/201907-geo_vandalismo.RDS')


#************************************************************************************************************
#************************************************************************************************************

#*********************************
## 4. Asaltos BBVA ####
#********************************
Asaltos_BBVA <- read_xlsx('./data/BBVA/Asaltos BBVA.xlsx', sheet = 'Asaltos')

resumen <- Asaltos_BBVA %>% 
           group_by(COD_SUCURSAL) %>%
           summarise(Suc = first(SUCURSAL), 
                     vrl_hurto = sum(`VALOR ILÍCITO`, na.rm=T)/1000000,
                     n = n()) %>%
           arrange(desc(n))

resumen

saveRDS(Asaltos_BBVA, file = './data/BBVA/data_wrangling/asaltos//201907-Asaltos_BBVA.RDS')






