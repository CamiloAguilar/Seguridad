source('./scripts/funciones_Camilo.R')

load.lib('dplyr', 'reshape2', 'stringr', 'progress', 'lubridate', 'rjson')

## API Google elevation
# https://maps.googleapis.com/maps/api/elevation/json?locations=4.994178,-74.0524603&key=

#*********************************
## 1. Google places API ####
#*********************************

lat <- cajeros$lat[9]
lon <- cajeros$lon[9]

## Peticiones:
cajeros = readRDS(file = './results/def_Cajeros.RDS') %>% filter(!is.na(lat))
prueba<-48

cajeros$`DIRECCION DEL ATM`[prueba]
cajeros$CIUDAD[prueba]
cajeros$lat[prueba]
cajeros$lon[prueba]



API_key = rjson::fromJSON(file = './key/api_key.json')
API_key$google_api
p <- search_API(cajeros$lat[prueba], cajeros$lon[prueba],
                API_key = API_key$google_api) 

summary(p)
p$results[[1]]


length(p$results)
p$results[[1]]$name
p$results[[1]]$vicinity
p$results[[1]]$geometry$location$lat
p$results[[1]]$geometry$location$lng
p$results[[1]]$rating
p$results[[1]]$user_ratings_total

#****************************************
## 2. Buscando para todos los cajeros ####
#****************************************

## definiendo objeto
consulta_ATMs <- list()
fin <- 3 #length(cajeros$lat)

for (i in 48:length(cajeros$lat)) {
  p <- search_API(cajeros$lat[i], cajeros$lon[i],
                  API_key = API_key$google_api)
  
  consulta_ATMs[[i]] <- list(ATM = cajeros$`NOMBRE DEL ATM`[i], 
                             City = cajeros$CIUDAD[i],
                             Direction = cajeros$`DIRECCION DEL ATM`[i],
                             lat = cajeros$lat[i],
                             lon = cajeros$lon[i])
  
  ## Extraemos información de cajeros NO BBVA
  cont <- 1
  neighborhood <- list()
  if(length(p$results) > 0){
    for (j in 1:length(p$results)){
      if(!grepl('[Bb][Bb][Vv][Aa]', p$results[[j]]$name)){
        neighborhood[[cont]] <- list(name = p$results[[j]]$name,
                                     direction = p$results[[j]]$vicinity,
                                     location = p$results[[j]]$geometry$location,
                                     rating = p$results[[j]]$rating,
                                     user_ratings_total = p$results[[j]]$user_ratings_total)
        cont <- cont + 1
      }
    }
  }
  
  consulta_ATMs[[i]]$total_neighborhood <- length(neighborhood)
  consulta_ATMs[[i]]$neighborhood <- neighborhood
  
}
names(consulta_ATMs) <- str_replace_all(cajeros$`COD DEL CAJERO`, ' ', '_')


## Extrae información sobre los cajeros en el vecindario
total_otros_ATMs <- c()
for (i in 1:length(consulta_ATMs)) {
  total_otros_ATMs <- c(total_otros_ATMs, consulta_ATMs[[i]]$total_neighborhood)
}

cajeros$total_otros_ATMs <- total_otros_ATMs


#**************************************************
## 3. Definición de red de cajeros en COL ####
#**************************************************
red_ATH <- '[Aa][Tt][Hh]|[Bb][Oo][Gg][Oo][Tt]|[Pp][Oo][Pp][Uu][Ll][Aa][Rr]|[Oo][Cc][Cc][Ii][Dd][Ee][Nn][Tt][Ee]|[Vv][Ii][Ll][Ll][Aa][Ss]|[Aa][Vv][Aa][Ll]'

red_Bancolombia <- '[Bb][Aa][Nn][Cc][Oo][Ll][Oo][Mm][Bb][Ii][Aa]'

red_Davivienda <- '[Dd][Aa][Vv][Ii][Vv][Ii][Ee][Nn][Dd][Aa]'

red_CajaSocial <- '[Cc][Aa][Jj][Aa]|[Ss][Oo][Cc][Ii][Aa][Ll]'

red_Citi <- '[Cc][Ii][Tt][Ii]|[Cc][Ii][Tt][Yy]|[Cc][Ii][Tt][Ii][Bb][Aa][Nn][Kk]'
  
red_Colpatria <- '[Cc][Oo][Ll][Pp][Aa][Tt][Rr][Ii][Aa]|[Ss][Cc][Oo][Tt][Ii][Aa] |[Mm][Uu][Ll][Tt][Ii][Bb][Aa][Nn][Cc][Aa]'

red_Servibanca <- '[Ss][Ee][Rr][Vv][Ii]|[Ss][Ee][Rr][Vv][Ii][Bb][Aa][Nn][Cc][Aa]'

red_Itau <- '[Ii][Tt][Aa][Uu]|[Ii][Tt][Aa][Úú]|[Hh][Ee][Ll][Mm]'

red_BBVA <- '[Bb][Bb][Vv][Aa]'

red_Agrario <- '[Aa][Gg][Rr][Aa][Rr][Ii][Oo]'

red_Falabella <- '[Ff][Aa][Ll][Aa][Bb][Ee][Ll][Ll][Aa]'

red_Pichincha <- '[Pp][Ii][Cc][Hh][Ii][Nn][Cc][Hh][Aa]'

redCajerosCOL <- data.frame(Banco = c('ATH', 'Bancolombia', 'Davivienda', 'Caja_Social', 
                                      'Citibank', 'Colpatria', 'Servibanca', 'Itau',
                                      'BBVA', 'Agrario', 'Falabella', 'Pichincha'),
                            Clave = c(red_ATH, red_Bancolombia, red_Davivienda, red_CajaSocial,
                                      red_Citi, red_Colpatria, red_Servibanca, red_Itau,
                                      red_BBVA, red_Agrario, red_Falabella, red_Pichincha))

redCajerosCOL$Banco <- as.character(redCajerosCOL$Banco)
redCajerosCOL$Clave <- as.character(redCajerosCOL$Clave)

#*************************************************************
## Definimos el números de cajeros vecinos de cada red
## usando el código a continuación.
#*************************************************************

## Definimos DataFrame
cajerosVecinos <- data.frame(cod_Cajero = cajeros$`COD DEL CAJERO`)
cajerosVecinos$Vecinos_Raw <- NA
cajerosVecinos$BBVA <- NA
cajerosVecinos$Bancolombia <- NA
cajerosVecinos$Davivienda <- NA
cajerosVecinos$ATH <- NA
cajerosVecinos$Caja_Social <- NA
cajerosVecinos$Citibank <- NA
cajerosVecinos$Colpatria <- NA
cajerosVecinos$Servibanca <- NA
cajerosVecinos$Itau <- NA
cajerosVecinos$Agrario <- NA
cajerosVecinos$Falabella <- NA
cajerosVecinos$Otros <- NA


## Alimentamos el dataframe definido anteriormente
total_cajeros <- length(cajeros$`COD DEL CAJERO`) 
pb <- progress_bar$new(total = total_cajeros)

for (i in 1:total_cajeros){
  pb$tick()
  others <- c()
  others_Normalized <- c()
  long <- consulta_ATMs[[i]]$total_neighborhood
  if (long > 0){
    for (j in 1:long) {
      others <- c(others, consulta_ATMs[[i]]$neighborhood[[j]]$name)
      others_Normalized <- c(others_Normalized, 
                             case_when(
                               grepl(redCajerosCOL$Clave[1], others[j]) ~ redCajerosCOL$Banco[1],
                               grepl(redCajerosCOL$Clave[2], others[j]) ~ redCajerosCOL$Banco[2],
                               grepl(redCajerosCOL$Clave[3], others[j]) ~ redCajerosCOL$Banco[3],
                               grepl(redCajerosCOL$Clave[4], others[j]) ~ redCajerosCOL$Banco[4],
                               grepl(redCajerosCOL$Clave[5], others[j]) ~ redCajerosCOL$Banco[5],
                               grepl(redCajerosCOL$Clave[6], others[j]) ~ redCajerosCOL$Banco[6],
                               grepl(redCajerosCOL$Clave[7], others[j]) ~ redCajerosCOL$Banco[7],
                               grepl(redCajerosCOL$Clave[8], others[j]) ~ redCajerosCOL$Banco[8],
                               grepl(redCajerosCOL$Clave[9], others[j]) ~ redCajerosCOL$Banco[9],
                               grepl(redCajerosCOL$Clave[10], others[j]) ~ redCajerosCOL$Banco[10],
                               grepl(redCajerosCOL$Clave[11], others[j]) ~ redCajerosCOL$Banco[11],
                               TRUE ~ 'Otros'
                             ))
      #print(j)
    }
    ## Completamos columna con todos los cajeros encontrados (nombres originales)
    Vecinos_Raw <- others[1]
    for(k in 2:length(others)){
      Vecinos_Raw <- paste(Vecinos_Raw, others[k], sep='|')
    }
    cajerosVecinos$Vecinos_Raw[i] <- Vecinos_Raw
    
    ## Agregamos resultado de conteo de cajeros vecinos en el dataframe
    p <- as.data.frame(table(others_Normalized))
    for(k in 1:nrow(p)){
      cajerosVecinos[i, which(names(cajerosVecinos) == p[k,]$others_Normalized)] <- p[k,]$Freq
    }
  }
}

cajerosVecinos[is.na(cajerosVecinos)] <- 0

consulta_ATMs[[766]]$neighborhood


## Guardamos tabla definitiva
saveRDS(cajerosVecinos, file = "./data/BBVA/data_wrangling/cajeros_vecinos/201907_cajeros_vecinos.RDS")
write.csv2(cajerosVecinos, file = "./data/BBVA/data_wrangling/cajeros_vecinos/201907_cajeros_vecinos.csv")


#*********************************
## Análisis de correlación ####
#*********************************
cajerosVecinos <- readRDS("./data/BBVA/data_wrangling/cajeros_vecinos/201907_cajeros_vecinos.RDS")
geo_vandalismo <- readRDS("./data/BBVA/data_wrangling/vandalismo/201907-geo_vandalismo.RDS") %>%
                  filter(!is.na(`Fecha apertura`))

cruce <- geo_vandalismo %>%
         filter(year(Mes) == 2019 & month(Mes) == 6) %>%
         group_by(cod_Cajero = `Código ATM`) %>%
         summarise(n=n())
  
cruce = merge(cruce, cajerosVecinos, by='cod_Cajero') %>%
        select(-(cod_Cajero), -(Vecinos_Raw), -(BBVA))

correlacion <- as.data.frame(cor(cruce))
correlacion$n


cruce2 <- cruce[,2:ncol(cruce)]
cruce2[cruce2 > 0] <- 1
cruce2$n <- cruce$n


correlacion <- as.data.frame(cor(cruce2))
correlacion$n

geo_vandalismo %>% group_by(wday(`Fecha apertura`)) %>%
                   summarise(n=n())

#View(table(geo_vandalismo$CIUDAD))

d_pay <- function(fecha){
  require(lubridate)
  fecha <- as.Date.character(fecha)
  
  ## primer día hábil del mes
  fday <- as.Date.character(paste0(year(fecha),'-', 
                                   str_pad(month(fecha), 2, pad='0'),'-', 
                                   '01'))
  fday <- ifelse(wday(fday) < 6, fday,
                 ifelse(wday(fday) == 6, fday + 2, 
                        fday + 1))
  fday <- as.Date.numeric(fday, origin = '1970-01-01')
  
  ## último día del mes
  lday <- as.Date.character(paste0(year(fday + 35),'-', 
                                   str_pad(month(fday + 35), 2, pad='0'),'-', 
                                   '01')) -1
  lday <- ifelse(wday(lday) < 6, lday,
                 ifelse(wday(lday) == 6, lday - 1, lday - 2))
  lday <- as.Date.numeric(lday, origin = '1970-01-01')
  
  ## día 15 del mes (hábil)
  mday <- as.Date.character(paste0(year(fecha),'-', 
                                   str_pad(month(fecha), 2, pad='0'),'-', 
                                   '15'))
  mday <- ifelse(wday(mday) < 6, mday,
                 ifelse(wday(mday) == 6, mday - 1, mday - 2))
  mday <- as.Date.numeric(mday, origin = '1970-01-01')
  
  return(c(fday, mday, lday))
}

fecha <- '2019-02-01'
d_pay('2019-02-01')


fecha <- as.Date.character(geo_vandalismo$`Fecha apertura`)
es_quincena <- NULL

for(i in 1:length(fecha)){
  es_quincena <- c(es_quincena, 
                   ifelse(fecha[i] %in% d_pay(fecha[i]), T, F))
}

length(es_quincena)
table(es_quincena)
geo_vandalismo$es_quincena <- es_quincena

p <- geo_vandalismo %>% 
  filter(CIUDAD == 'BOGOTÁ, D.C.') %>%
  group_by(day(`Fecha apertura`), es_quincena) %>%
  summarise(n=n())

p <- geo_vandalismo %>% 
  filter(CIUDAD == 'BOGOTÁ, D.C.') %>%
  group_by(Fecha = as.Date.character(`Fecha apertura`)) %>%
  summarise(n=n())


d_pay('2019-03-01')


plot(p$Fecha, p$n, type = 'l',
     xlab= 'Fecha', ylab='n° Vandalismos')
abline(v = as.numeric(as.Date.character("2019-03-03")), col='red')
abline(v = as.numeric(as.Date.character("2019-03-14")), col='red')
abline(v = as.numeric(as.Date.character("2019-03-31")), col='red')

abline(v = as.numeric(as.Date.character("2019-05-01")), col='red')
abline(v = as.numeric(as.Date.character("2019-05-15")), col='red')
abline(v = as.numeric(as.Date.character("2019-05-30")), col='red')

as.numeric(min(p$Fecha))
