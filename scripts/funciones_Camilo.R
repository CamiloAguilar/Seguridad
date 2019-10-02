

bbva_edad <- function(FNac, FAct){
  return(floor(as.numeric(FAct - FNac)/365.25))
}

run_mode <- function(){
  run_mode = ''
  while(!(run_mode %in% c(1, 2))){
    cat('\f')
    cat("\n Por favor, seleccione el modo de ejecución \n 1: Train \n 2: Test")
    run_mode <- readline('Ingrese el valor: ')
    ejecucion <- c('Train', 'Test')
    message('\n Ha seleccionado modo de ejecución para conjunto de ', ejecucion[as.numeric(run_mode)], '\n')
  }
  return(ejecucion[as.numeric(run_mode)])
}

load.lib<-function(...){
  #detach("package:data.table", unload=TRUE)
  #remove.packages("data.table")
  needs<-as.vector(list(...))
  libs<-as.data.frame(installed.packages(lib.loc = NULL, priority = NULL,
                                         noCache = FALSE, fields = NULL,
                                         subarch = .Platform$r_arch))
  libs<-libs$Package
  for(i in 1:length(needs)){
    if( needs[[i]] %in% libs ){
      message(" | Cargando libreria ", needs[[i]])
      library(eval(needs[[i]]), character.only=TRUE)
    } else{
      message("Libreria ", needs[[i]], " no se encuentra instalada... Instalando libreria ", needs[i])
      install.packages(eval(needs[[i]]), dependencies = T)
      message(" | Cargando libreria ", needs[[i]])
      library(eval(needs[[i]]), character.only=TRUE)
    }
  }
}

## Función de acumulación regla 414
f_acum_414 <- function(filtro){
  #filtro <- BD_reglas %>%
  #  select(HOD_key_day, ACF_Fecha_TRX, ACF_Hora_TRX, HOD_Cond_count414) %>%
  #  filter((HOD_key_day == trx$HOD_key_day) & (ACF_Hora_TRX < trx$ACF_Hora_TRX))
  
  return(sum(filtro$HOD_Cond_count414))
}


## Función para leer vlist
load_vlist <- function(folder){
  
  ## cargamos Vlist
  #folder = './data/vlist'
  vlist = list()
  cont=1
  for(i in list.files(folder)){
    vlist[[cont]] = readRDS(file = paste0(folder, '/', i))
    names(vlist[[cont]]) <- c('cod', 'description')
    cont = cont + 1
  }
  
  ## Cargamos nombres de los vlist
  nombres <- vector()
  for(i in str_split(list.files(folder), '[[.]]')){
    nombres <- c(nombres, i[2])
  }
  names(vlist) <- nombres
  #summary(vlist)
  
  return(vlist)
}

## Función de partición de clusters
cluster_partition <- function(BD_reglas, cl, grupo_0 = T){
  BD_list = list()
  ## Se asume group_0 como aquel grupo sobre el cual no se aplicará regla en
  ## Procesamiento paralelizado
  if(grupo_0 == F){
    for(i in 1:cl){
      BD_list[[i]] <- BD_reglas[BD_reglas$group == i,]
    }
    names(BD_list) <- paste0('group_', 1:cl)
  } else{
    for(i in 0:cl){
      BD_list[[i+1]] <- BD_reglas[BD_reglas$group == i,]
    }
    names(BD_list) <- paste0('group_', 0:cl)
  }
  message("\n Dimensión de cada grupo: \n")
  for(i in 0:cl) message(paste0(' dim group_', i, ' : '), dim(BD_list[[i+1]])[1])
  return(BD_list)
}

## identificar si el mes es el mismo, el anterior u otro (más de uno)


diff_months <- function(date1, date2){
  require(lubridate)
  fechas <- floor_date(c(date1, date2), 'month')
  dif <- diff.Date(fechas)
  #print(dif)
  result <- ifelse(dif == 0, 'mismo_mes', 
                   ifelse(dif < 32, 'cambio_1mes', 
                          'cambio_sup_1mes'))
  return(result)
}


get_rules <- function(modelo, umbral=0.5){
  require(rpart.plot)
  require(stringr)
  reglas <- rpart.plot::rpart.rules(modelo, style = 'tall', cover=T, nn=F, clip.facs = F)
  niveles <- c('VALIDA', 'FRAUDE')
  
  texto <- capture.output(reglas)
  texto <- c(texto, "")
  init_rule <- which(grepl('^Fraude', texto))+1
  end_rule <- which(texto == '')-1
  num_rules <- length(init_rule)
  
  op_math <- c('<', '>', '<=', '>=', '=', '!=')
  op_sets <- c('is', 'or')
  
  final_rules <- c()
  pred <- c()
  for(i in seq_along(init_rule)){
    rule <- texto[init_rule[i]:end_rule[i]]
    l <- length(rule)
    
    ## Obtenemos prob
    prob <- as.numeric(unlist(str_split(texto[init_rule[i] -1], " "))[3])
    decision <- ifelse(prob>umbral, niveles[1], niveles[2])
    
    for(j in 1:l){
      om <- op_math[which(op_math %in% unlist(str_split(rule[j], ' ')))]
      os <- op_sets[which(op_sets %in% unlist(str_split(rule[j], ' ')))]
      if(length(om) > 0){
        sp <- rule[j]
      } else{
        sp <- str_replace_all(rule[j], ' is ', " %in% c('")
        sp <- paste0(str_replace_all(sp, ' or ', "', '"), "')")
      }
      if(j == 1){
        toda <- sp
      } else {
        toda <- paste(toda, sp, sep=' & ')
      }
      
      
    } ## Fin for j
    final_rules <- c(final_rules, toda) 
    pred <- c(pred, decision)
  }
  
  return(list('rules'=final_rules, 'predictions'= pred))
}

## Función que determina la proporción de dinero detectado
## directamente por el modelo
savings_score <- function(y_true, y_pred, cost_Weights){
  cost_S <- sum(cost_Weights)
  cost_f_S <- sum(cost_Weights[y_true != y_pred])
  savings <- (cost_S - cost_f_S)/cost_S
  return(savings)
}

## Función para estimar el porcentaje de dinero salvado
## del total del Fraude marcado
savings_fraud <- function(y_true, y_pred, cost_Weights){
  cost_Fraud <- sum(cost_Weights[y_true == 'FRAUDE'], na.rm=T)
  cost_Detected <- sum(cost_Weights[y_true == 'FRAUDE' & y_true == y_pred], na.rm = T)
  return(cost_Detected/cost_Fraud)
}




geoClean = function(string, type='Zones'){
  string = toupper(string)
  string = gsub('Á', 'A', string)
  string = gsub('É', 'E', string)
  string = gsub('Í', 'I', string)
  string = gsub('Ó', 'O', string)
  string = gsub('Ú', 'U', string)
  string = gsub(' \\(CT)', '', string)
  string = gsub(' D[.]C[.]', '', string)
  string = gsub('VER[.] ', '', string)
  string = gsub('NO[.] ', ' ', string)
  string = gsub('URB[.] ', '', string)
  string = gsub('BARRIO ', '', string)
  string = gsub('B[.] ', '', string)
  string = gsub('Ñ', 'N', string)
  string = gsub(' ANDES E10 ', '', string)
  string = gsub('ALAMOS SUR', 'ALAMOS', string)
  string = gsub('SUBA CENTRO', 'CENTRO SUBA', string)
  string = gsub('CEDRO GOLF EL RODEO', 'CEDRO GOLF', string)
  string = gsub('PUENTE ARANDA', 'CARCEL MODELO', string)
  string = gsub('BOMBONA 1', 'BOMBONA', string)
  string = gsub('[.]', ' ', string)
  
  string = gsub(' E[0-9][0-9] ', '', string)
  string = gsub(' E[0-9] ', '', string)
  string = gsub('[.]', '', string)
  if(type == 'Zones'){
    string = gsub("[^\\s]*-[^\\s]*", "", string, perl=T)
  } else{
    string = gsub('-', ' ', string)
    string = gsub(' NO ', ' ', string)
    string = gsub('#', ' ', string)
  }
  string = gsub('  ', ' ', string)
  
  return(string)
}

geoBBVA_Zones <- function(key, location1, location2){
  require('stringr')
  googleAPI_dir <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  location1 = geoClean(location1)
  print(location1)
  location1 = str_replace_all(str_replace_all(location1, pattern = " ", "%20"),
                             ",", "%2C")
  get_json1 <- paste0(googleAPI_dir, location1, '&key=', key)
  
  location2 = geoClean(location2)
  location2 = str_replace_all(str_replace_all(location2, pattern = " ", "%20"),
                              ",", "%2C")
  get_json2 <- paste0(googleAPI_dir, location2, '&key=', key)
  
  ## Get geocode
  message("\n... Consultando API Google ", get_json1)
  raw_data <- tryCatch({
    p = fromJSON(file = get_json1)#$results[[1]]$address_components
    if(p$status == 'OK'){
      components = p$results[[1]]$address_components
      if(length(components) < 3){
        print('location1 no encontrada... buscando location2')
        print(location2)
        message("\n... Consultando API Google ", get_json2)
        fromJSON(file = get_json2)
      } else {
        p #fromJSON(file = get_json1)
      }
    } else {
      print('location1 no encontrada... buscando location2')
      print(location2)
      message("\n... Consultando API Google ", get_json2)
      fromJSON(file = get_json2)
    }
  },
  error = function(cond){
    message('\n ***** Imposible obtener información para ', location,'  *****\n')
    message(cond)
    
    return(NA)
  })
  
  ## Get important info
  if(raw_data$status != 'OK' | is.na(raw_data[1])){
    message('\n ***** Imposible obtener información para ', location,'  *****\n')
    message('status: ', raw_data$status)
    res_zipCode = NA
    res_geoCode = list(lat = NA, lng = NA)
    } else if(length(raw_data$results[[1]]$address_components) < 6){
        res_zipCode = NA
        res_geoCode = raw_data$results[[1]]$geometry$location
        } else{
              res_zipCode = raw_data$results[[1]]$address_components[[6]]$long_name
              res_geoCode = raw_data$results[[1]]$geometry$location
              }
  
  return(list(zipcode = res_zipCode, geocode=res_geoCode))
}

geoBBVA_Dir <- function(key, location1){
  require('stringr')
  googleAPI_dir <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  loc_original <- location1
  location1 = geoClean(location1, type = 'dir')
  print(location1)
  location1 = str_replace_all(str_replace_all(str_replace_all(location1, pattern = " ", "%20"),
                                              ",", "%2C"),
                              " NO", " %23")
  location1 = str_replace_all(location1, pattern = "-", "%2D")
  print(location1)
  
  get_json1 <- paste0(googleAPI_dir, location1, '&key=', key)
  
  ## Get geocode
  message("\n... Consultando API Google ", get_json1)
  raw_data <- tryCatch({
    rjson::fromJSON(file = get_json1)
  },
  error = function(cond){
    message('\n ***** Imposible obtener información para ', location1,'  *****\n')
    message(cond)
    
    return(NA)
  })
  
  ## Get important info
  if(raw_data$status != 'OK' | is.na(raw_data[1])){
    message('\n ***** Imposible obtener información para ', loc_original,'  *****\n')
    message('status: ', raw_data$status)
    res_zipCode = NA
    res_geoCode = list(lat = NA, lng = NA)
  } else if(length(raw_data$results[[1]]$address_components) < 8){
    res_zipCode = NA
    res_geoCode = raw_data$results[[1]]$geometry$location
  } else{
    res_zipCode = raw_data$results[[1]]$address_components[[8]]$long_name
    res_geoCode = raw_data$results[[1]]$geometry$location
  }
  
  return(list(zipcode = res_zipCode, geocode=res_geoCode))
}


#geo_lon_lat = HEF_total[, c('lon', 'lat', 'num_events')]
## función para determinar centroides a 1 km
centroides = function(geo_lon_lat){
  ## Calcula distancias
  distancias = as.data.frame(round(distm(geo_lon_lat[, c('lon', 'lat')], fun = distHaversine)/1000, 1))
  distancias$id = 1:nrow(distancias)
  
  ## Define nuevos objetos
  places = NULL
  num_events = NULL
  midlon = NULL
  midlat = NULL
  acumulador = NULL
  
  
  for(i in 1:nrow(distancias)){
    #*****************************
    ## Ubicaciones relacionadas
    #*****************************
    
    entran <- which(distancias[i, 1:(ncol(distancias)-2)] <= 1 ) #& distancias[i, 1:(ncol(distancias)-1)] > 0
    filas = distancias$id[entran[!(entran %in% acumulador)]]
    acumulador = c(acumulador, filas)
    
    res = ""
    if(length(filas) > 0){
      for(fila in filas){
        res = paste(fila, res, sep = ",")
      }
    } else {
      res = NA
    }
    
    places = c(places, res)
    
    #*****************************
    ## Cuenta de eventos
    #*****************************
    num_events = c(num_events, sum(geo_lon_lat$num_events[filas]))#ifelse(, , length(filas)+1))
    
    #*******************************************
    ## Centroide para ubicaciones relacionadas
    #*******************************************
    midlon = c(midlon, mean(geo_lon_lat$lon[filas]))
    midlat = c(midlat, mean(geo_lon_lat$lat[filas]))
  }
  
  new_places = data.frame(places = as.character(places), 
                          num_events = num_events, 
                          lon = midlon, 
                          lat = midlat)
  
  
  return(new_places %>% filter(!is.na(places)))
}


## API Google places
search_API <- function(lat, lon, r=200, key_word='cajero', place_type='finance', API_key){
  ## ESTRUCTURA DE LA API
  ## https://maps.googleapis.com/maps/api/place/nearbysearch/json
  ## ?location=4.6565062,-74.0574668
  ## &radius=200
  ## &type=atm
  ## &keyword=cajero
  ## &key=
  
  url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
  location = paste0("?location=", lat, ',', lon)
  radius = paste0('&radius=', r)
  type = paste0('&type=', place_type)
  keyword = paste0('&keyword=', key_word)
  APIkey = paste0('&key=', API_key)
  
  get_json <- paste0(url, location, radius, type, keyword, APIkey)
  print(get_json)
  
  ## Get places
  message("\n... Consultando API Google Places para ", key_word, ' en ', lat, ',', lon)
  raw_data <- tryCatch({
    rjson::fromJSON(file = get_json)
  },
  error = function(cond){
    message('\n ***** Imposible obtener información para ', location1,'  *****\n')
    message(cond)
    
    return(NA)
  })
  
  return(raw_data)
}

## Función de alerta consumo API
alerta_API <- function(petitions = 25000){
  msj = paste('Costo API (aprox): ', petitions/1000*5*2, ' USD \n ¿Desea continuar? (digite la opcion): 
              \n 1. Continuar \n 2. Cancelar \n')
  sel <- 0
  while(!(sel %in% 1:2)) {
    cat("\014")
    sel <- readline(prompt = msj)
  }
  cat("\014")
  message('\n\n', rep('*',25), '\n Seleccionó ', 
          ifelse(sel == 1, 'Continuar \n', 'Cancelar\n'), 
          rep('*',25), '\n\n')
  
  p <- readline('Presione enter...')
  
  return(sel)
}

diff_months <- function(date1, date2){
  require(lubridate)
  fechas <- floor_date(c(date1, date2), 'month')
  dif <- diff.Date(fechas)
  #print(dif)
  result <- ifelse(dif == 0, 'mismo_mes', 
                   ifelse(dif < 32, 'cambio_1mes', 
                          'cambio_sup_1mes'))
  return(result)
}

monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon 
} 

# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { 
  abs(monnb(d2) - monnb(d1) )
  }


## post function for web scrapping
## on www.bancos-colombia.com
make_post <- function(value, bank){
  petition <- list(
    accion = "actualizar-buscador",
    id = "listaProvincias",
    valor = value
  )
  
  res <- POST("https://www.bancos-colombia.com/config/ModulesAjax.php", 
              body = petition, 
              encode = "form", 
              add_headers(`origin` = 'https://www.bancos-colombia.com',
                          `referer` = paste0('https://www.bancos-colombia.com/', bank),
                          `x-requested-with` = 'XMLHttpRequest'),
              verbose())
  
  #res_t <- content(res, as="text")
  #res_h <- paste0(unlist(strsplit(res_t, "\r\n"))[-1], sep="", collapse="\n")

  return(res)
} 

## Obtiene lista de departamentos o municipios según se escoja
get_list_values <- function(url, find, css){
  #list_values <- read_html(url) %>% 
  #  html_nodes(css) 
  
  list_values <- read_url(url, css)
  
  if(!is.na(list_values[1])){
    ind <- which(grepl(paste0("id=\"",find,"\""), as.character(list_values)))
    texto_completo <- as.character(list_values[ind])
    
    buscar <- "<option value=\"/"
    texto_completo <- unlist(str_split(texto_completo, '\n'))
    texto_completo <- texto_completo[str_detect(texto_completo, pattern = buscar)]
    
    
    list_values = NULL
    values = NULL
    for(pos in 1:length(texto_completo)){
      res <- read_html(texto_completo[pos]) %>%
        html_text()
      list_values = c(list_values, res)
      
      # get values for POST
      res <- unlist(str_split(texto_completo[pos], '\"'))
      res <- res[grepl("^/", res)]
      
      values = c(values, res)
    }
    
    return(list(list_values = list_values, values = values))
  }
  else{
    return(list(list_values = NA, values = NA))
  }
  
  
}

## función de lectura con tryCatch
read_url <- function(url, css){
  out <- tryCatch(
    {
      read_html(url) %>%
        html_nodes(css)
    },
    error = function(cond){
      message('No hay información para ese pueblito...\n')
      message(cond, '\n')
      return(NA)
    }
  )
  return(out)
}

# This function find centroids of a events cluster.
centroids_finder <- function(BD){
  BD$num_events <- 1
  BD <- BD %>%
    #filter(get(vtgroup) == grouper) %>%
    select(lon, lat, num_events)
  
  p1 = centroides(BD)
  sum(p1$num_events)
  p1$places[which.max(p1$num_events)]
  
  
  p2 = centroides(p1[, c('lon', 'lat', 'num_events')])
  sum(p2$num_events)
  max(p2$num_events)
  
  return(p2)
} 

## this function acummulates number of events near a given radius (km)
events_acum <- function(locations, events_points, r = 0.5){
  peligro = as.data.frame(
    round(
      distm(locations[, c('lon', 'lat')], events_points[, c('lon', 'lat')])/1000
      ,2)
  )
  
  acumulator = NULL
  for(fila in 1:nrow(peligro)){
    count = sum(
      events_points$num_events[which(peligro[fila, ] <= r)]
    )
    acumulator = c(acumulator, count)
  }
  
  return(acumulator)
}