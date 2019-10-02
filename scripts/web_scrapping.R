source('./scripts/funciones_Camilo.R')

load.lib('httr', 'rvest', 'dplyr', 'stringr', 'stringi', 'progress')

#******************************
## bancos-colombia ####
#******************************

bancos <- c('davivienda', 'colpatria')

#****************************************
## Selección lista departamentos ####
#****************************************
url <- paste0("https://www.bancos-colombia.com/", bancos[1])

#**************************
## búsqueda departamentos
#**************************
## parámetros de búsqueda
css <- "select.lista_buscador"
find_dptos <- 'listaProvincias'

list_dptos <- get_list_values(url, find_dptos, css)
list_dptos$list_values
list_dptos$values

#****************************************
## Selección lista municipios ####
#****************************************
res <- make_post(list_dptos$values[15], bancos[1])
find_mcpios <- 'listaLocalidades'
css <- "select.lista_buscador"

list_mcpios <- get_list_values(res, find_mcpios, css = css)
list_mcpios$list_values
list_mcpios$values


#****************************************
## Sucursales ####
#****************************************
url2 <- paste(url, list_mcpios$list_values[92], list_dptos$list_values[15], sep='-')
url2 <- tolower(stri_trans_general(url2,"Latin-ASCII"))
url2

css <- "a.truncate"
directions <- read_url(url2, css) %>%
              html_text()



#*******************************************
## Pipeline scraping ####
#*******************************************
bank_scaping <- function(bancos){
  bank = NULL
  city = NULL
  directions = NULL
  
  for(banco in bancos){
    message('\n buscando sucursales para banco ', banco)
    url <- paste0("https://www.bancos-colombia.com/", banco)
    
    #**************************
    ## búsqueda departamentos
    #**************************
    ## parámetros de búsqueda
    css <- "select.lista_buscador"
    find_dptos <- 'listaProvincias'
    
    list_dptos <- get_list_values(url, find_dptos, css)
    #list_dptos$list_values
    #list_dptos$values
    
    pb <- progress_bar$new(
      format = "  downloading [:bar] :percent eta: :eta",
      total = length(list_dptos$values),
      clear = FALSE, width= 60
      )
    count_dpto = 1
    for(dpto in list_dptos$values){
      pb$tick()
      message('\n buscando sucursales para banco ', banco, ' en ', dpto)
      dpto_name <- list_dptos$list_values[count_dpto]
      #****************************************
      ## Selección lista municipios ####
      #****************************************
      res <- make_post(dpto, banco)
      find_mcpios <- 'listaLocalidades'
      css <- "select.lista_buscador"
      
      list_mcpios <- get_list_values(res, find_mcpios, css = css)
      #list_mcpios$list_values
      #list_mcpios$values
      
      pb2 <- progress_bar$new(
        format = "  downloading [:bar] :percent eta: :eta",
        total = length(list_mcpios$list_values),
        clear = FALSE, width= 60
        )
      for(mcpio in list_mcpios$list_values){
        message('\n buscando sucursales para banco ', banco, ' en ', dpto, ' ciudad: ', mcpio)
        pb2$tick()
        #**************************
        ## Sucursales ####
        #**************************
        if(mcpio %in% capitals){
          url2 <- paste(url, mcpio, dpto_name, sep='-')
          url2 <- tolower(stri_trans_general(url2,"Latin-ASCII"))
          url2
        } else {
          url2 <- paste(url, mcpio, sep='-')
          url2 <- tolower(stri_trans_general(url2,"Latin-ASCII"))
          url2
        }
        
        css <- "a.truncate"
        dir <- read_url(url2, css) 
        if(!is.na(dir[1])){
          directions <- c(directions, html_text(dir))
          bank <- c(bank, rep(banco, length(directions)))
          city <- c(city, rep(mcpio, length(directions)))
        }
      }
      count_dpto = count_dpto + 1
    }
  }
  
  ## Dataframe final
  p <- data.frame(bank = bank, city = city, direction = directions)
  return(p)
  
}

p = bank_scaping(bancos)
saveRDS(p, './results/scraping.RDS')

#****************************************************************************************************
#****************************************************************************************************

## https://stackoverflow.com/questions/46552923/downloading-excel-file-using-r

# Es un poco tarde la respuesta pero creo que aún vale la pena aportar una solución.
# No es posible usar la función download.file puesto que dicho enlace realmente no direcciona directamente al archivo. En realidad es una consulta a una API usando el método GET, por lo que deberías usar otra estructura de código para poder obtener el archivo, situación que puede ocurrir en repetidas oportunidades para los que usamos técnicas de webscraping
# Te comparto un ejemplo de cómo se obtiene el archivo excel para el ídice COLCAP dario: 

url <- 'http://obieebr.banrep.gov.co/analytics/saw.dll?Download&Format=excel2007&Extension=.xlsx&BypassCache=true&Path=%2fshared%2fSeries%20Estad%c3%adsticas_T%2f1.%20%c3%8dndices%20de%20mercado%20burs%c3%a1til%20colombiano%2f1.1.%20IGBC,%20IBB%20e%20IBOMED%2f1.1.1.IMBC_COLCAP%20IQY&lang=es&NQUser=publico&NQPassword=publico&SyncOperation=1'

content_type = "text/html; charset=utf-8"
while (content_type == "text/html; charset=utf-8") {
  request <- GET(url,
                 add_headers(`Accept` = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', #'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3',#
                             `Accept-Encoding` = 'gzip, deflate',
                             `Accept-Language` = 'es-ES,es;q=0.9',
                             `Connection` = 'keep-alive',
                             `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.90 Safari/537.36',
                             `Host` = 'obieebr.banrep.gov.co'),
                 write_disk("COLCAP_daily.xlsx", overwrite = T),
                 verbose()
  )
  content_type = request$all_headers[[1]]$headers$`content-type`
}


request$cookies

