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



