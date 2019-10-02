source('./scripts/funciones_Camilo.R')

load.lib('dplyr', 'reshape2', 'readxl', 'ggmap', 'RCurl', 'rjson', 'stringr', 'progress',
         'rgeos', 'sp', 'maptools', 'car', 'geoR', 'gstat', 'gdata', 'geosphere', 'lubridate',
         'rgdal', 'htmlwidgets', 'caret')

fecha_informe <- as.Date.character('2019-06-01')
#*************************
## 1. Lectura info ####
#*************************
cajeros <- readRDS(file = './data/BBVA/data_wrangling/cajeros/201907_def_Cajeros.RDS')
geo_vandalismo <- readRDS(file = './data/BBVA/data_wrangling/vandalismo/201907-geo_vandalismo.RDS') %>%
                  mutate(diff_month = paste0('diff_', mondf(fecha_informe, geo_vandalismo$Mes)))

cajerosVecinos <- readRDS(file = './data/BBVA/data_wrangling/cajeros_vecinos/201907_cajeros_vecinos.RDS') %>%
                  select(-(Vecinos_Raw), -(BBVA))


mondf(fecha_informe, geo_vandalismo$Mes[1:10])

cuenta_vandal <- geo_vandalismo %>%
               group_by(`Código ATM`, diff_month) %>%
               summarise(n = n()) %>%
               dcast(`Código ATM` ~ diff_month, value.var = 'n', fill = 0)

cuenta_vandal$vand_12m <- apply(cuenta_vandal[,2:ncol(cuenta_vandal)], 1, sum)


consolida_cajeros <- merge(cajeros, cuenta_vandal, by.x='COD DEL CAJERO', by.y='Código ATM',
                           all.x=T, all.y=T) 

consolida_cajeros <- merge(consolida_cajeros, cajerosVecinos, by.x='COD DEL CAJERO', 
                           by.y='cod_Cajero', all.x=T, all.y=T)

desde = which(names(consolida_cajeros) == names(cuenta_vandal)[2])
consolida_cajeros[, desde:ncol(consolida_cajeros)][is.na(
  consolida_cajeros[, desde:ncol(consolida_cajeros)]
  )] <- 0

consolida_cajeros$is_vandal <- ifelse(consolida_cajeros$vand_12m > 0, 1, 0)
consolida_cajeros$is_vandal_month <- ifelse(consolida_cajeros$diff_0 > 0, 1, 0)

#****************
## 2. EDA ####
#****************

prop_vandal <- consolida_cajeros %>%
               group_by(CIUDAD) %>%
               summarise(n_cajeros = n(), is_vandal = sum(is_vandal, na.rm = T)) %>%
               mutate(prop_vandal = round(is_vandal/n_cajeros *100, 0))

table(consolida_cajeros$MARCA)

marcas_vandal <- consolida_cajeros %>%
                 group_by(MARCA = toupper(MARCA)) %>%
                 summarise(n_cajeros = n(), is_vandal = sum(is_vandal, na.rm = T)) %>%
                 mutate(prop_vandal = round(is_vandal/n_cajeros *100, 0))

marcas_vandal

marcas_vandal_total <- consolida_cajeros %>%
  group_by(MARCA = toupper(MARCA)) %>%
  summarise(n_cajeros = n(), vand_12m = sum(vand_12m, na.rm = T)) %>%
  mutate(prop_vandal = round(vand_12m/n_cajeros *100, 0))

marcas_vandal_total


models_vandal <- consolida_cajeros %>%
  group_by(MODELO = toupper(MODELO)) %>%
  summarise(n_cajeros = n(), is_vandal = sum(is_vandal, na.rm = T)) %>%
  mutate(prop_vandal = round(is_vandal/n_cajeros *100, 0))

models_vandal


models_vandal_total <- consolida_cajeros %>%
  group_by(MODELO = toupper(MODELO), `NIVEL DE PROVISION`) %>%
  summarise(n_cajeros = n(), vand_12m = sum(vand_12m, na.rm = T)) %>%
  mutate(prop_vandal = round(vand_12m/n_cajeros *100, 0))
  

models_vandal_total

ubication_vandal_total <- consolida_cajeros %>%
                    group_by(Ubication = toupper(`UBICACIÓN ESPECIFICA`)) %>%
                    summarise(n_cajeros = n(), vand_12m = sum(vand_12m, na.rm = T)) %>%
                    mutate(prop_vandal = round(vand_12m/n_cajeros *100, 0))

ubication_vandal_total


ubication_vandal <- consolida_cajeros %>%
  group_by(Ubication = toupper(`UBICACIÓN ESPECIFICA`)) %>%
  summarise(n_cajeros = n(), is_vandal = sum(is_vandal, na.rm = T)) %>%
  mutate(prop_vandal = round(is_vandal/n_cajeros *100, 0))

ubication_vandal


nivel_vandal <- consolida_cajeros %>%
  group_by(`NIVEL DE PROVISION`) %>%
  summarise(n_cajeros = n(), is_vandal = sum(is_vandal, na.rm = T)) %>%
  mutate(prop_vandal = round(is_vandal/n_cajeros *100, 0))

nivel_vandal


nivel_vandal_total <- consolida_cajeros %>%
  group_by(`NIVEL DE PROVISION`) %>%
  summarise(n_cajeros = n(), vand_12m = sum(vand_12m, na.rm = T)) %>%
  mutate(prop_vandal = round(vand_12m/n_cajeros *100, 0))

nivel_vandal_total


ATH_vandal <- consolida_cajeros %>%
  group_by(ATH) %>%
  summarise(n_cajeros = n(), is_vandal_month = sum(is_vandal_month, na.rm = T)) %>%
  mutate(prop_vandal = round(is_vandal_month/n_cajeros *100, 0))

ATH_vandal


Dav_vandal <- consolida_cajeros %>%
  group_by(Davivienda) %>%
  summarise(n_cajeros = n(), is_vandal_month = sum(is_vandal_month, na.rm = T)) %>%
  mutate(prop_vandal = round(is_vandal_month/n_cajeros *100, 0))

Dav_vandal



#***********************************
## 2. Data Wrangling ####
#***********************************
names(consolida_cajeros) <- str_replace_all(names(consolida_cajeros), ' ', '_')
names(consolida_cajeros)

consolida_cajeros$UBICACIÓN_ESPECIFICA <- toupper(consolida_cajeros$UBICACIÓN_ESPECIFICA)

consolida_cajeros$Medio <- ifelse(is.na(consolida_cajeros$Medio), 'NoIndica', 
                                  consolida_cajeros$Medio)

consolida_cajeros$MONITOREO_DE_ALARMAS <- ifelse(is.na(consolida_cajeros$MONITOREO_DE_ALARMAS),
                                                 'NoIndica', consolida_cajeros$MONITOREO_DE_ALARMAS)

consolida_cajeros$Proveedor_de_Vigilancia <- ifelse(is.na(consolida_cajeros$Proveedor_de_Vigilancia),
                                                    'NoIndica', consolida_cajeros$Proveedor_de_Vigilancia)

consolida_cajeros$MEDIO_PRINCIPAL <- ifelse(is.na(consolida_cajeros$MEDIO_PRINCIPAL),
                                            'NoIndica', consolida_cajeros$MEDIO_PRINCIPAL)

consolida_cajeros$MEDIO_DE_CONTINGENCIA <- ifelse(is.na(consolida_cajeros$MEDIO_DE_CONTINGENCIA),
                                                  'NoIndica', consolida_cajeros$MEDIO_DE_CONTINGENCIA)

consolida_cajeros$LLAVE_DE_SEGURIDAD <- ifelse(is.na(consolida_cajeros$LLAVE_DE_SEGURIDAD),
                                               'NoIndica', consolida_cajeros$LLAVE_DE_SEGURIDAD)

var_factor <- c('TERRITORIAL', 'ZONA', 'CAJEROS_VIP',  
                'MARCA', 'MODELO', 'TIPO_DE_PROCESADOR_DIEBOLD',
                'CARGA', 'NIVEL_DE_PROVISION', 'UBICACIÓN_ESPECIFICA', 'SITE_TOPPER',
                'SEGMENTO', 'TIPO_DE_UBICACIÓN', 'TRANSACCIONALIDAD_L_V', 
                'TRANSACCIONALIDAD_FINES_DE_SEMANA', 'PROTECTOR_DE_TECLADO',
                'MODULO_TELDAT', 'TIPO_DE_ENLACE', 'MEDIO_PRINCIPAL',
                'MEDIO_DE_CONTINGENCIA', 'MODULO_TELDAT', 'DEPOSITARIOS_ACTIVOS_EFECTIVO',
                'DEPOSITARIOS_ACTIVOS_CHEQUE', 'TRANSPORTADORA', 'CLASIFICACION_TDV', 
                'RUTA_NOCTURNA', 'PRIMER_NIVEL', 'SEGUNDO_NIVEL',
                'TIPO_DE_BILLETE', 'CUPO_AUTORIZADO', 'ANTISKIMING', 'CONFIGURACION_ATM',
                'DIAS_DE_SERVICIO', 'Nombre_del__Departamento',
                'HORARIO_DE_APERTURA', 'HORARIO__DE_CIERRE', 'HORARIO_DE_SERVICIO',
                'METODO_CIERRE_CONSOLA', 'LLAVES', 'LLAVE_DE_SEGURIDAD', 'Medio',
                'MONITOREO_DE_ALARMAS', 'ANTELACIÓN_PARA_TRAMITAR_PERMISOS',
                'MULTIVENDOR')

var_date <- c('FECHA_DE_MAQUINA', 'FECHA_DE_ENTRADA_EN_PRODUCCION_EN_EL_PUNTO')


consolida_cajeros$FECHA_DE_MAQUINA <- as.Date.character(consolida_cajeros$FECHA_DE_MAQUINA,
                                                        format = "%d/%m/%Y")

consolida_cajeros$FECHA_DE_ENTRADA_EN_PRODUCCION_EN_EL_PUNTO <- as.Date.character(consolida_cajeros$FECHA_DE_ENTRADA_EN_PRODUCCION_EN_EL_PUNTO,
                                                        format = "%d/%m/%Y")

consolida_cajeros$FECHA_DE_ENTREGA_DE_MAQUINA_A_PUNTO <- as.Date.character(consolida_cajeros$FECHA_DE_ENTREGA_DE_MAQUINA_A_PUNTO,
                                                        format = "%d/%m/%Y")

#**************************
## Distancias Vandalismos
#**************************
?distm

distm(consolida_cajeros[, c('lon', 'lat')], 
      consolida_cajeros[, c('lon', 'lat')])



#**************************
## Grupos de Variables
#**************************
var_num <- c('AÑOS_DE_LOS_CAJEROS', 'Nro__Gavetas', 'vand_12m', 'Bancolombia', 'Davivienda', 
             'ATH', 'Caja_Social', 'Citibank', 'Colpatria', 'Servibanca', 'Colpatria',
             'Agrario', 'Falabella', 'Otros', 'diff_1', 'diff_2', 'diff_3', 'diff_4',
             'diff_5', 'diff_6', 'diff_7', 'diff_8', 'diff_9')

var_location <- c('lat', 'lon')

var_obj <- c('is_vandal', 'is_vandal_month')

cond <- !(names(consolida_cajeros) %in% var_factor) & 
        !(names(consolida_cajeros) %in% var_date) &
        !(names(consolida_cajeros) %in% var_location) &
        !(names(consolida_cajeros) %in% var_num) &
        !(names(consolida_cajeros) %in% var_obj)

var_out <- names(consolida_cajeros)[cond]

BD_model <- consolida_cajeros %>%
            select(-var_out) %>%
            #select(-var_location) %>%
            #select(-var_obj) %>%
            mutate_at(var_factor, as.factor) %>%
            mutate_at(var_obj, as.factor) %>%
            mutate_at(var_num, as.numeric)

## Revisa columnas con NA
for(col in names(BD_model)){
  if(sum(is.na(BD_model[, col])) > 0) print(col)
}


## Revisa columnas con mas de 20 categorías
for(col in names(BD_model)){
  if(length(unique(BD_model[, col])) > 30) print(col)
}

length(unique(BD_model$FONDO_DE_EFECTIVO))


#***********************************
## 3. Variable importance ####
#***********************************
require(randomForest)
library(caret)
fit <- randomForest(is_vandal_month ~., data = BD_model)
(VI_F=importance(fit))
varImp(fit)
varImpPlot(fit,type=2)


var_importance <- data_frame(variable=setdiff(colnames(BD_model), "is_vandal_month"),
                             importance=as.vector(importance(fit)))
var_importance <- arrange(var_importance, desc(importance))
var_importance <- var_importance[var_importance$importance>9,]

var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
p <- p + xlab("Demographic Attribute") + ylab("Variable Importance (Mean Decrease in Gini Index)")
p <- p + scale_fill_discrete(name="Variable Name")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=12))

table(BD_model$ZONA, BD_model$is_vandal_month)
table(BD_model$UBICACIÓN_ESPECIFICA, BD_model$is_vandal_month)
table(BD_model$HORARIO_DE_SERVICIO, BD_model$is_vandal_month)



#***********************************
## 4. Train_test ####
#***********************************
set.seed(3456)
trainIndex <- createDataPartition(BD_model$is_vandal_month, p = .7, 
                                  list = FALSE, 
                                  times = 1)

train <- BD_model[trainIndex,]
table(train$is_vandal_month)

test <- BD_model[-trainIndex,]
table(test$is_vandal_month)


#***********************************
## 5. Models ####
#***********************************

#Nombre_del__Departamento + MODELO + SEGMENTO + UBICACIÓN_ESPECIFICA + FECHA_DE_MAQUINA + HORARIO_DE_SERVICIO
# + diff_4 + diff_5 + diff_6 + diff_7 + diff_8 + diff_9 + 

mod1 <- glm(is_vandal_month ~ vand_12m + ZONA +
              diff_1 + diff_2 + diff_3 + is_vandal + lon + lat +
              ATH + Bancolombia + Davivienda + MULTIVENDOR - 1, 
            family = binomial(link = "logit"), data = train)

summary(mod1)  
 
  
## OTRO MODELO
## Revisa columnas CONSTANTES
for(col in names(train)){
  if(length(unique(train[, col])) < 2) print(col)
}


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(is_vandal_month ~ ., data = train, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)

gbmFit2$modelInfo
saveRDS(gbmFit2, './results/models/gbmFit2.RDS')

whichTwoPct <- tolerance(gbmFit2$results, metric = "ROC", tol = 2, maximize = TRUE)  
cat("best model within 2 pct of best:\n")

confusionMatrix(predict(gbmFit2, train), 
                train$is_vandal_month, positive = '1')
  
confusionMatrix(predict(gbmFit2, test), 
                test$is_vandal_month, positive = '1')
  

#*********************************
## Test model with new data
#*********************************


