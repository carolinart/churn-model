# setwd("//bdbemcfs/Analytics/proyectos_practica/Otros")
# x <- fread("DATOS_BI.csv", colClasses = "character", na.strings = c("", "NA"))
# names(x)
# rm(x)


#Cargar directorio y librerías
setwd("//bdbemcfs/Analytics/proyectos_practica/modelo_churn/scripts")
source("library.R")


setwd("//bdbemcfs/Analytics/proyectos_practica/modelo_churn/data/original/master_file")


#Leer archivos formato csv y extraer caracteres numéricos
fileList <- list.files(path=getwd(), pattern=".csv")
position <- sapply(str_extract_all(fileList, "[0-9]+"), "[[", 1) %>% as.numeric

# levels_month.id <- c("201801", "201802", "201803", "201804", "201805",
#                      "201806", "201807", "201808", "201809","201810", 
#                      "201811", "201812", "201901", "201902",
#                      fileList[position])

levels_month.id <- c("201808", "201809","201810", "201811", "201812", "201901", "201902",
                     fileList[position])

base_list <- list()


#Variables que se van a usar
var_int <-
  c(
    "COD_INT",
    "TIPO_CLIENTE",
    "TIPO_CARTERA",
    "PERIODO",
    "ANTIGUEDAD",
    # "RANGO_ANTIGUEDAD",
    "CUPO",
    # "RANGO_CUPO",
    "UTILIZACION",
    # "RANGO_UTILIZACION",
    "DIAS_MORA",
    # "RANGO_EDAD", 
    "EDAD",
    "FRANQUICIA", 
    "RENTA", 
    "MARCA_COMPARTIDA", 
    "TIPO_TARJ",
    "CUOMANO",
    "CARTERA_CAPITAL_TOTAL",
    "CARTERA_VIGENTE", 
    "SDO_TOTAL",
    "PAGO_MINIMO",
    # "RANGO_PAGO_MINIMO",
    "SALDO_MORA",
    # "DEPARTAMENTO",
    "GENERO",
    # "MUNICIPIO",
    # "DESCRIPCION_TIPO_VIVIENDA",
    "DESCRIPCION_NIVEL_EDUCATIVO",
    # "DESCRIPCION_ESTADO_CIVIL",
    # "DESCRIPCION_SEG_COMERCIAL",
    # "DESCRIPCION_OCUPACION",
    # "CLASIFIC_ACTIVIDAD_ECONOMICA",
    "AA_ESTRATO", 
    # "AA_VLR_ACTIVOS",
    # "AA_VLR_ING_BRU_MES",
    # "AA_VLR_EGRESO_MES",
    # "AA_VLR_PASIVOS",
    "SCORE_COMP_CALIF_ACT")


#Leer bases de datos
for(i in 1:length(fileList)){
  #  dt <- fread(i, colClasses = "character", na.strings=c("","NA")) 
  base_list[[i]] <- fread(fileList[i], colClasses = "character", na.strings=c("","NA"))
  base_list[[i]][, PERIODO := position[i]]
  base_list[[i]] <- base_list[[i]][, mget(var_int)]
}

base <- rbindlist(base_list)
names(base) <- tolower(names(base))
rm(base_list)

#Ver significancia de las variables
summary(factor(base$tipo_cartera))
base <- base[tipo_cartera == "Consumo"]
base[, month.id := as.numeric(factor(periodo, levels = unique(levels_month.id)))]


###########################################################################################

#Convertir formato yyyymm de periodo a fecha formato yearmon
base[, fecha := as.Date(paste0(as.character(periodo), '01'), format='%Y%m%d')]
base[, fecha := as.yearmon(fecha)]
base0 <- copy(base[, .(cod_int, periodo, fecha, month.id)])


#Cálculo variables
base[, month.id := as.numeric(factor(periodo, levels = unique(levels_month.id)))]

base1 <- base0[, .(cod_int, month.id)]
base1[, month.id := month.id - 2]
base1[, var.churn := 1]

base0 <- merge(base, base1, by = c("cod_int", "month.id"), all.x = T)


#Eliminar últimos dos periodos
base_target <- base0[ periodo != '201901' & periodo !=  '201902']
base_target[, .N, by = periodo]


#Definir target
base_target[var.churn == "1", var.churn := 0]   
base_target[is.na(var.churn), var.churn := 1]     ##Los NA son "churn"


#Tratamiento a nombres
setnames(base_target, "var.churn", "Y")

var_int <- tolower(var_int[-c(1:3)])

master <- copy(base_target[, mget(c(var_int, "Y"))])

# factor_vars <- grep( "rango", var_int, value = T)
factor_vars <-
  c(
    # factor_vars
    "franquicia",
    "renta",
    "marca_compartida",
    "tipo_tarj",
    # "departamento",
    "genero",
    # "descripcion_estado_civil",
    # "descripcion_seg_comercial",
    # "descripcion_ocupacion",
    # "clasific_actividad_economica",
    "aa_estrato",
    "score_comp_calif_act",
    "descripcion_nivel_educativo"
    # "descripcion_tipo_vivienda"
  )

#Hallar número de utilización
master[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

numeric_vars <- names(master)[!(names(master) %in% factor_vars)]

master[, util := as.numeric(str_extract_all(master$utilizacion, "[0-9]+"))/100]
master[, (numeric_vars) := lapply(.SD, as.numeric), .SDcols = numeric_vars]
master[, utilizacion := NULL]


#Ver Missing values
missing_data <- master_lp %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- tidyr::gather(missing_data, key = "variables", value = "percent_missing")

ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()


#Omitir NA's en el master
master_lp <- na.omit(master)


################################################################################################
# Análisis exploratorio #
##############################################################################################

 # Ver porcentaje de churn 
options(repr.plot.width = 6, repr.plot.height = 4)
master_lp %>% 
  group_by(Y) %>% 
  dplyr::summarize(count = n())%>% 
  mutate(percent = prop.table(count)*100)%>%
  ggplot(aes(reorder(Y, -percent), percent), fill = Y)+
  geom_col(fill = c("#FC4E10", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  labs(title = "Porcentaje de Churn", caption = "Nota: 1 si churn a dos meses, 0 si stock a dos meses",x = "Churn", y = "%")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


##############################################################################################
# Definiciones previas al modelamiento #
##############################################################################################

#Definir periodo de base de entrenamiento
master_train <- master_lp[periodo != "201812"]
# master_train <- na.omit(master_train)

#Ver balance del training
table(master_train$Y)


#Definir periodo de base de test
master_test <- master_lp[periodo == "201812"]
# master_test <- na.omit(master_test)
# master_master_test <- na.omit(master_master_test)
# sapply(master_train, function(x){unique(is.na(x))} )

#Ver balance del test
table(master_test$Y)


rm(base, base0, base1, base_target)
gc()


#############################################################################
# Previo al modelamiento #
############################################################################

set.seed(666)

#porc_master_train <- 0.8

#ID_master_train <- sort(sample(nrow(master), nrow(master)*porc_master_train))
#master_train <- master[ID_master_train]
#master_test <- Ahorros[-ID_master_train]


if (nrow(master_train) + nrow(master_test) != nrow(master)){
  stop("Hay un error en la separacion de data.")}


sample_train <- sample(1:nrow(master_train), 0.7*nrow(master_train))
master_train_sp <- master_train[sample_train,]


###############################################################################
## Matriz de correlaciones para variables numéricas##
###############################################################################

corr_mat <- cor(master_train_sp %>% dplyr::select (
  # "cod_int",
  "antiguedad",
  "cupo",
  "util",
  "dias_mora",
  "edad",
  "cuomano",
  "cartera_capital_total",
  "cartera_vigente", 
  "sdo_total",
  "pago_minimo",
  "saldo_mora"
))
round(corr_mat, 2)



#############################################################################################
corr_mat <- cor(master_train_sp %>% dplyr::select (
  # "cod_int",
  "antiguedad",
  "cupo",
  "util",
  "dias_mora",
  "edad",
  "cuomano",
  "cartera_capital_total",
  # "sdo_total",
  "pago_minimo",
  "saldo_mora"
))
round(corr_mat, 2)



#####################################################################
##Seleccionar variables##
####################################################################

######################################
##Primer grupo de variables##
#####################################

##Ajuste del modelo sin variables, únicamente intercepto
nothing <- glm(Y ~ 1,family=binomial, data = master_train_sp)


##Ajuste del modelo saturado/ algunas variables escogidas
fullmodel <- glm(
  formula = Y ~ antiguedad +
  cupo +
  franquicia +
  renta +
  # score_comp_calif_act +
  descripcion_nivel_educativo +
  # aa_estrato +
  util +
  dias_mora +
  # edad +
  # genero + 
  tipo_tarj +
  cuomano +
  cartera_capital_total +
  cartera_vigente +
  pago_minimo +
  saldo_mora,
  family = binomial(link = "logit"), 
  data = master_train_sp)


###Forward para seleccionar variables

forwards = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(fullmodel)), direction="forward")


rm(forwards, nothing)
gc()


########################################################################
# Modelamiento#
#######################################################################

  #Modelo logit

# sLogit <- glm(formula = Y ~ .,
#               family = binomial(link = "logit"), data = master_train_sp)

sLogit <- glm(
  formula = Y ~ dias_mora + util + renta + antiguedad + 
    cartera_capital_total + cupo + franquicia + 
    saldo_mora + tipo_tarj + cuomano + pago_minimo,
              family = binomial(link = "logit"), 
              data = master_train_sp)




  #Regression Tree

m1 <- rpart(
  formula = Y ~ .,
  data    = master_train_sp,
  method  = "anova"
)

rpart.plot(m1)






#Calcular vector de probabilides predichas
pgorro <- predict(sLogit, type = "response", newdata = master_test)


#' Extraer grafica del ROC relacionado a master_test, con AUC graficado
#'
#' @param pgorro vector de probabilidades predichas para master_test.
#' @param master_test vector de realizaciones del proceso de la variable dependiente para master_test.
#'
#' @return En la ruta, carpeta "Graficas/modelo_#", se presenta la grafica mencionada.
#'
#' @examples
#' Extraer_ROC(pgorro = pgorro, master_test = master_test)
#' 

ROC <- function(pgorro, master_test) {
  
  ROCRpred <- prediction(pgorro, master_test$Y)
  ROCRperf <- performance(ROCRpred, 'tpr','fpr')
  auc_ROCR <- performance(ROCRpred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  plot(ROCRperf, colorize = F, text.adj = c(-0.2,1.7), xlab = "1 - Specificity", ylab = "Recall") 
  legend(0.2, 0.6, paste0("AUC = ", format(auc_ROCR, digits = 4)), border="white", cex=1, box.col = "white")
  
}

ROC(pgorro = pgorro, master_test = master_test)



#' Exporta master con matriz de confusion
#'
#' @param predict vector de probabilidades predichas.
#' @param response vector con realizaciones. Ojo: no la master, sino el vector.
#' @param threshold debe ser un valor entre 0 y 1. Define con que probabilidad es el limite entre decir que una probabilidad 
#' es mas cercana a 0 o 1. Si no se pone nada, se asume el thresold optimo.
#' Esto afecta a la matriz de confusion principalmente.
#'
#' @return Regresa la matrix.
#'
#' @examples
#' conf_matrix <- Confusion_matrix(predict = pgorro, response = master_test$Y)
#' conf_matrix <- Confusion_matrix(predict = pgorro, response = master_test$Y, threshold = 0.1)
#' 

confusion_matrix <- function(predict, response, threshold = "opt_threshold") {
  
  
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  
  if(threshold == "opt_threshold"){
    opt_threshold <- df[which.max(df$sens + df$spec), "cut"]
  } else {
    opt_threshold <- threshold
  }
  
  conf <- confusion.matrix(obs = response, pred = predict, threshold = opt_threshold)
  
  colnames(conf) <- c("Obs 0", "Obs 1")
  rownames(conf) <- c("Pred 0", "Pred 1")
  
  return(conf)
}



#' Devuelve el uplift relacionado a un percentil
#'
#' @param conf_matrix matriz de confusion arrojada por el modelo.
#'
#' @return el valor del uplift.
#'
#' @examples
#' Uplft10 <- Uplift(conf_matrix = conf_matrix)
#' 

Uplift <- function(conf_matrix){
  conf <- conf_matrix
  UPLIFT <- (conf[2,2]/(conf[2,2] + conf[2,1]))/((conf[1,2] + conf[2,2])/(conf[1,1] + conf[1,2] + conf[2,1] + conf[2,2]))
  return(UPLIFT)
}


#' Devuelve el recall relacionado a un percentil
#'
#' @param conf_matrix matriz de confusion arrojada por el modelo.
#'
#' @return el valor del recall.
#'
#' @examples
#' Recall10 <- Recall(conf_matrix = conf_matrix)
#' 

Recall <- function(conf_matrix){
  conf <- conf_matrix
  RECALL <- conf[2,2]/(conf[2,2] + conf[1,2])
  return(RECALL)
}


#' Devuelve el precision relacionado a un percentil
#'
#' @param conf_matrix matriz de confusion arrojada por el modelo.
#'
#' @return el valor del precision.
#'
#' @examples
#' Precision10 <- Precision(conf_matrix = conf_matrix)
#' 

Precision <- function(conf_matrix){
  conf <- conf_matrix
  PRECISION <- conf[2,2]/(conf[2,2] + conf[2,1])
  return(PRECISION)
}



#' Exporta grafica con Uplift para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param master_test la master de master_test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_uplift(pgorro = pgorro, conf_matrix = conf_matrix, master_test = master_test)
#' 

Grafica_uplift <- function(pgorro, master_test, percentiles = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09)) {
  
  master_uplift <- matrix(NA, nrow = length(percentiles), ncol = 2)
  master_uplift[, 1] <- percentiles*100
  colnames(master_uplift) <- c("Percentil", "Uplift")
  
  for (i in 1:length(percentiles)){
    master_uplift[i, 2] <- Uplift(conf_matrix = confusion_matrix(predict = pgorro, 
                                                                 response = master_test$Y, 
                                                                 threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  master_uplift <- data.table(master_uplift)
  plot <- ggplot(data=master_uplift, aes(x= Percentil, y=Uplift, fill = -Uplift)) +
    geom_bar(stat="identity")+
    geom_text(label = round(master_uplift$Uplift, digits = 2) , size = 7, vjust = c(1.5, rep(-1, times = length(percentiles) - 1)), color = c("white", rep("black", times = length(percentiles) - 1)), hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Uplift para cada percentil de probabilidades",
         caption = "\n Uplift: el modelo cuantas veces mejoran el Recall y Precision, con respecto a una seleccion aleatoria") + scale_x_continuous(breaks = percentiles*100)
  
  return(plot)
}

Grafica_uplift(pgorro = pgorro, master_test = master_test)

#' Exporta grafica con Recall para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param master_test la master de master_test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_recall(pgorro = pgorro, conf_matrix = conf_matrix, master_test = master_test)
#' 

Grafica_recall <- function(pgorro, master_test, percentiles = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09)) {
  
  master_recall <- matrix(NA, nrow = length(percentiles), ncol = 2)
  master_recall[, 1] <- percentiles*100
  colnames(master_recall) <- c("Percentil", "Recall")
  
  for (i in 1:length(percentiles)){
    master_recall[i, 2] <- Recall(conf_matrix = confusion_matrix(predict = pgorro, 
                                                                 response = master_test$Y, 
                                                                 threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  master_recall <- data.table(master_recall)
  plot <- ggplot(data=master_recall, aes(x= Percentil, y=Recall, fill = -Recall)) +
    geom_bar(stat="identity")+
    geom_text(label = round(master_recall$Recall, digits = 2) , size = 7, vjust = c(1.5, rep(1.5, times = length(percentiles) - 1)), color = c("white", rep("white", times = length(percentiles) - 1)), hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Recall para cada percentil de probabilidades",
         caption = "\n Recall: qué porcentaje de los Observados Positivos son identificados por el modelo") + scale_x_continuous(breaks = percentiles*100)
  return(plot)
}

Grafica_recall(pgorro = pgorro, master_test = master_test)

#' Exporta grafica con Precision para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param master_test la master de master_test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_precision(pgorro = pgorro, conf_matrix = conf_matrix, master_test = master_test)
#' 

Grafica_precision <- function(pgorro, master_test, percentiles = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09)) {
  
  master_precision <- matrix(NA, nrow = length(percentiles), ncol = 2)
  master_precision[, 1] <- percentiles*100
  colnames(master_precision) <- c("Percentil", "Precision")
  
  for (i in 1:length(percentiles)){
    master_precision[i, 2] <- Precision(conf_matrix = confusion_matrix(predict = pgorro, 
                                                                       response = master_test$Y, 
                                                                       threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  master_precision <- data.table(master_precision)
  plot <- ggplot(data=master_precision, aes(x= Percentil, y=Precision, fill = -Precision)) +
    geom_bar(stat="identity")+
    geom_text(label = round(master_precision$Precision, digits = 2) , size = 7, vjust = c(1.5, rep(-1, times = length(percentiles) - 1)), color = c("white", rep("black", times = length(percentiles) - 1)), hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Precision para cada percentil de probabilidades",
         caption = "\n Precision: qué porcentaje de los que el modelo clasifica como positivos, efectivamente lo son.") + scale_x_continuous(breaks = percentiles*100)
  
  return(plot)
}


Grafica_precision(pgorro = pgorro, master_test = master_test)


