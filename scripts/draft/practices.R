


Tabla <- data.table(Y = c(1,0,1), X1 = c(2,6,2), X2 = c(1,2,3)) #X1 no es continua sino categorica

set.seed(1905)

#porc_training <- 0.8

#ID_training <- sort(sample(nrow(Tabla), nrow(Tabla)*porc_training))
#Training <- Tabla[ID_training]
#Test <- Ahorros[-ID_training]

Training <- basee[periodo %in% c("201808", "201809", "201810", "201811")]
Test <- basee[periodo %in% c("201812")]

if (nrow(master_train) + nrow(master_test) != nrow(master)){
  stop("Hay un error en la separacion de data.")
}



Logit <- glm(formula = Y ~ 
               factor(X1) + #para categorica 
               X2, 
             family = binomial(link = "logit"), data = Training)

#pgorro <- predict(Logit, type = "response", newdata = Training)
pgorro <- predict(Logit, type = "response", newdata = Test)




#' Extraer grafica del ROC relacionado a Test, con AUC graficado
#'
#' @param pgorro vector de probabilidades predichas para Test.
#' @param Test vector de realizaciones del proceso de la variable dependiente para Test.
#'
#' @return En la ruta, carpeta "Graficas/modelo_#", se presenta la grafica mencionada.
#'
#' @examples
#' Extraer_ROC(pgorro = pgorro, Test = Test)
#' 
ROC <- function(pgorro, Test) {
  
  ROCRpred <- prediction(pgorro, Test$Y)
  ROCRperf <- performance(ROCRpred, 'tpr','fpr')
  auc_ROCR <- performance(ROCRpred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  plot(ROCRperf, colorize = F, text.adj = c(-0.2,1.7), xlab = "1 - Specificity", ylab = "Recall") 
  legend(0.2, 0.6, paste0("AUC = ", format(auc_ROCR, digits = 4)), border="white", cex=1, box.col = "white")

}

ROC(pgorro = pgorro, Test = Test)



#' Exporta tabla con matriz de confusion
#'
#' @param predict vector de probabilidades predichas.
#' @param response vector con realizaciones. Ojo: no la tabla, sino el vector.
#' @param threshold debe ser un valor entre 0 y 1. Define con que probabilidad es el limite entre decir que una probabilidad 
#' es mas cercana a 0 o 1. Si no se pone nada, se asume el thresold optimo.
#' Esto afecta a la matriz de confusion principalmente.
#'
#' @return Regresa la matrix.
#'
#' @examples
#' conf_matrix <- Confusion_matrix(predict = pgorro, response = Test$Y)
#' conf_matrix <- Confusion_matrix(predict = pgorro, response = Test$Y, threshold = 0.1)
#' 
Confusion_matrix <- function(predict, response, threshold = "opt_threshold") {
  
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
#' @param Test la tabla de Test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_uplift(pgorro = pgorro, conf_matrix = conf_matrix, Test = Test)
#' 

Grafica_uplift <- function(pgorro, Test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  
  Tabla_uplift <- matrix(NA, nrow = length(percentiles), ncol = 2)
  Tabla_uplift[, 1] <- percentiles*100
  colnames(Tabla_uplift) <- c("Percentil", "Uplift")
  
  for (i in 1:length(percentiles)){
    Tabla_uplift[i, 2] <- Uplift(conf_matrix = confusion_matrix(predict = pgorro, 
                                                                response = Test$Y, 
                                                                threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  Tabla_uplift <- data.table(Tabla_uplift)
  plot <- ggplot(data=Tabla_uplift, aes(x= Percentil, y=Uplift, fill = -Uplift)) +
    geom_bar(stat="identity")+
    geom_text(label = round(Tabla_uplift$Uplift, digits = 1) , size = 8, vjust = c(1.5, rep(-1, times = length(percentiles) - 1)), color = c("white", rep("black", times = length(percentiles) - 1)), hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Uplift para cada decil de probabilidades",
         caption = "\n Uplift: el modelo cuantas veces mejoran el Recall y Precision, con respecto a una seleccion aleatoria") + scale_x_continuous(breaks = percentiles*100)
  
}

Grafica_uplift(pgorro = pgorro, Test = Test)

#' Exporta grafica con Recall para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param Test la tabla de Test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_recall(pgorro = pgorro, conf_matrix = conf_matrix, Test = Test)
#' 
Grafica_recall <- function(pgorro, Test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  
  Tabla_recall <- matrix(NA, nrow = length(percentiles), ncol = 2)
  Tabla_recall[, 1] <- percentiles*100
  colnames(Tabla_recall) <- c("Percentil", "Recall")
  
  for (i in 1:length(percentiles)){
    Tabla_recall[i, 2] <- Recall(conf_matrix = Confusion_matrix(predict = pgorro, 
                                                                response = Test$Y, 
                                                                threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  Tabla_recall <- data.table(Tabla_recall)
  plot <- ggplot(data=Tabla_recall, aes(x= Percentil, y=Recall, fill = -Recall)) +
    geom_bar(stat="identity")+
    geom_text(label = round(Tabla_recall$Recall, digits = 1) , size = 8, vjust = c(1.5, rep(1.5, times = length(percentiles) - 1)), color = c("white", rep("white", times = length(percentiles) - 1)), hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Recall para cada decil de probabilidades",
         caption = "\n Recall: qué porcentaje de los Observados Positivos son identificados por el modelo") + scale_x_continuous(breaks = percentiles*100)
  
}

Grafica_recall(pgorro = pgorro, Test = Test)

#' Exporta grafica con Precision para diferentes percentiles
#'
#' @param pgorro vector con probabilidades predichas.
#' @param Test la tabla de Test.
#' @param percentiles los percentiles se quieren graficar. Estan por default todos los deciles.
#'
#' @return Exporta la grafica mencionada.
#'
#' @examples
#' Grafica_precision(pgorro = pgorro, conf_matrix = conf_matrix, Test = Test)
#' 
Grafica_precision <- function(pgorro, Test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  
  Tabla_precision <- matrix(NA, nrow = length(percentiles), ncol = 2)
  Tabla_precision[, 1] <- percentiles*100
  colnames(Tabla_precision) <- c("Percentil", "Precision")
  
  for (i in 1:length(percentiles)){
    Tabla_precision[i, 2] <- Precision(conf_matrix = Confusion_matrix(predict = pgorro, 
                                                                      response = Test$Y, 
                                                                      threshold = quantile(pgorro, probs = 1 - percentiles[i])))
  }
  
  Tabla_precision <- data.table(Tabla_precision)
  plot <- ggplot(data=Tabla_precision, aes(x= Percentil, y=Precision, fill = -Precision)) +
    geom_bar(stat="identity")+
    geom_text(label = round(Tabla_precision$Precision, digits = 1) , size = 8, vjust = c(1.5, rep(-1, times = length(percentiles) - 1)), color = c("white", rep("black", times = length(percentiles) - 1)), hjust = 0.5) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.caption = element_text(size = 12)) + 
    labs(title = "Precision para cada decil de probabilidades",
         caption = "\n Precision: qué porcentaje de los que el modelo clasifica como positivos, efectivamente lo son.") + scale_x_continuous(breaks = percentiles*100)
  
  
}


Grafica_precision(pgorro = pgorro, Test = Test)

