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

Grafica_uplift <- function(pgorro, master_test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  
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
    labs(title = "Uplift para cada decil de probabilidades",
         caption = "\n Uplift: el modelo cuantas veces mejoran el Recall y Precision, con respecto a una seleccion aleatoria") + scale_x_continuous(breaks = percentiles*100)
  
  return(plot)
}


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

Grafica_recall <- function(pgorro, master_test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  
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
    labs(title = "Recall para cada decil de probabilidades",
         caption = "\n Recall: qué porcentaje de los Observados Positivos son identificados por el modelo") + scale_x_continuous(breaks = percentiles*100)
  return(plot)
}


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

Grafica_precision <- function(pgorro, master_test, percentiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  
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
    labs(title = "Precision para cada decil de probabilidades",
         caption = "\n Precision: qué porcentaje de los que el modelo clasifica como positivos, efectivamente lo son.") + scale_x_continuous(breaks = percentiles*100)
  
  return(plot)
}
