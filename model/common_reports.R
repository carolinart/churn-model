#' Create reports for the dt table
#' @param dt: data used to make the performance report (data.table)
#' @param path:  path where all the models and reports are stored (character) 
#' @param modelFolder: Specific folder for this model and data.(character) 
#' @param alias: sufix which indicates the data origin (train, test or dev) (character)
#' @return: it writes the output files in the specified path.
performanceReport <-
  function(dt,
           path = models_path,
           modelFolder = NULL,
           alias = "train") {
    #pintar cada gini en la carpeta modelo
    pintaGini(dt$target, dt$pred, os.path.join(
      path,
      modelFolder,
      paste0(modelFolder, "_", alias, "_gini.png")
    ))
    
    uplift(
      true = dt$target,
      prob = dt$pred,
      filepath = os.path.join(
        path,
        modelFolder,
        paste0(modelFolder, "_", alias, "_uplift.csv")
      ),
      primerCorte = 0.1,
      salto = 0.1
    )
  }

#' Plots the ROC curve if a filepath is specified and returns the GINI value
#' @param actual: the target value (logical|integer {0,1} vector)
#' @param predicted: the score value (numeric vector)
#' @param filepath: filepath to store the roc curve plots. If not specified, 
#' the figures will not be generated (character)
#' @param lang: language to use to generate the labels of the plot (character {"eng"|"esp"})
#' @return: the gini value (numeric)
pintaGini <-
  function(actual,
           predicted,
           filepath = NULL,
           lang = "eng") {
    require(ROCR)
    perf <-
      performance(pred <- prediction(predicted, actual), "tpr", "fpr")
    gini <-
      (unlist(performance(pred, measure = "auc")@y.values) - 0.5) / 0.5
    auc <- unlist(performance(pred, measure = "auc")@y.values)
    ks <- max(unlist(perf@y.values) - unlist(perf@x.values))
    if (!is.null(filepath)) {
      if (lang == "eng") {
        yl = "% true positives"
        xl = "% false positives"
      } else {
        yl = "% verdaderos positivos"
        xl = "% falsos positivos"
      }
      png(paste0(filepath), height = 700, width = 700)
      par(mar = c(5.1, 5.1, 4.1, 2.1))
      plot(
        x = 100 * unlist(perf@x.values),
        y = 100 * unlist(perf@y.values),
        type = "l",
        col = "#00598e",
        xaxs = "i",
        lwd = 2,
        yaxs = "i",
        xlab = xl,
        ylab = yl,
        font.lab = 2,
        cex.lab = 1.2
      )
      abline(a = 0, b = 1, lty = 5)
      legend(
        "bottomright",
        "hi",
        paste0(
          "AUC = ",
          round(auc, 2),
          "\n",
          "Gini = ",
          round(gini*100, 2),
          "%\n",
          "Ks = ",
          round(100 * ks, 2),
          "%\n\n"
        ),
        text.font = 2,
        cex = 1.2
      )
      dev.off()
    }
    
    return(gini)
  }

#' Calculates the uplift for the selected buckets
#' @param true: vector of true values
#' @param prob: vector of scores given by the model
#' @param filepath: file where the output is going to be exported
#' @param primerCorte: first interval
#' @param salto: length of the interval
#' @return: output, data table with the requested info.
uplift <-
  function(true,
           prob,
           filepath,
           primerCorte = 1,
           salto = 0.1) {
    dt <- data.table(Prob = prob,
                     Target = true,
                     Exposition = exp)
    setorder(dt,-Prob)
    Nt <- nrow(dt)
    Nc <- sum(dt$Target == 1)
    Mean_Compradores <- mean(dt$Target)
    
    dt$ID <- (1:Nt) / Nt * 100
    porcentajesAnalizados <- seq(primerCorte, 100, by = salto)
    
    if (max(porcentajesAnalizados) < 100) {
      porcentajesAnalizados <- c(porcentajesAnalizados, 100)
    }
    
    dt$Bin <-
      findInterval(dt$ID,
                   porcentajesAnalizados,
                   rightmost.closed = F,
                   left.open = T)
    
    dt <-
      dt[, .(
        Clientes = .N,
        Compradores = sum(Target == 1),
        porcentajeNeto = sum(Target == 1) / Nc,
        Uplift.Segmento = mean(Target) / Mean_Compradores,
        porcentajeCompradoresEnSoloEsteSegmento = mean(Target)
      ), by = .(Bin)]
    
    dt <- cbind(dt, porcentajesAnalizados)
    dt$Bin <- NULL
    
    dt$totalClientesAcumulados <- dt$Clientes %>% cumsum
    dt$totalCompradoresAcumulados <- dt$Compradores %>% cumsum
    dt$porcentajesAtrapadosAcumulados <- dt$porcentajeNeto %>% cumsum
    
    dt$porcentajesCompradoresEnSegmento <-
      dt$totalCompradoresAcumulados / dt$totalClientesAcumulados
    dt$Uplift.Acumulado <- dt$porcentajesCompradoresEnSegmento / Mean_Compradores
    
    # Order as the old subroutines + 2 new columns with uplifts:
    names_order <- c("porcentajesAnalizados", names(dt)[names(dt) != 
                                                          "porcentajesAnalizados"])
    setcolorder(dt, names_order)
    
    fwrite(dt, file = filepath)
    
  }

#' Create distribution for the dt table
#' @param dt: data used to make the performance report (data.table)
#' @param mostImp: important variables
#' @param outputPath: output path
#' @return: it writes the output files in the specified path.
exportQuantile <- function(dt, mostImp, outputPath){
  quantile <- calculateVariablesDistribution(dt, "pred")
  quantile <- merge(x = mostImp[, .(Feature, Gain)],
                    y = quantile,
                    all.x = T,
                    all.y = T,
                    by.x = "Feature",
                    by.y = "Var")
  quantile[, Category := str_split_fixed(Feature, "_", 2)[,1]]
  setcolorder(quantile, c("Feature", "Category", setdiff(names(quantile), c("Feature", "Category"))))
  quantile[is.na(Gain), Gain := 0]
  setorder(quantile, -Gain)
  fwrite(quantile, outputPath, row.names = F)
}
#' Create distribution for the dt table
#' @param dt: data used to make the performance report (data.table)
#' @param pred: variable that corresponds to the score
#' @return: it writes the output files in the specified path.
calculateVariablesDistribution <- function(dt, pred){
  
  # Numeric variables
  numVars <- names(dt)[dt[, sapply(.SD, is.numeric)]]
  setorderv(dt, cols = pred, order = -1)
  i <- 0
  resumen <- data.table()
  
  for(numVar in numVars){
    print(numVar)
    
    aux <- dt[get(numVar) != -999999999999, c(numVar, pred), with = F]
    if(nrow(aux)){
      aux$Quantile <- ntile(aux[[pred]], 10)
      aux <- aux[, mean(get(numVar)), by = Quantile]
      aux[, Var := numVar]
      aux <- dcast(data = aux, formula = "Var ~ Quantile", value.var = "V1")
      resumen <- rbind(resumen, aux, fill = TRUE)
    }
  }
  setnames(resumen, setdiff(names(resumen), "Var"), paste0("Decile_", setdiff(names(resumen), "Var")))
  return(resumen)
}

#' Calculate 1 and 0 vector from a probabilities vector
#' @param dt: data used to make the performance report (data.table)
#' @param target: target name vector on dt
#' @param pred: probablities name vector on dt
#' @return: dt with vector.
calculate_pred <- function(dt, target, prediction) {
  require(pROC)
  rc <-
    roc(response = dt[[which(names(dt) == target[1])]],
        predictor = dt[[which(names(dt) == prediction[1])]])
  
  pUmbral <- coords(rc, "best", ret = "threshold")
  
  
  dt[, pred_num := ifelse(dt[[which(names(dt) == prediction[1])]] >= pUmbral, 1, 0), ]
  setnames(dt, "pred_num", paste0(prediction, "_num"))
           
  return(dt)
}
