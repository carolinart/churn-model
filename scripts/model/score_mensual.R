

score_mensual <- function(model_alias_scoring, date_to_score, performance_calculation){

##### model meta data ####
print("Making folds")


# model performance/predict folder

  if (performance_calculation  == TRUE) {
    performance_path <-
      os.path.join(performance_path, date_to_score)
    dir.create(performance_path)
    datos_files <-
      list.files(os.path.join(master_path, "train"), full.names = T)
  } else{
    prediction_path <-
      os.path.join(prediction_path, date_to_score)
    dir.create(prediction_path)
    datos_files <-
      datos_files <-
      list.files(os.path.join(master_path, "score"), full.names = T)
  }

##### load data ####
datos_files <- datos_files

#substract dates from datos file path
position <-sapply(str_extract_all(datos_files, "[0-9]+"), "[[", 1)%>% as.numeric()
position <- paste0(position, "01")

position <- as.Date(position, format = "%Y%m%d")

# create a reference table of datos files 
datos_position <- data.frame(files = datos_files, position = position)

#compare score
compare <- as.Date(paste0(date_to_score, "01"), format = "%Y%m%d")

if (FALSE %in% (compare %in% datos_position$position)) {
  stop("Does not exist datos table for some of the train months specified")
}

print("Loading data")
datos <-
  datos_position$files[datos_position$position == compare] %>% as.character() %>%  readRDS()

print("Cleaning data")
#last cleaning

datos[, crm_estrato := paste0("estr_", crm_estrato)]
datos[, crm_estrato := factor(crm_estrato, levels = c(paste0("estr_", 0:6)))]

datos[, target := ifelse(tipo_cancelacion_futuro == "Sin Cancelacion", 0, 1)]
datos[, tipo_cancelacion_futuro := NULL]
datos[, ciclo := NULL]

print("Loading separating variables")
#Separating variables
id_variables <-
  c("cod_int", "nro_id" , "periodo", "target")

crm_vars <-
  names(datos)[names(datos) %!in% c(id_variables)]
crm_vars <- grep("crm_", crm_vars, value = T)

tc_vars <- names(datos)[names(datos) %!in% c(id_variables, crm_vars)]

categorical_cols <-
  c(crm_vars[sapply(datos[, mget(crm_vars)], is.factor)],
    tc_vars[sapply(datos[, mget(tc_vars)], is.factor)])

numeric_cols <- names(datos)[names(datos) %!in% c(id_variables, categorical_cols)]

print("One hot encoding")
# one-hot encode the categorical features
final_cols <- c("target", categorical_cols, numeric_cols)

##### making xgb data ####  
print("making xgb data")

datos_dmatrix <- datos[, mget(final_cols)]
datos_dmatrix <-
  sparse.model.matrix(target ~ . - 1, data = datos_dmatrix)

model_cols <- datos_dmatrix@Dimnames[[2]]
model_cols <- tolower(model_cols)
model_cols <- 
  stri_trans_general(model_cols,"Latin-ASCII")
model_cols <- tm::removePunctuation(model_cols)
model_cols <- gsub(" ", "", model_cols, fixed = TRUE)

datos_dmatrix@Dimnames[[2]] <- model_cols

#### Cargar modelo ####

print("...Loading model...")

model_files <- os.path.join(models_path, model_alias_scoring, "models_library") %>% list.files(full.names = T) 

model_xgb_auc <-
  grep(pattern = ".model", model_files, value = T ) %>% xgb.load()

##### Predicciones ####
print("Make predictions / performance")
datos[, xgb_auc_pred := predict(model_xgb_auc, datos_dmatrix)]

if(performance_calculation == FALSE){
  fwrite(datos[, .(cod_int,
                    periodo,
                    xgb_auc_pred
  )],
  os.path.join(prediction_path, paste0(model_alias_scoring, "_performance.csv")))
}

# exportar probabilidades 
if (performance_calculation == TRUE) {
  datos <- calculate_pred(datos, "target", "xgb_auc_pred")
  fwrite(datos[, .(cod_int,
                   periodo,
                   target,
                   xgb_auc_pred,
                   xgb_auc_pred_num
  )],
  os.path.join(performance_path, paste0(model_alias_scoring, "_predict.csv")))
  
  ##### metrics xgb #####
  metrics_xgb <-
    data.frame(datos = c(
      Metrics::accuracy(datos$target, datos$xgb_auc_pred_num),
      Metrics::precision(datos$target, datos$xgb_auc_pred_num),
      Metrics::recall(datos$target, datos$xgb_auc_pred_num),
      Metrics::auc(datos$target, datos$xgb_auc_pred)
    ))
    
    rownames(metrics_xgb) <- c("accuracy", "precision", "recall", "auc")
    write.csv(metrics_xgb, os.path.join(performance_path, paste0(model_alias_scoring, "_metrics_xgb.csv")))
    
    ######   Uplift of models on datos #####
    
    uplift(
      true = datos$target,
      prob = datos$xgb_auc_pred,
      filepath = os.path.join(
        performance_path,
        paste0(model_alias_scoring, "_uplift.csv")
      ),
      primerCorte = 0.1,
      salto = 0.1
    )
    
    xgb_auc_datos <-
      pROC::roc(response = datos$target,
                predictor = datos$xgb_auc_pred)
    
    rocs <- list("Mes" = xgb_auc_datos)
    breaks = seq(0, 1, 0.1)
    RocVals <- plyr::ldply(names(rocs), function(rocName) {
      if (class(rocs[[rocName]]) != "roc") {
        stop("Please provide roc object from pROC package")
      }
      data.frame(
        fpr = rev(rocs[[rocName]]$specificities),
        tpr = rev(rocs[[rocName]]$sensitivities),
        names = rep(rocName, length(rocs[[rocName]]$sensitivities)),
        stringAsFactors = T
      )
    })
    RocVals <- data.table(RocVals)
    set.seed(666)
    RocVals <- RocVals[sample(.N, 1000)]
    AUC <- sapply(rocs, "[[", "auc")
    aucs <- data.frame(AUC)
    aucs$AUC <- round(aucs$AUC, 3)
    aucs <- t(aucs)
    tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
    matrix <- tableGrob(aucs, theme=tt)
    
    rocPlot <-
      ggplot(RocVals, aes(x = fpr, y = tpr, colour = names)) +
      geom_segment(aes(
        x = 0,
        y = 1,
        xend = 1,
        yend = 0
      ),
      alpha = 0.5,
      colour = "gray") +
      geom_step() +
      scale_x_reverse(name = "Tasa de falsos positivos  (1 - Especificidad)",
                      limits = c(1, 0),
                      breaks = breaks) +
      scale_y_continuous(name = "Tasa de verdadderos positivos (Sensitividad)",
                         limits = c(0, 1),
                         breaks = breaks) +
      theme_bw() +
      coord_equal() +
      labs(title = paste("Curva ROC y AUC en churn tc en el mes",   date_to_score))+
      theme(legend.position = "none")+
      theme(axis.ticks = element_line(color = "grey80"))
    
    p1 <- grid.arrange(
      rocPlot,
      matrix,
      nrow = 2,
      as.table = TRUE,
      heights = c(3, 1)
    )
    
    ggsave(file = os.path.join(performance_path, paste0(model_alias_scoring, "auc.png" )),
           plot = p1)
    
  
   }

}

















  
