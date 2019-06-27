
create_model <- function(models_path,
                         model_alias,
                         master_path,
                         train_months,
                         dev_months,
                         test_months) {
  ##### model meta data ####
  print("Making model's fold")
  
  # model product folder
  model_alias_modeling <- os.path.join(models_path, model_alias)
  dir.create(model_alias_modeling)
  
  # models folder
  models_folder <-
    os.path.join(model_alias_modeling, "models_library")
  dir.create(models_folder)
  
  # plots folder
  plots_folder <- os.path.join(model_alias_modeling, "plots")
  dir.create(plots_folder)
  
  ##### load data ####
  print("Loading master table")
  master_path <- os.path.join(master_path, "train")
  master_files <- list.files(master_path, full.names = T)
  
  #substract dates from master file path
  position <-
    sapply(str_extract_all(master_files, "[0-9]+"), "[[", 1) %>% as.numeric()
  position <- paste0(position, "01")
  
  position <- as.Date(position, format = "%Y%m%d")
  
  # create a reference table of master files
  master_position <-
    data.frame(files = master_files, position = position)
  
  # comparing cut months from user to master files created
  if (FALSE %in% (train_months %in% master_position$position)) {
    stop("Does not exist master table for some of the train months specified")
  }
  if (FALSE %in% (dev_months %in% master_position$position)) {
    stop("Does not exist master table for some of the development months specified")
  }
  if (FALSE %in% (test_months %in% master_position$position)) {
    stop("Does not exist master table for some of the test months specified")
  }
  
  files_train <-
    master_position[master_position$position >= min(train_months) &
                      master_position$position <= max(train_months),]
  files_train <- files_train[1]
  
  files_dev <-
    master_position[master_position$position >= min(dev_months) &
                      master_position$position <= max(dev_months),]
  files_dev <- files_dev[1]
  
  files_test <-
    master_position[master_position$position >= min(test_months) &
                      master_position$position <= max(test_months),]
  files_test <- files_test[1]
  
  files <- rbind(files_train, files_dev, files_test)
  files <- as.character(files$files)
  
  datos_list <- list()
  
  for (i in 1:length(files)) {
    print(paste("Loading datos from", files[i]))
    datos_list[[i]] <- readRDS(files[i])
  }
  
  datos <- rbindlist(datos_list, use.names = T, fill = T)
  rm(datos_list)
  gc()
  
  
  #last cleaning
  # datos[, crm_estrato := paste0("estr_", crm_estrato)]
  # datos[, crm_estrato := factor(crm_estrato, levels = c(paste0("estr_", 0:6)))]
  
  
  #Definir el target 
  datos[tipo_cancelacion == "Sin Cancelacion",  target := 0]
  datos[tipo_cancelacion == "Cancelacion Voluntaria",  target := 1]
  datos[, tipo_cancelacion := NULL]
  
  
  #Chosen variables
  datos <-
    datos[, .(
      target,
      periodo,
      cod_int,
      nro_id,
      antiguedad,
      sdo_total,
      vlr_ult_compra,
      vlr_ult_av,
      # util,
      mes_venci,
      dias_ult_pago,
      crm_valor_egreso_mes,
      crm_edad,
      crm_antiguedad,
      cant_total_prod,
      cuotas_median,
      sdo_capt_total_tmenos1,
      util_tmenos1,
      dias_ult_compra_tmenos1,
      cartera_capital_total
    )]
  #15
  #con ingreso se sobre ajusta el modelo
  
  
  # set.seed(666)
  # #Sample
  # index <- base::sample(1:nrow(datos), 0.9*(nrow(datos)))
  # datos <- datos[index]
  # rm(index)
  # gc()
  
  # #Balance
  # balance <- datos[target == 0]
  # index <- sample(1:nrow(balance), 200000)
  # balance <- balance[index]
  # target <- datos[target == 1]
  # index <- sample(1:nrow(target), 50000)
  # target <- target[index]
  #
  # base_list <- list(target, balance)
  # datos <- rbindlist(base_list)
  #
  # rm(base_list, balance, target, index)
  # gc()
  
  
  #partition of data
  
  dev <-
    datos[periodo ==  dev_months[1]]
  # & periodo <=  dev_months[2]]
  
  master <- datos[periodo >= train_months[1] &
                    periodo <= train_months[2]]
  
  test <- datos[periodo == test_months]
  
  #Separating variables
  id_variables <-
    c("cod_int", "nro_id" , "periodo", "target")
  
  # crm_vars <-
  #   names(master)[names(master) %!in% c(id_variables)]
  # # crm_vars <- grep("crm_", crm_vars, value = T)
  # #
  # # tc_vars <-
  # #   names(master)[names(master) %!in% c(id_variables, crm_vars)]
  # 
  # categorical_cols <-
  #   c("crm_genero")
  # 
  
  numeric_cols <-
    names(master)[names(master) %!in% c(id_variables)]
  
  
  
  # var_num <-
  #   c(
  #     "crm_valor_activos",
  #     "crm_valor_ing_bru_mes",
  #     "crm_valor_egreso_mes",
  #     "crm_valor_pasivos",
  #     "crm_edad",
  #     "crm_antiguedad"
  #   )
  #
  # #Variables numericas
  # crm[, (var_num) := lapply(.SD, as.numeric), .SDcols = var_num]
  # no_factor <- c(id_vars, var_num)
  #
  #
  # #Variables factor
  # factor_vars <- names(crm)[!(names(crm) %in% no_factor)]
  # crm[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
  # crm <- crm[!is.na(crm_numero_identificacion)]
  
  
  # categorical_cols <-
  #   c(crm_vars[sapply(master[, mget(crm_vars)], is.factor)],
  #     tc_vars[sapply(master[, mget(tc_vars)], is.factor)])
  #
  # numeric_cols <-
  #   names(master)[names(master) %!in% c(id_variables, categorical_cols)]
  #
  
  # one-hot encode the categorical features
  print("One hot encoding")
  final_cols <- c("target", numeric_cols)
  
  ##### making xgb data ####
  master_dmatrix <- master[, mget(final_cols)]
  master_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = master_dmatrix)
  
  dev_dmatrix <- dev[, mget(final_cols)]
  dev_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = dev_dmatrix)
  
  test_dmatrix <- test[, mget(final_cols)]
  test_dmatrix <-
    sparse.model.matrix(target ~ . - 1, data = test_dmatrix)
  
  model_cols <- master_dmatrix@Dimnames[[2]]
  
  model_cols <- tolower(model_cols)
  model_cols <-
    stri_trans_general(model_cols, "Latin-ASCII")
  model_cols <- tm::removePunctuation(model_cols)
  model_cols <- gsub(" ", "", model_cols, fixed = TRUE)
  
  master_dmatrix@Dimnames[[2]] <- model_cols
  dev_dmatrix@Dimnames[[2]] <- model_cols
  test_dmatrix@Dimnames[[2]] <- model_cols
  
  
  # separate target
  target_train_dmatrix <-
    as(data.matrix(master$target), 'dgCMatrix')
  target_dev_dmatrix <-
    as(data.matrix(dev$target), 'dgCMatrix')
  
  dtrain <-
    xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)
  ddev <-
    xgb.DMatrix(data = dev_dmatrix, label = target_dev_dmatrix)
  
  
  rm(target_dev_dmatrix, target_train_dmatrix)
  gc()
  
  watchlist <- list(train = dtrain, test = ddev)
  # # set random seed for reproducibility
  set.seed(1104)
  
  cores <- parallel::detectCores() - 1
  
  
  ##### training xgboost model ####
  # xgboost maximizing AUC of ROC
  
  print("Training xgboost model using ROC curve")
  
  xgb.parameters <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    early_stoping_round = 10,
    nrounds = 250,
    max_depth = 4
  )
  
  model_xgb_auc <- xgb.train(
    data = dtrain,
    nround = xgb.parameters$nrounds,
    params = xgb.parameters,
    early_stopping_rounds = xgb.parameters$early_stoping_round,
    verbose = 1 ,
    nthread = cores,
    watchlist = watchlist
  )
  
  xgb.save(model_xgb_auc,
           os.path.join(models_folder, "xgb_auc.model"))
  
  save(
    final_cols,
    model_cols,
    master_dmatrix,
    dev_dmatrix,
    test_dmatrix,
    master,
    dev,
    test,
    file = os.path.join(model_alias_modeling, "xgb_objects.RData")
  )
  
  ##### Predicciones ####
  
  # exportar probabilidades
  
  master[, xgb_auc_pred := predict(model_xgb_auc, master_dmatrix)]
  master <- calculate_pred(master, "target", "xgb_auc_pred")
  
  dev[, xgb_auc_pred := predict(model_xgb_auc, dev_dmatrix)]
  dev <- calculate_pred(dev, "target", "xgb_auc_pred")
  
  test[, xgb_auc_pred := predict(model_xgb_auc, test_dmatrix)]
  test <- calculate_pred(test, "target", "xgb_auc_pred")
  
  
  fwrite(master[, .(cod_int,
                    periodo,
                    target,
                    xgb_auc_pred,
                    xgb_auc_pred_num)],
         os.path.join(model_alias_modeling, "pred_train.csv"))
  
  fwrite(dev[, .(cod_int,
                 periodo,
                 target,
                 xgb_auc_pred,
                 xgb_auc_pred_num)],
         os.path.join(model_alias_modeling, "pred_dev.csv"))
  
  fwrite(test[, .(cod_int,
                  periodo,
                  target,
                  xgb_auc_pred,
                  xgb_auc_pred_num)],
         os.path.join(model_alias_modeling, "pred_test.csv"))
  
  ##### metrics xgb #####
  metrics_xgb <-
    data.frame(
      train = c(
        Metrics::accuracy(master$target, master$xgb_auc_pred_num),
        Metrics::precision(master$target, master$xgb_auc_pred_num),
        Metrics::recall(master$target, master$xgb_auc_pred_num),
        Metrics::auc(master$target, master$xgb_auc_pred)
      ),
      dev = c(
        Metrics::accuracy(dev$target, dev$xgb_auc_pred_num),
        Metrics::precision(dev$target, dev$xgb_auc_pred_num),
        Metrics::recall(dev$target, dev$xgb_auc_pred_num),
        Metrics::auc(dev$target, dev$xgb_auc_pred)
      ),
      test = c(
        Metrics::accuracy(test$target, test$xgb_auc_pred_num),
        Metrics::precision(test$target, test$xgb_auc_pred_num),
        Metrics::recall(test$target, test$xgb_auc_pred_num),
        Metrics::auc(test$target, test$xgb_auc_pred)
      )
    )
  rownames(metrics_xgb) <-
    c("accuracy", "precision", "recall", "auc")
  write.csv(metrics_xgb,
            os.path.join(model_alias_modeling, "metrics_xgb.csv"))
  
  ######   Uplift of models on test #####
  
  uplift(
    true = test$target,
    prob = test$xgb_auc_pred,
    filepath = os.path.join(
      model_alias_modeling,
      paste0("xgboost", "_test_uplift.csv")
    ),
    primerCorte = 0.1,
    salto = 0.1
  )
  
  
  # Important variables
  importance_matrix <-
    xgb.importance(feature_names =  model_cols, model = model_xgb_auc)
  fwrite(
    importance_matrix,
    os.path.join(model_alias_modeling, "xgb_auc_important_variables.csv")
  )
  
  # Gain
  p1 <-
    xgb.ggplot.importance(
      importance_matrix,
      measure = "Gain",
      rel_to_first = FALSE,
      top_n = 20
    )
  p1 <- p1 + ggplot2::ylab("Gain")
  ggsave(filename = os.path.join(plots_folder, "gain_xgboost_auc.png"),
         plot = p1)
  
  # Cover
  p1 <-
    xgb.ggplot.importance(
      importance_matrix,
      measure = "Cover",
      rel_to_first = FALSE,
      top_n = 20
    )
  p1 <- p1 + ggplot2::ylab("Cover")
  ggsave(filename = os.path.join(plots_folder, "cover_xgboost_auc.png"),
         plot = p1)
  
  # Frequency
  p1 <-
    xgb.ggplot.importance(
      importance_matrix,
      measure = "Frequency",
      rel_to_first = FALSE,
      top_n = 20
    )
  p1 <- p1 + ggplot2::ylab("Frequency")
  ggsave(
    filename = os.path.join(plots_folder, "frequency_xgboost_auc.png"),
    plot = p1
  )
  
  
  xgb_auc_train <-
    pROC::roc(response = master$target,
              predictor = master$xgb_auc_pred)
  
  xgb_auc_dev <-
    pROC::roc(response = dev$target,
              predictor = dev$xgb_auc_pred)
  
  xgb_auc_test <-
    pROC::roc(response = test$target,
              predictor = test$xgb_auc_pred)
  
  rocs <- list("train" = xgb_auc_train,
               "dev" = xgb_auc_dev,
               "test" = xgb_auc_test)
  breaks = seq(0, 1, 0.1)
  legendTitel = "Datos"
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
  tt <-
    ttheme_default(colhead = list(fg_params = list(parse = TRUE)))
  matrix <- tableGrob(aucs, theme = tt)
  
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
    labs(title = paste("Curvas ROC y AUC en churn tc")) +
    guides(colour = guide_legend(legendTitel)) +
    theme(axis.ticks = element_line(color = "grey80"))
  
  p1 <- grid.arrange(
    rocPlot,
    matrix,
    nrow = 2,
    as.table = TRUE,
    heights = c(3, 1)
  )
  
  ggsave(file = os.path.join(model_alias_modeling, "auc_all.png"),
         plot = p1)
  
}
