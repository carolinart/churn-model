i = 2
##### model meta data ####
print("Making model's fold")
gc()
model_alias_modeling <-
  paste0("prueba_", today() %>% format(., "%Y%m%d"), "_", products[i])
model_type_modeling <- products[i]
# model product folder
model_alias_modeling <- os.path.join(models_path, model_alias_modeling)
dir.create(model_alias_modeling)

# models folder
models_folder <- os.path.join(model_alias_modeling, "models_library")
dir.create(models_folder)

# plots folder 
plots_folder <- os.path.join(model_alias_modeling, "plots")
dir.create(plots_folder)

##### load data ####
print("Loading master table ")
data <- get.path(master_path, "master") %>% readRDS

print(paste("Loading data for", products[i]))

if(products[i] == "cancer"){data <- data[oferta == 673]}
if(products[i] == "pif"){data <- data[oferta == 676]}
if(products[i] == "fraude"){data <- data[oferta == 682]}

# fixing periodo variable that daniel shit on it
data[, periodo1 := yearmon(periodo1)]
data[, periodo := as.Date(periodo1)]
data[, periodo1 := NULL]
data[, target := factor(target)]
data[, oferta := NULL]
data[, codigo_subproducto := factor(codigo_subproducto)]

# divinding master table
test <- data[periodo == test_month]
master <- data[periodo != test_month]
set.seed(123)
id <- sample(1:nrow(master), nrow(master)*0.65)
dev <- master[!id]
master <- master[id]

##### separating variabbles #### 

id_variables <- c("definit", "periodo", "target")
cols <- names(master)
cols <- cols[cols %!in% id_variables]
# dropping extra variables
cols <- cols[cols %!in% c("crm_grupo_ocupacion")]

categorical_cols <-
  c(cols[sapply(master[, mget(cols)], is.factor)])

numeric_cols <-
  c(cols[sapply(master[, mget(cols)], is.numeric)])

print("One hot encoding")
# one-hot encode the categorical features
final_cols <- c("target", categorical_cols, numeric_cols)

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
  stri_trans_general(model_cols,"Latin-ASCII")
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
ddev <- xgb.DMatrix(data = dev_dmatrix, label = target_dev_dmatrix)

gc()
rm(target_dev_dmatrix, target_train_dmatrix)
watchlist <- list(train = dtrain, test = ddev)
# # set random seed for reproducibility
set.seed(1104)

cores <- parallel::detectCores() - 1
##### training xgboost model ####


# xgboost maximizing AUC of ROC 

print("Training xgboost model using ROC curve")

xgb.parameters <- list(booster = "gbtree",
                       objective = "binary:logistic",
                       eval_metric = "auc",
                       early_stoping_round = 30,
                       nrounds = 500)

model_xgb_auc <- xgb.train(
  data = dtrain,
  nround = xgb.parameters$nrounds,
  params = xgb.parameters,
  early_stopping_rounds = xgb.parameters$early_stoping_round,
  verbose = 1 ,
  nthread = cores, 
  watchlist = watchlist)

xgb.save(model_xgb_auc, os.path.join(models_folder, "xgb_auc.model"))

data_id <- data.table(var = c("model_type",
                              "test_month"),
                      value = c(model_type_modeling,
                                test_month)
)

fwrite(data_id, os.path.join(model_alias_modeling, "model_id.csv"))

save(
  final_cols, 
  model_cols,
  master_dmatrix,
  dev_dmatrix,
  test_dmatrix,
  master,
  dev,
  test,
  file = os.path.join(model_alias_modeling, paste0(products[i], "_xgb_objects.RData"))
)



##### data preparation for other models ####

# convert to h2o object
class(master$target)
master_h2o <-
  model.matrix(target ~ . - 1, data = master[, mget(final_cols)])

dev_h2o <-
  model.matrix(target ~ . - 1, data = dev[, mget(final_cols)])

test_h2o <-
  model.matrix(target ~ . - 1, data = test[, mget(final_cols)])

master_h2o <- data.table(target = master$target, master_h2o)
dev_h2o <- data.table(data.table(target = dev$target, dev_h2o))
test_h2o <- data.table(data.table(target = test$target, test_h2o))

names(master_h2o) <- tolower(names(master_h2o))
names(master_h2o) <- 
  stri_trans_general(names(master_h2o),"Latin-ASCII")
names(master_h2o) <- tm::removePunctuation(names(master_h2o))
names(master_h2o) <- gsub(" ", "", names(master_h2o), fixed = TRUE)
names(dev_h2o) <- names(master_h2o)
names(test_h2o) <- names(master_h2o)

master_h2o <- as.h2o(master_h2o)
dev_h2o <- as.h2o(dev_h2o)
test_h2o <- as.h2o(test_h2o)
y <- "target"
x <- setdiff(names(master_h2o), y) 

##### Training h2o regression  ####

models_folder_aux <- os.path.join(models_folder, "glm")
dir.create(models_folder_aux)

print("Trainig h2o models")
# elastic net model 
glm <- h2o.glm(
  x = x, 
  y = y, 
  training_frame = master_h2o,
  validation_frame = dev_h2o,
  family = "binomial",
  seed = 123
)

h2o.saveModel(object = glm,
              path = models_folder_aux,
              force = TRUE)

# random forest model
models_folder_aux <- os.path.join(models_folder, "rfm")
dir.create(models_folder_aux)

rf <- h2o.randomForest(
  x = x, 
  y = y,
  training_frame = master_h2o,
  validation_frame = dev_h2o,
  ntrees = 500,
  stopping_metric = "AUC",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123
)

h2o.saveModel(object = rf,
              path = models_folder_aux,
              force = TRUE)
# # gradient boosting machine model
models_folder_aux <- os.path.join(models_folder, "gbm")
dir.create(models_folder_aux)

gbm <-  h2o.gbm(
  x = x,
  y = y,
  training_frame = master_h2o,
  validation_frame = dev_h2o,
  ntrees = 1000,
  stopping_metric = "AUC",
  stopping_rounds = 10,
  stopping_tolerance = 0.005,
  seed = 123
)

h2o.saveModel(object = gbm,
              path = models_folder_aux,
              force = TRUE)


##### Predicciones ####
xgb_auc_preds_master <-  predict(model_xgb_auc, master_dmatrix)
xgb_auc_preds_dev <-  predict(model_xgb_auc, dev_dmatrix)
xgb_auc_preds_test <-  predict(model_xgb_auc, test_dmatrix)

glm_preds_master <- h2o.predict(glm, master_h2o) %>% as.data.frame()
glm_preds_dev <- h2o.predict(glm, dev_h2o) %>% as.data.frame()
glm_preds_test <- h2o.predict(glm, test_h2o) %>% as.data.frame()

rf_preds_master <- h2o.predict(rf, master_h2o) %>% as.data.frame()
rf_preds_dev <- h2o.predict(rf, dev_h2o) %>% as.data.frame()
rf_preds_test <- h2o.predict(rf, test_h2o) %>% as.data.frame()

gbm_preds_master <- h2o.predict(gbm, master_h2o) %>% as.data.frame()
gbm_preds_dev <- h2o.predict(gbm, dev_h2o) %>% as.data.frame()
gbm_preds_test <- h2o.predict(gbm, test_h2o) %>% as.data.frame()

# exportar probabilidades 

master[,':=' (xgb_auc_pred = xgb_auc_preds_master,
              glm_pred = glm_preds_master$p1,
              rf_pred = rf_preds_master$p1,
              gbm_pred = gbm_preds_master$p1
)
]

master <- calculate_pred(master, "target", "xgb_auc_pred")
master <- calculate_pred(master, "target", "glm_pred")
master <- calculate_pred(master, "target", "gbm_pred")
master <- calculate_pred(master, "target", "rf_pred")


dev[,':=' (xgb_auc_pred = xgb_auc_preds_dev,
           glm_pred = glm_preds_dev$p1,
           rf_pred = rf_preds_dev$p1,
           gbm_pred = gbm_preds_dev$p1
)
]

dev <- calculate_pred(dev, "target", "xgb_auc_pred")
dev <- calculate_pred(dev, "target", "glm_pred")
dev <- calculate_pred(dev, "target", "gbm_pred")
dev <- calculate_pred(dev, "target", "rf_pred")

test[,':=' (xgb_auc_pred = xgb_auc_preds_test,
           glm_pred = glm_preds_test$p1,
           rf_pred = rf_preds_test$p1,
           gbm_pred = gbm_preds_test$p1
)
]

test <- calculate_pred(test, "target", "xgb_auc_pred")
test <- calculate_pred(test, "target", "glm_pred")
test <- calculate_pred(test, "target", "gbm_pred")
test <- calculate_pred(test, "target", "rf_pred")


fwrite(master[, .(definit,
                  periodo,
                  target,
                  xgb_auc_pred,
                  glm_pred,
                  rf_pred,
                  gbm_pred,
                  xgb_auc_pred_num,
                  glm_pred_num,
                  rf_pred_num,
                  gbm_pred_num
                  )],
       os.path.join(model_alias_modeling, "pred_train.csv"))

fwrite(dev[, .(definit,
               periodo,
               target,
               xgb_auc_pred,
               glm_pred,
               rf_pred,
               gbm_pred,
               xgb_auc_pred_num,
               glm_pred_num,
               rf_pred_num,
               gbm_pred_num)],
       os.path.join(model_alias_modeling, "pred_dev.csv"))

fwrite(test[, .(definit,
                periodo,
                target,
                xgb_auc_pred,
                glm_pred,
                rf_pred,
                gbm_pred,
                xgb_auc_pred_num,
                glm_pred_num,
                rf_pred_num,
                gbm_pred_num)],
       os.path.join(model_alias_modeling, "pred_dev.csv"))

master[, target := as.numeric(target)]
master[, target := ifelse(target == 1, 0, 1)]

dev[, target := as.numeric(target)]
dev[, target := ifelse(target == 1, 0, 1)]

test[, target := as.numeric(target)]
test[, target := ifelse(target == 1, 0, 1)]
##### metrics xgb #####
metrics_xgb <-
  data.frame(train = c(
    accuracy(master$target, master$xgb_auc_pred_num),
    Metrics::precision(master$target, master$xgb_auc_pred_num),
    Metrics::recall(master$target, master$xgb_auc_pred_num),
    Metrics::auc(master$target, master$xgb_auc_pred)
  ),
  dev = c(
    accuracy(dev$target, dev$xgb_auc_pred_num),
    Metrics::precision(dev$target, dev$xgb_auc_pred_num),
    Metrics::recall(dev$target, dev$xgb_auc_pred_num),
    Metrics::auc(dev$target, dev$xgb_auc_pred_num)
  ),
  test = c(
    accuracy(test$target, test$xgb_auc_pred_num),
    Metrics::precision(test$target, test$xgb_auc_pred_num),
    Metrics::recall(test$target, test$xgb_auc_pred_num),
    Metrics::auc(test$target, test$xgb_auc_pred_num)
  )
  )
rownames(metrics_xgb) <- c("accuracy", "precision", "recall", "auc")
write.csv(metrics_xgb, os.path.join(model_alias_modeling, "metrics_xgb.csv"))

##### metrics gbm #####
metrics_gbm <-
  data.frame(train = c(
    accuracy(master$target, master$gbm_pred_num),
    Metrics::precision(master$target, master$gbm_pred_num),
    Metrics::recall(master$target, master$gbm_pred_num),
    Metrics::auc(master$target, master$gbm_pred)
  ),
  dev = c(
    accuracy(dev$target, dev$gbm_pred_num),
    Metrics::precision(dev$target, dev$gbm_pred_num),
    Metrics::recall(dev$target, dev$gbm_pred_num),
    Metrics::auc(dev$target, dev$gbm_pred)
  ),
  test = c(
    accuracy(test$target, test$gbm_pred_num),
    Metrics::precision(test$target, test$gbm_pred_num),
    Metrics::recall(test$target, test$gbm_pred_num),
    Metrics::auc(test$target, test$gbm_pred)
  )
  )
rownames(metrics_gbm) <- c("accuracy", "precision", "recall", "auc")
write.csv(metrics_gbm, os.path.join(model_alias_modeling, "metrics_gbm.csv"))

##### metrics glm #####
metrics_glm <-
  data.frame(train = c(
    accuracy(master$target, master$glm_pred_num),
    Metrics::precision(master$target, master$glm_pred_num),
    Metrics::recall(master$target, master$glm_pred_num),
    Metrics::auc(master$target, master$glm_pred)
  ),
  dev = c(
    accuracy(dev$target, dev$glm_pred_num),
    Metrics::precision(dev$target, dev$glm_pred_num),
    Metrics::recall(dev$target, dev$glm_pred_num),
    Metrics::auc(dev$target, dev$glm_pred)
  ),
  test = c(
    accuracy(test$target, test$glm_pred_num),
    Metrics::precision(test$target, test$glm_pred_num),
    Metrics::recall(test$target, test$glm_pred_num),
    Metrics::auc(test$target, test$glm_pred)
  )
  )
rownames(metrics_glm) <- c("accuracy", "precision", "recall", "auc")
write.csv(metrics_glm, os.path.join(model_alias_modeling, "metrics_glm.csv"))


##### metrics rf ####
metrics_rf <-
    data.frame(train = c(
      accuracy(master$target, master$rf_pred_num),
      Metrics::precision(master$target, master$rf_pred_num),
      Metrics::recall(master$target, master$rf_pred_num),
      Metrics::auc(master$target, master$rf_pred)
    ),
    dev = c(
      accuracy(dev$target, dev$rf_pred_num),
      Metrics::precision(dev$target, dev$rf_pred_num),
      Metrics::recall(dev$target, dev$rf_pred_num),
      Metrics::auc(dev$target, dev$rf_pred)
    ),
    test = c(
      accuracy(test$target, test$rf_pred_num),
      Metrics::precision(test$target, test$rf_pred_num),
      Metrics::recall(test$target, test$rf_pred_num),
      Metrics::auc(test$target, test$rf_pred)
    )
    )
  rownames(metrics_rf) <- c("accuracy", "precision", "recall", "auc")
  write.csv(metrics_rf, os.path.join(model_alias_modeling, "metrics_rf.csv"))

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
  
  uplift(
    true = test$target,
    prob = test$glm_pred,
    filepath = os.path.join(
      model_alias_modeling,
      paste0("glm", "_test_uplift.csv")
    ),
    primerCorte = 0.1,
    salto = 0.1
  )
  
  uplift(
    true = test$target,
    prob = test$rf_pred,
    filepath = os.path.join(
      model_alias_modeling,
      paste0("rf", "_test_uplift.csv")
    ),
    primerCorte = 0.1,
    salto = 0.1
  )
  
  uplift(
    true = test$target,
    prob = test$gbm_pred,
    filepath = os.path.join(
      model_alias_modeling,
      paste0("gbm", "_test_uplift.csv")
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

variable_importance <- h2o.varimp(rf)
variable_importance <- data.table(variable_importance)
fwrite(variable_importance,
       os.path.join(
         model_alias_modeling,"rf_important_variables.csv"
       ))

variable_importance <- h2o.varimp(glm)
variable_importance <- data.table(variable_importance)
fwrite(variable_importance,
       os.path.join(
         model_alias_modeling,"glm_important_variables.csv"
       ))
variable_importance <- h2o.varimp(gbm)
variable_importance <- data.table(variable_importance)
fwrite(variable_importance,
       os.path.join(
         model_alias_modeling,"gbm_important_variables.csv"
       ))

# create custom predict function
pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[2L]])
}
# elastic net explainer
explainer_glm <- explain(
  model = glm,
  data = as.data.frame(dev_h2o)[, -1],
  y = as.vector(as.numeric(dev$target)),
  predict_function = pred,
  label = "h2o glm"
)

# random forest explainer
explainer_rf <- explain(
  model = rf,
  data = as.data.frame(dev_h2o)[, -1],
  y = as.vector(as.numeric(dev$target)),
  predict_function = pred,
  label = "h2o rf"
)

# GBM explainer
explainer_gbm <- explain(
  model = gbm,
  data = as.data.frame(dev_h2o)[, -1],
  y = as.vector(as.numeric(dev$target)),
  predict_function = pred,
  label = "h2o gbm"
)

explainer_xgb_auc <- explain(
  model_xgb_auc,
  data = dev_dmatrix,
  y = dev$target == 1,
  label = "xgboost auc"
)

##### residual analysis ####
resids_glm <- model_performance(explainer_glm)
resids_rf  <- model_performance(explainer_rf)
resids_gbm <- model_performance(explainer_gbm)
resids_xgb_auc <- model_performance(explainer_xgb_auc)

# create comparison plot of residuals for each model
p1 <- plot(resids_glm, resids_rf, resids_gbm, resids_xgb_auc)
p2 <- plot(resids_glm, resids_rf, resids_gbm, resids_xgb_auc, geom = "boxplot")

plot <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave(file = os.path.join(plots_folder,  "resids.png"),
       plot = plot)

pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}
# elastic net explainer
explainer_glm <- explain(
  model = glm,
  data = as.data.frame(dev_h2o)[, -1],
  y = as.vector(as.numeric(dev$target)),
  predict_function = pred,
  label = "h2o glm"
)

# random forest explainer
explainer_rf <- explain(
  model = rf,
  data = as.data.frame(dev_h2o)[, -1],
  y = as.vector(as.numeric(dev$target)),
  predict_function = pred,
  label = "h2o rf"
)

# GBM explainer
explainer_gbm <- explain(
  model = gbm,
  data = as.data.frame(dev_h2o)[, -1],
  y = as.vector(as.numeric(dev$target)),
  predict_function = pred,
  label = "h2o gbm"
)


# vip_rf  <-
#   variable_importance(explainer_rf, loss_function = loss_root_mean_square)
# vip_gbm  <-
#   variable_importance(explainer_gbm, loss_function = loss_root_mean_square)
# vip_glm  <-
#   variable_importance(explainer_glm, loss_function = loss_root_mean_square)
# vip_xgb_auc <-
#   variable_importance(explainer_xgb_auc,
#                       loss_function = loss_root_mean_square)

##### variable importance plots #####

p1 <- plot(vip_rf,max_vars = 20, show_baseline = T)
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_vip_rf.png")), plot = p1)

p1 <- plot(vip_glm,max_vars = 20, show_baseline = T)
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_vip_glm.png")), plot = p1)

p1 <- plot(vip_rf,max_vars = 20, show_baseline = T)
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_vip_gbm.png")), plot = p1)


p1 <- plot(vip_xgb_auc,max_vars = 20, show_baseline = T)
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_vip_xgboost_auc.png")), plot = p1)

# Gain
p1 <- xgb.ggplot.importance(importance_matrix, measure = "Gain", rel_to_first = FALSE, top_n = 20)
p1 <- p1 + ggplot2::ylab("Gain")
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_gain_xgboost_auc.png")), plot = p1)

# Cover
p1 <- xgb.ggplot.importance(importance_matrix, measure = "Cover", rel_to_first = FALSE, top_n = 20)
p1 <- p1 + ggplot2::ylab("Cover")
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_cover_xgboost_auc.png")), plot = p1)

# Frequency
p1 <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = FALSE, top_n = 20)
p1 <- p1 + ggplot2::ylab("Frequency")
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_frequency_xgboost_auc.png")), plot = p1)

rm(p1)

ppi = 300
png(
  os.path.join(plots_folder, paste0(products[i], "_rf_variable_importance.png")),
  width = 7 * ppi,
  height = 7 * ppi,
  res = ppi
)
h2o.varimp_plot(rf, num_of_features = 20)
dev.off()

ppi = 300
png(
  os.path.join(plots_folder, paste0(products[i], "_glm_variable_importance.png")),
  width = 7 * ppi,
  height = 7 * ppi,
  res = ppi
)
h2o.varimp_plot(glm, num_of_features = 20)
dev.off()

ppi = 300
png(
  os.path.join(plots_folder, paste0(products[i], "_gbm_variable_importance.png")),
  width = 7 * ppi,
  height = 7 * ppi,
  res = ppi
)
h2o.varimp_plot(gbm, num_of_features = 20)
dev.off()

##### curvas AUC ####

##### AUC Validation ####
glm_roc <- pROC::roc(response = dev$target, predictor = dev$glm_pred)
rf_roc <-
  pROC::roc(response = dev$target, predictor = dev$rf_pred)
gbm_roc <-
  pROC::roc(response = dev$target, predictor = dev$gbm_pred)
xgb_auc_roc <-
  pROC::roc(response = dev$target,
            predictor = dev$xgb_auc_pred)

rocs <- list(
  "Logit" = glm_roc,
  "Bosques aleatorios" = rf_roc,
  "GBM" = gbm_roc,
  "XGB-AUC" = xgb_auc_roc
)
breaks = seq(0, 1, 0.1)
legendTitel = "Modelos"
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
  labs(title = paste("Curvas ROC y AUC en validación para", products[i]))+
  guides(colour = guide_legend(legendTitel)) +
  theme(axis.ticks = element_line(color = "grey80"))

p1 <- gridExtra::grid.arrange(
  rocPlot,
  matrix,
  nrow = 2,
  as.table = TRUE,
  heights = c(3, 1)
)

ggsave(file = os.path.join(model_alias_modeling, paste0("validacion", "_auc_all.png")),
       plot = p1)

##### AUC test ####

glm_roc <- pROC::roc(response = test$target, predictor = test$glm_pred)
rf_roc <-
  pROC::roc(response = test$target, predictor = test$rf_pred)
gbm_roc <-
  pROC::roc(response = test$target, predictor = test$gbm_pred)
xgb_auc_roc <-
  pROC::roc(response = test$target,
            predictor = test$xgb_auc_pred)

rocs <- list(
  "Logit" = glm_roc,
  "Bosques aleatorios" = rf_roc,
  "GBM" = gbm_roc,
  "XGB-AUC" = xgb_auc_roc
)
breaks = seq(0, 1, 0.1)
legendTitel = "Modelos"
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
  labs(title = paste("Curvas ROC y AUC en pruebas para", products[i]))+
  guides(colour = guide_legend(legendTitel)) +
  theme(axis.ticks = element_line(color = "grey80"))

p1 <- gridExtra::grid.arrange(
  rocPlot,
  matrix,
  nrow = 2,
  as.table = TRUE,
  heights = c(3, 1)
)

ggsave(file = os.path.join(model_alias_modeling, paste0("test", "_auc_all.png")),
       plot = p1)
