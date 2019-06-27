## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="CHURN_TC> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("Scripts/utiles.R")
set_environment()

library(Matrix)
library(rsample)
library(breakDown)
library(cowplot)
library(scales)
library(MASS)
library(stringi)


# library(randomForest)
# h2o.no_progress()
h2o.init()

master_path <- "data/master"
models_path <- "modelos"

train_months <- c("2018-07-01", "2018-10-01")
dev_month <- c("2018-11-01", "2018-12-01")
test_month <- c("2019-01-01")


# set random seed for reproducibility
set.seed(5)

##### model meta datos ####
print("Making model's fold")
gc()
model_alias_modeling <-
  paste0("churn_v1_", today() %>% format(., "%Y%m%d"))
# model product folder
model_alias_modeling <-
  os.path.join(models_path, model_alias_modeling)
dir.create(model_alias_modeling)

# models folder
models_folder <-
  os.path.join(model_alias_modeling, "models_library")
dir.create(models_folder)

# plots folder 
plots_folder <- os.path.join(model_alias_modeling, "plots")
dir.create(plots_folder)

##### load datos ####
master_files <- list.files(master_path, full.names = T)
datos_list <- list()
for(i in 1:length(master_files)){
  print(paste("Loading datos from", master_files[i]))
  datos_list[[i]] <- readRDS(master_files[i])
}

datos <- rbindlist(datos_list, use.names = T, fill = T)
rm(datos_list)
gc()



###### Ultima limpieza previa######

print(nrow(datos))
datos <- datos[!is.na(crm_antiguedad)]
datos <- datos[!is.na(antiguedad)]
print(nrow(datos))


# datos[, crm_estrato := factor(crm_estrato, levels = c("1","2","3","4","5","6"))]

datos[, crm_estrato := paste0("estr_", crm_estrato)]
datos[, crm_estrato := order(crm_estrato, levels = c(paste0("estr_", 1:6)))]

datos[, target := ifelse(churn_voluntario_futuro == "STOCK", 0, 1)]
datos[, churn_voluntario_futuro := NULL]

datos[, ciclo := NULL]
data <- copy(datos)
datos <- copy(data)

#Sample
index <- sample(1:nrow(datos), .N*(0.2))
datos <- datos[index]
rm(index)
gc()

#Verificación de NA's
gg_miss_fct(datos, periodo)


# divinding master table
dev <- datos[periodo >=  dev_month[1] & periodo <=  dev_month[2]]
master <- datos[periodo >= train_months[1] &
                 periodo <= train_months[2] ]
test <- datos[periodo == test_month]

cores <- parallel::detectCores() - 1

##### Plots maker ####
datos_plot <- copy(datos)
datos_plot[, target := factor(target)]
print(" making products plots")
name_f <- "per_target"
p <- datos_plot %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::mutate(percent = prop.table(Count) * 100) %>%
  ggplot(aes(reorder(target,-percent), percent), fill = target) +
  geom_col(fill = c("#FC4E07", "#E7B800")) +
  geom_text(
    aes(label = sprintf("%.2f%%", percent)),
    hjust = 0.01,
    vjust = -0.5,
    size = 3
  ) +
  theme_bw() +
  xlab(" Tarjetas canceladas voluntariamente") +
  ylab("Porcentaje") +
  ggtitle(paste("Porcentaje agregado de tarjeta de crédito  \n con cancelación voluntaria a dos meses"))
ggsave(plot = p,
       file = os.path.join(plots_folder, paste0(name_f, ".png")))


name_f <- "per_target_mes"
plot_base <- datos_plot %>% 
  dplyr::group_by(periodo, target) %>% 
  dplyr::summarise(Count = n())%>% 
  dplyr::mutate(percent = prop.table(Count)*100) %>% data.table()

p <-
  ggplot(plot_base[target == 1], aes(periodo, percent), fill = periodo) +
  geom_col(fill = "#FC4E07") +
  geom_text(
    aes(label = sprintf("%.2f%%", percent)),
    hjust = 0.5,
    vjust = 1,
    size = 3,
    angle = -45
  ) +
  
  theme_bw() +
  xlab("Meses") +
  ylab("Porcentaje") +
  ggtitle("Porcentaje agregado de tarjeta de crédito  \n con cancelación voluntaria a dos meses por mes")
ggsave(plot = p,
       file = os.path.join(plots_folder, paste0( name_f, ".png")))


name_f <- "sexo"
x_lab <- "Sexo"
p <- datos_plot %>%
  dplyr::group_by(crm_genero, target) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::mutate(percent = prop.table(Count) * 100) %>% data.table() 
p <- p[target == 1] %>% 
  ggplot(aes(x = crm_genero, y = percent, fill = target)) +
  geom_bar(stat = "identity") + geom_text(aes(
    x = crm_genero,
    y = percent,
    label = paste0(round(percent, 2), "%")
  ), size = 4) + theme_bw() +
  xlab(x_lab) +
  ylab("Porcentaje") +
  ggtitle(paste("Porcentaje de tarjetas canceladas voluntariamente  por", tolower(x_lab)))

ggsave(plot = p,
       file = os.path.join(plots_folder, paste0(name_f, ".png")))

name_f <- "cuo_mano"
y_lab <- "Cuota de manejo"

p <-
  ggplot(datos_plot, aes(y = cuomano, x = "", fill = target)) +
  geom_boxplot() +
  theme_bw() +
  xlab(" ") +
  ylab(y_lab) +
  scale_y_continuous(labels = comma, limits = quantile(datos_plot[, get("cuomano")], c(0.1, 0.9))) +
  ggtitle(paste("Porcentaje de tarjetas canceladas voluntariamente por", tolower(y_lab)))

ggsave(plot = p,
       file = os.path.join(plots_folder, paste0(name_f, ".png")))

rm(datos_plot)
gc()

##### model fase #####
### separating variabbles ###

id_variables <-
  c("cod_int", "nro_id" , "periodo", "target")

crm_vars <-
  names(master)[names(master) %!in% c(id_variables)]
crm_vars <- grep("crm_", crm_vars, value = T)

tc_vars <- names(master)[names(master) %!in% c(id_variables, crm_vars)]

categorical_cols <-
  c(crm_vars[sapply(master[, mget(crm_vars)], is.factor)],
    tc_vars[sapply(master[, mget(tc_vars)], is.factor)])

numeric_cols <- names(master)[names(master) %!in% c(id_variables, categorical_cols)]

print("One hot encoding")
# one-hot encode the categorical features
final_cols <- c("target",categorical_cols, numeric_cols)

#### making xgb data ####
master_dmatrix <- master[, mget(final_cols)]
master_dmatrix <-
  sparse.model.matrix(target ~ . - 1, data = master_dmatrix)

dev_dmatrix <- dev[, mget(final_cols)]
dev_dmatrix <- 
  sparse.model.matrix(target ~ . - 1, data = dev_dmatrix)

model_cols <- master_dmatrix@Dimnames[[2]]

model_cols <- tolower(model_cols)
model_cols <- 
  stri_trans_general(model_cols,"Latin-ASCII")
model_cols <- tm::removePunctuation(model_cols)
model_cols <- gsub(" ", "", model_cols, fixed = TRUE)

master_dmatrix@Dimnames[[2]] <- model_cols
dev_dmatrix@Dimnames[[2]] <- model_cols

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

##### training xgboost model ####
# there are two ways of trainging xgboost models for a binary clasification
# problems: using AUC of precision-recall curve or AUC of ROC. The firsth is
# more usefull for unbalance classes as capture a the clients whose getting
# financial product month through month.


#### xgboost maximizing Precision and recall AUC ####
print("Training xgboost model using precision-recall curve")
xgb.parameters <- list(booster = "gbtree",
                       objective = "binary:logistic",
                       eval_metric = "aucpr",
                       early_stoping_round = 30,
                       nrounds = 500)

model_xgb_pr <- xgb.train(
  data = dtrain,
  nround = xgb.parameters$nrounds,
  params = xgb.parameters,
  early_stopping_rounds = xgb.parameters$early_stoping_round,
  verbose = 1 ,
  nthread = cores, 
  watchlist = watchlist
  
)
xgb.save(model_xgb_pr, os.path.join(models_folder, "xgb_pr.model"))

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

data_id <- data.table(var = c("model_alias",
                              "model_type",
                              "train_months_since",
                              "train_months_to",
                              "dev_month",
                              "test_month"),
                      value = c(model_alias_modeling,
                                model_type_modeling,
                                train_months[1],
                                train_months[2],
                                dev_month,
                                test_month)
)

fwrite(data_id, os.path.join(model_alias_modeling, "model_id.csv"))

save(
  final_cols, 
  model_cols,
  master_dmatrix,
  dev_dmatrix,
  master,
  dev,
  file = os.path.join(model_alias_modeling, paste0(products[i], "_xgb_objects.RData"))
)

##### data preparation for other models ####

#### convert to h2o object ####
# class(master$target)
# 
# master_h2o <-
#   model.matrix(target ~ . - 1, data = master[, mget(final_cols)])
# 
# dev_h2o <-
#   model.matrix(target ~ . - 1, data = dev[, mget(final_cols)])
# 
# master_h2o <- data.table(target = master$target, master_h2o)
# dev_h2o <- data.table(data.table(target = dev$target, dev_h2o))
# 
# names(master_h2o) <- tolower(names(master_h2o))
# names(master_h2o) <- 
#   stri_trans_general(names(master_h2o),"Latin-ASCII")
# names(master_h2o) <- tm::removePunctuation(names(master_h2o))
# names(master_h2o) <- gsub(" ", "", names(master_h2o), fixed = TRUE)
# names(dev_h2o) <- names(master_h2o)


master[, target := as.factor(target)]
master_h2o <- as.h2o(master)
dev_h2o <- as.h2o(dev)
test_h2o <- as.h2o(test)
y <- "target"
x <- setdiff(names(master_h2o), y) 

##### Training h2o regression  ####

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

h2o.saveModel(object = glm, path = models_folder, force = TRUE)
h2o.saveModelDetails(object = glm,
                     path = models_folder,
                     force = TRUE)

# random forest model
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

h2o.saveModel(object = rf, path = models_folder, force = TRUE)
h2o.saveModelDetails(object = rf,
                     path = models_folder, force = TRUE)

# # gradient boosting machine model
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

h2o.saveModel(object = gbm, path = models_folder, force = TRUE)
h2o.saveModelDetails(object = gbm,
                     path = models_folder, force = TRUE)



# model performance
h2o.auc(glm, valid = TRUE)
#[1] 0.6913626
h2o.auc(rf, valid = TRUE)
#[1] 0.6550603
h2o.auc(gbm, valid = TRUE)
#[1] 0.7076738

source("Scripts/performance.R")

pgorro <- predict(glm, type = "response", newdata = test_h2o)
ROC(pgorro = pgorro, master_test = test) 
Grafica_uplift(pgorro = pgorro, master_test = test_h2o)
Grafica_recall(pgorro = pgorro, master_test = test_h2o)
Grafica_precision(pgorro = pgorro, master_test = test_h2o)




##### Predicciones ####
# xgb_auc_preds_master <-  predict(model_xgb_auc, master_dmatrix)
# xgb_auc_preds_dev <-  predict(model_xgb_auc, dev_dmatrix)
# 
# xgb_pr_preds_master <-  predict(model_xgb_pr, master_dmatrix)
# xgb_pr_preds_dev <-  predict(model_xgb_pr, dev_dmatrix)

glm_preds_master <- h2o.predict(glm, master_h2o) %>% as.data.frame()
glm_preds_dev <- h2o.predict(glm, dev_h2o) %>% as.data.frame()

rf_preds_master <- h2o.predict(rf, master_h2o) %>% as.data.frame()
rf_preds_dev <- h2o.predict(rf, dev_h2o) %>% as.data.frame()

gbm_preds_master <- h2o.predict(gbm, master_h2o) %>% as.data.frame()
gbm_preds_dev <- h2o.predict(gbm, dev_h2o) %>% as.data.frame()


glm_performance_master <- h2o.performance(glm) 
glm_performance_dev <- h2o.performance(glm, dev_h2o)

rf_performance_master <- h2o.performance(rf)
rf_performance_dev <- h2o.performance(rf, dev_h2o)

gbm_performance_master <- h2o.performance(gbm)
gbm_performance_dev <- h2o.performance(gbm, dev_h2o)

save(
  glm_performance_master,
  rf_performance_master,
  gbm_performance_master,
  glm_performance_dev,
  rf_performance_dev,
  gbm_performance_dev,
  file = os.path.join(model_alias_modeling, paste0 ("_performance.RData"))
)
rm(
  glm_performance_master,
  rf_performance_master,
  gbm_performance_master,
  glm_performance_dev,
  rf_performance_dev,
  gbm_performance_dev
)
gc()
# exportar probabilidades 
master[, ':=' (
  glm_pred = glm_preds_master$p1,
  rf_pred = rf_preds_master$p1,
  gbm_pred = gbm_preds_master$p1
  #xgb_auc_pred = xgb_auc_preds_master,
  #xgb_pr_pred = xgb_pr_preds_master
)
]

dev[, ':=' (
  glm_pred = glm_preds_master$p1,
  rf_pred = rf_preds_master$p1,
  gbm_pred = gbm_preds_master$p1
  #xgb_auc_pred = xgb_auc_preds_master,
  #xgb_pr_pred = xgb_pr_preds_master
)]

fwrite(master[, .(llave, periodo, target, xgb_auc_pred, xgb_pr_pred,
                  glm_pred, rf_pred, gbm_pred)],
       os.path.join(model_alias_modeling, "pred_train.csv"))
fwrite(dev[, .(llave, periodo, target, xgb_auc_pred, xgb_pr_pred,
               glm_pred, rf_pred, gbm_pred)],
       os.path.join(model_alias_modeling, "pred_dev.csv"))

rm(
  glm_preds_master,
  rf_preds_master,
  gbm_preds_master,
  xgb_auc_preds_master,
  xgb_pr_preds_master,
  glm_preds_dev,
  gbm_preds_dev,
  rf_preds_dev,
  xgb_auc_preds_dev,
  xgb_pr_preds_dev
  )

gc()
# Important variables 
# importance_matrix <-
#   xgb.importance(feature_names =  model_cols, model = model_xgb_auc)
# fwrite(importance_matrix,
#        os.path.join(
#          model_alias_modeling,"xgb_auc_important_variables.csv"
#        ))

# importance_matrix <-
#   xgb.importance(feature_names =  model_cols, model = model_xgb_pr)
# fwrite(importance_matrix,
#        os.path.join(
#          model_alias_modeling,"xgb_pr_important_variables.csv"
#        ))


variable_importance_rf <- h2o.varimp(rf)
variable_importance_rf <- data.table(variable_importance_rf)
fwrite(variable_importance_rf,
       os.path.join(
         model_alias_modeling,"rf_important_variables.csv"
       ))

# build explainers on dev data


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
explainer_xgb_pr <- explain(
  model_xgb_pr,
  data = dev_dmatrix,
  y = dev$target == 1,
  label = "xgboost pr"
)

save(
  explainer_glm,
  explainer_rf,
  explainer_gbm,
  explainer_xgb_auc,
  explainer_xgb_pr,
  file = os.path.join(model_alias_modeling, paste0(products[i], "_explainers.RData"))
)

rm(
  master,
  master_dmatrix,
  master_h2o
)
gc()
##### residual analysis ####
resids_glm <- model_performance(explainer_glm)
resids_rf  <- model_performance(explainer_rf)
resids_gbm <- model_performance(explainer_gbm)
resids_xgb_auc <- model_performance(explainer_xgb_auc)
resids_xgb_pr <- model_performance(explainer_xgb_pr)

save(
  resids_glm,
  resids_rf,
  resids_gbm,
  resids_xgb_auc,
  resids_xgb_pr,
  file = os.path.join(model_alias_modeling, paste0(products[i], "_resids.RData"))
)
rm(
  resids_glm,
  resids_rf,
  resids_gbm,
  resids_xgb_auc,
  resids_xgb_pr
)
rm(
  explainer_glm,
  explainer_rf,
  explainer_gbm,
  explainer_xgb_auc,
  explainer_xgb_pr
)
gc()
## remake explainers using p1 probability 
# create custom predict function
pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}
# elastic net explainer
# random forest explainer
explainer_rf <- explain(
  model = rf,
  data = as.data.frame(dev_h2o)[, -1],
  y = as.vector(as.numeric(dev$target)),
  predict_function = pred,
  label = "h2o rf"
)

explainer_xgb_auc <- explain(
  model_xgb_auc,
  data = dev_dmatrix,
  y = dev$target == 1,
  label = "xgboost auc"
)

unique(datos$crm_estrato
       )



