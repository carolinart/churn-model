#' Interpretacion de modelos
#' Una vez entrenado el modelo, dada su estructura compleja, para saber como aportan las variables a la prediccion
#' se hace una serie de graficas que ayuden a este fin. 

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
    util,
    mes_venci,
    dias_ult_pago,
    crm_valor_egreso_mes,
    crm_edad,
    crm_antiguedad,
    cant_total_prod,
    sdo_capt_total_tmenos1,
    util_tmenos1,
    dias_ult_compra_tmenos1,
    cartera_capital_total
  )]
#15

#partition of data
dev <-
  datos[periodo ==  dev_months[1]]

master <- datos[periodo >= train_months[1] &
                  periodo <= train_months[2]]

test <- datos[periodo == test_months]

#Separating variables
id_variables <-
  c("cod_int", "nro_id" , "periodo", "target")

numeric_cols <-
  names(master)[names(master) %!in% c(id_variables)]


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
  early_stoping_round = 5,
  nrounds = 200
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

####Explainer####
#Crear un "explainer"
predict_logit <- function(model, x){
  raw_x <- predict(model, x)
  exp(raw_x)/(1+exp(raw_x))
}

logit <- function(x) exp(x)/(1+exp(x))

explainer_xgb <- explain(model_xgb_auc,
                         data = master_dmatrix,
                         y = master$target,
                         predict_function = predict_logit,
                         link = logit,
                         label = "xgboost"
                         )
explainer_xgb


#