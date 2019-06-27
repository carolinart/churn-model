#' Interpretacion de modelos
#' Una vez entrenado el modelo, dada su estructura compleja, para saber como aportan las variables a la prediccion
#' se hace una serie de graficas que ayuden a este fin. 
#' 
#' @param model_interp_alias contiene la ruta donde van a quedar guardadas las graficas
#' @param master_path aloja los datos de las tablas master mensuales
#' @param train_months meses de entrenamiento del modelo
#' @param dev_months meses de desarrollo del modelo
#' @param test_months meses de test del modelo
#' @usage interpretation_maker(model_interp_alias, master_path, train_months, dev_months, test_months)


interpretation_maker <- function(model_interp_alias,
                         master_path,
                         train_months,
                         dev_months,
                         test_months) {
  
print("Creating plots path")
plot_path <- "documentos/plots/model_interpret"
# model product folder
model_interp_alias <- os.path.join(plot_path, model_alias)
dir.create(model_interp_alias)


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
final_cols <- c("target", numeric_cols)

master_f <- master[, mget(final_cols)]


model_matrix_train <- model.matrix(target ~ . - 1, master_f)
data_train <- xgb.DMatrix(model_matrix_train, label = master_f$target)


dev_f <- dev[, mget(final_cols)]
model_matrix_train <- model.matrix(target ~ . - 1, dev_f)
data_dev <- xgb.DMatrix(model_matrix_train, label = dev_f$target)

watchlist <- list(train = data_train, test = data_dev)

cores <- parallel::detectCores() - 1

param <- list(booster = "gbtree",
              objective = "binary:logistic",
              eval_metric = "auc",
              early_stoping_round = 5,
              nrounds = 200)

print("Running model")
churn_xgb_model <- xgb.train(data = data_train,
                             nround = param$nrounds,
                             params = param,
                             early_stopping_rounds = param$early_stoping_round,
                             verbose = 1 ,
                             nthread = cores,
                             watchlist = watchlist,
                             max_depth = 4
)


###########GRAPHS###############
#variable importance
name <- "var_imp"
p <- vip(churn_xgb_model, num_features = 12)
ggsave(p, file = os.path.join(model_interp_alias, paste0(name, ".png")))


#Crear un "explainer"
print("Creating explainer for creation of PDP's")
predict_logit <- function(model, x){
  raw_x <- predict(model, x)
  exp(raw_x)/(1+exp(raw_x))
}

logit <- function(x) exp(x)/(1+exp(x))
explainer <- explain(churn_xgb_model, 
                     data = model_matrix_train, 
                     y = master_f$target, 
                     predict_function = predict_logit,
                     label = "xgboost", 
                     link = logit)


#Analisis variable individual (single variable)
#cartera capital total
name <- "pdp_cartera"
sv_xgb <- variable_response(explainer,
                                  variable = "cartera_capital_total",
                                  type = "pdp")
p <- plot(sv_xgb)
ggsave(p, file = os.path.join(model_interp_alias, paste0(name, ".png")))


#saldo total
name <- "pdp_sdo"
sv_xgb <- variable_response(explainer,
                            variable = "sdo_total",
                            type = "pdp")
p <- plot(sv_xgb)
ggsave(p, file = os.path.join(model_interp_alias, paste0(name, ".png")))



#antiguedad
name <- "pdp_antig"
sv_xgb <- variable_response(explainer,
                                  variable = "antiguedad",
                                  type = "pdp")
p <- plot(sv_xgb)
ggsave(p, file = os.path.join(model_interp_alias, paste0(name, ".png")))



# #Variable importance (basado en dropout loss)
# name <- "vi_dropout"
# vd_xgb <- variable_importance(explainer, type = "raw")
# head(vd_xgb)
# p <- plot(vd_xgb)
# ggsave(p, file = os.path.join(model_interp_alias, paste0(name, ".png")))

print(paste("Graphs generated and saved in", model_interp_alias))

}
