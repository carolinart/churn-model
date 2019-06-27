
##MODELOS DE PRIMERA LINEA##

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
source("Scripts/performance.R")
set_environment()
library(pROC)
library(MASS)
library(ROCR)

master_path <- "data/master"
models_path <- "modelos"

train_months <- c("2018-06-01", "2018-10-01")
dev_month <- c("2018-11-01", "2018-12-01")
test_month <- c("2019-01-01")

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
datos <- datos[!is.na(crm_antiguedad)]
datos <- datos[!is.na(antiguedad)]

datos[, target := ifelse(churn_voluntario_futuro == "STOCK", 0, 1)]
datos[, churn_voluntario_futuro := NULL]

datos[, ciclo := NULL]
datos[, target := as.factor(target)]

# data <- copy(datos)
# datos <- copy(data)
set.seed(666)
#Sample
index <- sample(1:nrow(datos), nrow(datos)*(0.05))
datos <- datos[index]
rm(index)
gc()

#Cambio de nombre
data.table::setnames(datos, "target", "Y")

# divinding master table
dev <- datos[periodo >=  dev_month[1] & periodo <=  dev_month[2]]
master <- datos[periodo >= train_months[1] &
                  periodo <= train_months[2] ]
test <- datos[periodo == test_month]

cores <- parallel::detectCores() - 1


#############################################################################
##Modelamiento##
############################################################################

### separating variabbles ###

id_variables <-
  c("cod_int", "nro_id" , "periodo", "Y")

crm_vars <-
  names(master)[names(master) %!in% c(id_variables)]
crm_vars <- grep("crm_", crm_vars, value = T)

tc_vars <- names(master)[names(master) %!in% c(id_variables, crm_vars)]

categorical_cols <-
  c(crm_vars[sapply(master[, mget(crm_vars)], is.factor)],
    tc_vars[sapply(master[, mget(tc_vars)], is.factor)])

numeric_cols <- names(master)[names(master) %!in% c(id_variables, categorical_cols)]

# final_cols <- c("target",categorical_cols, numeric_cols)
vars <- c(categorical_cols, numeric_cols)


rm(datos)
gc()

###############################################################################
##Matriz de correlaciones para variables numéricas##
###############################################################################

corr_mat <- cor(master %>% dplyr::select( 
  numeric_cols
))
round(corr_mat, 2)


#####################################################################
##Seleccionar variables##
####################################################################

######################################
##Primer grupo de variables##
#####################################


##Primer modelo con todas las variables
logit_1 <- glm(Y ~ .,family=binomial(link = "logit"), data = master)
##No hay memoria suficiente

##Segundo modelo
logit_2 <-
  glm(
    Y ~ sub_producto +
      tipo_tarj +
      cuomano +
      antiguedad +
      cupo +
      cartera_capital_total +
      cartera_vigente +
      sdo_total +
      sdo_capt_total +
      pago_minimo +
      vlr_ult_compra +
      vlr_ult_av +
      vlr_ult_pago +
      util +
      ultimo_aumento +
      crm_estrato +
      crm_genero +
      crm_valor_activos +
      crm_valor_ing_bru_mes +
      crm_valor_egreso_mes +
      crm_valor_pasivos +
      crm_edad +
      crm_antiguedad +
      crm_segmento +
      crm_depto,
    family = binomial,
    data = master
  )

summary(logit_2)

#Calcular curva roc
resRoc <- roc(master$Y ~ logit_2$fitted)
plot(resRoc, legacy.axes = TRUE)
resRoc

rm(logit_2, resRoc)
gc()

#Calcular vector de probabilides predichas
pgorro <- predict(logit_2, type = "response", newdata = test)
ROC(pgorro = pgorro, master_test = test) 
Grafica_uplift(pgorro = pgorro, master_test = test)
Grafica_recall(pgorro = pgorro, master_test = test)
Grafica_precision(pgorro = pgorro, master_test = test)


#Modelo 3
logit_3<- MASS::stepAIC(logit_2, direction="both")

#Calcular curva roc
resRoc <- roc(master$Y ~ logit_3$fitted)
plot(resRoc, legacy.axes = TRUE)
resRoc

rm(logit_3, resRoc)
gc()

#Modelo 4 (CON MASTER)
logit_4 <-
  glm(
    Y ~ antiguedad +
      cupo +
      cartera_capital_total +
      sdo_total +
      vlr_ult_compra +
      vlr_ult_av +
      vlr_ult_pago +
      util +
      crm_valor_ing_bru_mes +
      crm_valor_pasivos +
      crm_edad +
      crm_antiguedad +
      crm_segmento,
    family = binomial,
    data = master
  )


#Calcular curva roc master
master_roc <- roc(master$Y ~ logit_4$fitted)
master_roc

#Calcular curva roc dev
dev_roc <- roc(dev$Y ~ logit_5$fitted)
dev_roc

#Calcular curva roc test
test_roc <- roc(test$Y ~ logit_6$fitted)
test_roc

rm(master_roc, dev_roc, test_roc)
gc()


#Modelos de RF

#Modelo 1

forest_1 <- randomForest(
  Y ~ antiguedad +
    cupo +
    cartera_capital_total +
    sdo_total +
    vlr_ult_compra +
    vlr_ult_av +
    vlr_ult_pago +
    util +
    crm_valor_ing_bru_mes +
    crm_valor_pasivos +
    crm_edad +
    crm_antiguedad +
    crm_segmento,
  data = master,
  proximity = FALSE,
  importance = TRUE,
  ntree = 500,
  mtry = 4,
  do.trace = FALSE
)


# View the forest results.
print(forest_1) 

testPred <- predict(forest_1, newdata=master[,-21])
table(testPred, master$Y)

#AUC para RF
predictions=as.vector(forest_1$votes[,2])
pred=prediction(predictions,master$Y)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))


#INFO SOBRE MODELOS RF
#http://scg.sdsu.edu/rf_r/


# bestmtry <- tuneRF(master[-1],master$Y, ntreeTry=100, 
#                    stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)


#Arbol con todas las variables

id_variables <-
  c("cod_int", "nro_id" , "periodo")

master[, (id_variables) := NULL]
dev[, (id_variables) := NULL]
test[, (id_variables) := NULL]


# Modelo 1 RF
model1.rf <-randomForest(Y~.,data=master, mtry=2, ntree=500, 
                        keep.forest=TRUE, importance=TRUE,test=dev)

#Graficas
model1.rf.pr = predict(model1.rf,type="prob",newdata=dev)[,2]
model1.rf.pred = prediction(model1.rf.pr, dev$Y)
model1.rf.perf = performance(model1.rf.pred,"tpr","fpr")
plot(model1.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#
importance(model1.rf)
varImpPlot(model1.rf)


# Modelo 2 RF
model2.rf <-randomForest(Y~.,data=master, mtry=2, ntree=500, 
                         keep.forest=TRUE, importance=TRUE,test=test)

#Graficas
model2.rf.pr = predict(model2.rf,type="prob",newdata=test)[,2]
model2.rf.pred = prediction(model2.rf.pr, test$Y)
model2.rf.perf = performance(model2.rf.pred,"tpr","fpr")
plot(model2.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#
importance(model2.rf)
varImpPlot(model2.rf)








