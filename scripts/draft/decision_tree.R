#Leer funcion staging en consola
  #master_staging_maker(original_path, var_int, staging_path)


#Llamar staging
staging_path <- "data/staging/master_file/master_vp_cleaned.rds" 


#Leer master limpio
master_lp <- readRDS(os.path.join(getwd(), staging_path))


#Asignar variable respuesta como factor
master_lp[, Y := as.factor(Y)]

set.seed(123)

#Dividir el master en train y test
master_train <- base[periodo != "201812"]
master_test <- base[periodo == "201812"]


#Training
dtree = rpart(Y ~ dias_mora + util + antiguedad + cupo + edad + franquicia + 
                cuomano + saldo_mora + descripcion_nivel_educativo + genero + 
                aa_estrato + tipo_tarj + cartera_vigente + pago_minimo + 
                periodo, 
              data = master_train, method = "class")

summary(dtree)


#Predicting
dpred <- predict(dtree,type = "class", newdata = master_test)

conf_matrix <- confusion.matrix(obs = master_test$Y, pred = dpred,  threshold = 0.1)

Uplft10 <- Uplift(conf_matrix = conf_matrix)
Uplft10


Recall10 <- Recall(conf_matrix = conf_matrix)
Recall10


Precision10 <- Precision(conf_matrix = conf_matrix)
Precision10

# ROC(pgorro = dpred, master_test = master_test) 
# 
# Grafica_uplift(pgorro = dpred, master_test = master_test)
# 
# Grafica_recall(pgorro = dpred, master_test = master_test)
# 
# Grafica_precision(pgorro = dpred, master_test = master_test) 

##Se observa una mejoria en cuanto a precision en el primer decil (88%), pero el recall del primer decil es 25%,
##peor al de logit (31%)
