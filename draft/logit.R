#Leer funcion staging en consola
  #master_staging_maker(original_path, var_int, staging_path)


#Llamar staging
staging_path <- "data/staging/master_file/master_vp_cleaned.rds" 


#Leer master limpio
master_lp <- readRDS(os.path.join(getwd(), staging_path))


#Asignar variable respuesta como factor
master_lp[, Y := as.factor(Y)]


#Balanceo de los datos
balance <- master_lp[Y == 0]
index <- sample(1:nrow(balance), 500000)
balance <- balance[index]
target <- master_lp[Y == 1]

base_list <- list(target, balance)
base <- rbindlist(base_list)


#Dividir el master en train y test
master_train <- base[periodo != "201812"]
master_test <- base[periodo == "201812"]

summary(factor(master_train$Y))
summary(factor(master_test$Y))
head(master_train)


# master_master_test <- na.omit(master_master_test)
# sapply(master_train, function(x){unique(is.na(x))} )

#############################################################################
##Modelamiento##
############################################################################

set.seed(666)

#porc_master_train <- 0.8

#ID_master_train <- sort(sample(nrow(master), nrow(master)*porc_master_train))
#master_train <- master[ID_master_train]
#master_test <- Ahorros[-ID_master_train]


if (nrow(master_train) + nrow(master_test) != nrow(master_lp)){
  stop("Hay un error en la separacion de data.")}


sample_train <- sample(1:nrow(master_train), 0.3*nrow(master_train))
master_train_sp <- master_train[sample_train,]


#####################################################################
##Seleccionar variables##
####################################################################

#Ver entre cartera vigente y cartera capital total cual tiene menor AIC
logit_cart <- glm(formula = Y ~ cartera_vigente + cartera_capital_total, 
                  family = binomial(link = "logit"), 
                  data = master_train)


fwd_cartera <-  stepAIC(logit_cart, direction="both")

summary(fwd_cartera)

   ##cartera_capital_total tiene menor AIC
rm(logit_cart, fwd_cartera)


#Modelo 1: Usando variables escogidas que no tengan mucha correlacion y solo rangos
logit1 <- glm(formula = Y ~ periodo + rango_antiguedad + 
                rango_cupo + dias_mora + rango_edad +
                franquicia + tipo_tarj + cuomano + 
                cartera_capital_total + sdo_total + 
                rango_pago_minimo + saldo_mora + genero + descripcion_nivel_educativo + 
                aa_estrato + util, 
              family = binomial(link = "logit"), 
              data = master_train)

summary(logit1)

vif(logit1)  ##cartera_capital_total y saldo total tienen un factor inflacionario muy alto



#Modelo 2:  Modelo sin rangos, solamente continuas
logit2 <- glm(formula = Y ~ periodo + antiguedad + edad +
                cupo + dias_mora + franquicia + tipo_tarj + cuomano + 
                cartera_capital_total + pago_minimo + 
                saldo_mora + genero + descripcion_nivel_educativo + 
                aa_estrato + util, 
              family = binomial(link = "logit"), 
              data = master_train)

summary(logit2)

vif(logit2)

  ##Estas variables son mucho mas significativas y tienen un VIF mas razonable

rm(logit1, logit2)


############################################################################
##Construccion modelo##
###########################################################################

##Ajuste del modelo sin variables, únicamente intercepto
nothing <- glm(Y ~ 1, family=binomial, data = master_train)


##Ajuste del modelo saturado/ algunas variables escogidas (con modelo 2)
fullmodel <- glm(formula = Y ~ periodo + antiguedad + edad +
                   cupo + dias_mora + franquicia + tipo_tarj + cuomano + 
                   cartera_capital_total + pago_minimo + 
                   saldo_mora + genero + descripcion_nivel_educativo + 
                   aa_estrato + util, 
                 family = binomial(link = "logit"), 
                 data = master_train)


###Forward para seleccionar variables

forward <- step(nothing,
                scope=list(lower=formula(nothing),upper=formula(fullmodel)), direction="forward")


rm(forward, nothing)
gc()



#Modelo final (con la combinacion de variables del forward)
logit <- glm(
  formula = Y ~ dias_mora + util + antiguedad + cupo + edad + cartera_capital_total + 
    franquicia + cuomano + descripcion_nivel_educativo + genero + 
    tipo_tarj + aa_estrato + saldo_mora + pago_minimo + periodo,
  family = binomial(link = "logit"), 
  data = master_train)


#Calcular vector de probabilides predichas e indicadores
pgorro <- predict(logit, type = "response", newdata = master_test)

confusion_matrix(predict = pgorro, response = master_test$Y, threshold = 0.1)

ROC(pgorro = pgorro, master_test = master_test) 

Grafica_uplift(pgorro = pgorro, master_test = master_test)

Grafica_recall(pgorro = pgorro, master_test = master_test)

Grafica_precision(pgorro = pgorro, master_test = master_test)
