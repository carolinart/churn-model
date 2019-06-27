#' En este script se ven las variables importantes del modelo de McKinsey y las tablas
#' de las que estas provienen
mk_path <- "churn_mckinsey/"

#Leer meta variables
meta_mk_files <- list.files(os.path.join(mk_path, "meta"), pattern = "metaVariables_")
meta_path_mk <- os.path.join(mk_path, "meta", meta_mk_files)

base_list <- list()

for(i in 1:length(meta_mk_files)){
  base_list[[i]] <- read_excel(meta_path_mk[i])
  base_list[[i]] <- as.data.table(base_list[[i]])
}

#'Cambiar el nombre de cada elemento de la lista para que cada uno tenga el nombre del archivo
#'del que provienen los datos (e.g. el elemento 1 de la lista, base_list[[1]], sale de la tabla
#'metaVariables_additional_info_portfolio_accounts.xlsx. Para que el nombre de ese elemento sea
#'metaVariables_additional_info_portfolio_accounts.xlsx y no base_list[[1]], se debe utilizar meta_mk_files,
#'donde están todos los nombres de los archivos a leer. Esto debe hacerse para cada elemento i de la lista)

names(base_list) <- meta_mk_files


#Crear una variable que indique el nombre de la tabla de donde salen los datos
for(i in 1:length(meta_mk_files)){
 base_list[[i]] <- base_list[[i]][, nombre_tabla := names(base_list[i])]
}

#Unir verticalmente las meta variables
base_var <- rbindlist(base_list, fill = T)

#Eliminar filas que contengan x_sell
drop_vars <- grep("_xsell_|_xsell3_", meta_path_mk, value = T)

base_var <- base_var[!(base_var$variable %in% drop_vars),]

base_var[duplicated(variable)] %>% nrow()


###########################################################
#Leer variables importantes
imp_vars <- fread(os.path.join(mk_path, "20190404_Modelo_bajas/20190404_Modelo_bajas_important_variables.csv"), 
                  colClasses = "character", na.strings = c("", "NA"))
setnames(imp_vars, old = "Feature", new = "variable")

##########################################################
#Crear tabla únicamente con las variables que se encuentran en las variables importantes
base_imp_var <- base_var[variable %in% imp_vars$variable]

#Merge de variables importantes con la tabla anterior
important_var <- merge(base_imp_var, imp_vars, by = "variable", all.x = T)

#Eliminar variables que no sirven
drop_vars <- c("impute", "...6", "format")
important_var <- important_var[, (drop_vars):= NULL]

#########################################################
#Guardar el archivo
write.csv(important_var, file = "scripts/variable_ideas/important_vars_mck.csv")
print("Archivo guardado :)")

# 
# base_var[variable %in% imp_vars$Feature] %>% nrow()
# imp_var_mk <- imp_vars[variable %in% base_var$variable] 
