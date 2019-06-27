#' Funcion para crear las tablas master para cada periodo
#' @param yearmonth especifica el periodo en formato yyyy/mm para el cual se crea master
#' @param master_path es la carpeta donde se van a alojar los archivos rds resultantes
#' master_path <- "data/master/train"


master_maker <- function(yearmonth, master_path) {
  #comprobar meses
  
  staging_datostc <- os.path.join(staging_path, "/datos_tc")
  staging_crm <- os.path.join(staging_path, "/crm")
  staging_ten <- os.path.join(staging_path, "/tenencia")
  staging_fac <- os.path.join(staging_path, "/facturacion")
  
  
  files_datostc <- list.files(staging_datostc)
  files_crm <- list.files(staging_crm)
  files_ten <- list.files(staging_ten)
  files_fac <- list.files(staging_fac)
  
  position_datostc <-
    as.vector(sapply(files_datostc, extraer_numeros))
  
  position_crm <-
    as.vector(sapply(files_crm, extraer_numeros))
  
  position_ten <-
    as.vector(sapply(files_ten, extraer_numeros))
  
  position_fac <-
    as.vector(sapply(files_fac, extraer_numeros))
  
  
  if (FALSE %in% (yearmonth %in% position_datostc)) {
    stop("Does not exist datos tc table for some of the months specified")
  }
  
  if (yearmonth <= 201811) {
    yearmonth_crm <- 201811
  } else {
    yearmonth_crm <- yearmonth
  }
  
  if (FALSE %in% (yearmonth_crm %in% position_crm)) {
    stop("Does not exist crm table for some of the months specified")
  }
  
  if (FALSE %in% (yearmonth %in% position_ten)) {
    stop("Does not exist tenencia table for some of the months specified")
  }
  
  if (FALSE %in% (yearmonth %in% position_fac)) {
    stop("Does not exist facturacion table for some of the months specified")
  }
  
  #
  # #Verificar que existan rezagos
  # if (FALSE %in% ((yearmonth-1) %in% position_datostc)) {
  #   stop("Does not exist  table for some of the months specified")
  # }
  #
  # if (yearmonth <= 201811) {
  #   yearmonth_crm <- 201811
  # }
  
  # if (FALSE %in% (yearmonth_crm-1 %in% position_crm)) {
  #   stop("Does not exist  table for some of the months specified")
  # }
  #
  # if (FALSE %in% (yearmonth-1 %in% position_fac)) {
  #   stop("Does not exist  table for some of the months specified")
  # }
  #
  
  ####datos_tc####

  #Leer archivos formato csv y extraer posiciones
  files <- list.files(path = os.path.join(staging_path, "datos_tc"), pattern = ".rds")
  files <- os.path.join(os.path.join(staging_path, "datos_tc"), files)
  
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1)) %>% as.numeric
  
  # comparing cut months from user to master files created
  if (FALSE %in% (yearmonth %in% position)) {
    stop("Does not exist datos tc table for some of the master months specified")
  }
  
  df_position <-
    data.frame(files = files, position = position)
  
  files <- df_position[df_position$position == yearmonth,]
  files <- as.character(files[[1]])
  
  #Leer datostc de datos
  print("Cargando datos TC")
  datostc <- readRDS(files)
  
  ####CRM####
  
  #Leer archivos formato csv y extraer posiciones
  files <- list.files(path = os.path.join(staging_path, "crm"), pattern = ".rds")
  files <- os.path.join(os.path.join(staging_path, "crm"), files)
  
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1)) %>% as.numeric
  
  df_position <-
    data.frame(files = files, position = position)
  
  if (yearmonth <= 201811) {
    yearmonth_crm <- 201811
    files <- df_position[df_position$position == yearmonth_crm,]
    } else{
    files <- df_position[df_position$position == yearmonth,]
  }
  
  files <- as.character(files[[1]])
  files <- unique(files)
  
  #Leer crm
  print("Cargando crm")
  crm <- readRDS(files)
  
  ####Primer etapa####
  print("Uniendo datos TC con crm")
  semi_master <-
    data.table(left_join(datostc, crm, by = c("nro_id" = "crm_numero_identificacion")))
  
  print(paste("La tabla tiene", nrow(semi_master), "filas"))
  semi_master <- semi_master[!is.na(crm_antiguedad)]
  semi_master <- semi_master[!is.na(util)]
  semi_master <- semi_master[!is.na(sobregiro)]
  semi_master <- semi_master[!is.na(antiguedad)]
  print(paste("Despues de eliminar missing values la tabla tiene", nrow(semi_master), "filas"))
  
  
  #Verificar que no tenga duplicados
  if (nrow(semi_master[duplicated(cod_int)]) > 0) {
    warning("Hay duplicados luego de la union de datos TC con crm")
  }
  
  #Eliminar valores duplicados de id. Se seleccionan id con antiguedad de tarjeta mas reciente.
  # semi_master <-
  #   unique(semi_master[order(antiguedad)], by = "nro_id", fromLast = TRUE)
  # print(paste("Luego de agregar los numeros de ID por ultima antiguedad se tienen", nrow(semi_master), "filas"))
  # 
  # if (nrow(semi_master[duplicated(nro_id)]) > 0) {
  #   warning("Hay duplicados de id luego de la union de datos TC con crm")
  # }
  # 
  
  #####tenencia#####
  #Leer archivos y extraer posiciones
  files <- list.files(path = os.path.join(staging_path, "tenencia"), pattern = ".rds")
  files <- os.path.join(os.path.join(staging_path, "tenencia"), files)
  
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1)) %>% as.numeric
  
  df_position <-
    data.frame(files = files, position = position)
  
  files <- df_position[df_position$position == yearmonth, ]
  files <- as.character(files[[1]])
  
  #Leer data
  print("Cargando tenencia")
  tenencia <- readRDS(files)
  
  ####merge####
  print("Uniendo la tabla anterior con tenencia")
  master_v1 <-
    data.table(left_join(semi_master,  tenencia, by = c("nro_id" = "numero_identificacion")))
  
    #Verificar que no tenga duplicados
  if (nrow(master_v1[duplicated(cod_int)]) > 0) {
    warning("Hay duplicados luego de la union de semi_master con tenencia")
  }
  
  
  
  ####facturacion####
  #Leer archivos formato csv y extraer posiciones
  files <- list.files(path = os.path.join(staging_path, "facturacion"), pattern = ".rds")
  files <- os.path.join(os.path.join(staging_path, "facturacion"), files)
  
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1)) %>% as.numeric
  
  df_position <-
    data.frame(files = files, position = position)
  
  files <- df_position[df_position$position == yearmonth, ]
  files <- as.character(files[[1]])
  
  #Leer data
  print("Cargando facturacion")
  fac <- readRDS(files)
  
  ####merge final####
  print("Uniendo la tabla anterior con facturacion")
  master <-
    data.table(left_join(master_v1,  fac, by = "cod_int"))
  
  # #Asignar valores a NA
  # master %<>% mutate(
  #   grupo1 = fct_explicit_na(grupo1, na_level = "None"),
  #   grupo2 = fct_explicit_na(grupo2, na_level = "None"),
  #   grupo3 = fct_explicit_na(grupo3, na_level = "None"),
  #   grupo4 = fct_explicit_na(grupo4, na_level = "None")
  # )
  
  
  #'Necesitamos convertir los NA's de las variables de facturacion en 0, porque no todas
  #'las tarjetas de credito facturan en cada periodo
  master$dummy_ccartera <- as.numeric(master$dummy_ccartera)
  
  num_vars <-
    c(
      "vlr_pro",
      "vlr_mode",
      "vlr_median",
      "trx",
      "total_comp_nal",
      "total_comp_int",
      "max_cuota",
      "dummy_ccartera",
      "cuotas_pro",
      "cuotas_mode",
      "cuotas_median"
    )
  
  for (col in (num_vars)){
    master[is.na(get(col)), (col) := 0]}
  
  master$dummy_ccartera <- as.factor(master$dummy_ccartera)
  
  #Verificar que no hay duplicados
  if (nrow(master[duplicated(cod_int) > 0])) {
    warning("Hay duplicados luego de la union de master version 1 con facturacion")
  }
  
  # if (nrow(master[duplicated(nro_id) > 0])) {
  #   warning("Hay duplicados de id luego de la union de master version 1 con facturacion")
  # }
  # 
  
  #Variables rezagadas
  ##Datos tc
  files <- list.files(path = staging_datostc, pattern = ".rds")
  files <- os.path.join(staging_datostc, files)
  
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1)) %>% as.numeric
  
  df_position <-
    data.frame(files = files, position = position)
  
  if (yearmonth == 201901) {
    files <- df_position[df_position$position == 201812,]
  } else {
    files <- df_position[df_position$position == yearmonth - 1,]
  }
  
  files <- as.character(files[[1]])
  
  #Leer datostc de datos
  datostc <- readRDS(files)
  
  #Seleccionar variables
  print("Creando rezagos de datos tc")
  rez_var <- c(
    "cod_int",
    "sdo_capt_total",
    "util",
    "sobregiro",
    "dias_avance",
    "dias_ult_compra",
    "cupo"
  )
  
  drop <-
    names(datostc)[!(names(datostc) %in% rez_var)]
  
  datapast <- datostc[, (drop) := NULL]
  
  #Nombre variables
  
  # setnames(
  #   groups_dcast,
  #   old = c("1", "2", "3", "4"),
  #   new = c("grupo1", "grupo2", "grupo3", "grupo4")
  # )
  
  setnames(datapast, "sdo_capt_total", "sdo_capt_total_tmenos1")
  setnames(datapast, "util", "util_tmenos1")
  setnames(datapast, "sobregiro", "sobregiro_tmenos1")
  setnames(datapast, "dias_avance", "dias_avance_tmenos1")
  setnames(datapast, "dias_ult_compra", "dias_ult_compra_tmenos1")
  setnames(datapast, "cupo", "cupo_tmenos1")
  
  #Unir la tabla master en t con la master en t-1
  print("Uniendo la tabla master en t con la tabla de datos tc en t-1")  
  master <-
    data.table(left_join(master, datapast, by = "cod_int"))
  
  #Limpieza de las variables rezagadas. Convertir los NA's en 0.
  rez_var <- c(
    "sdo_capt_total_tmenos1",
    "util_tmenos1",
    "sobregiro_tmenos1",
    "dias_avance_tmenos1",
    "dias_ult_compra_tmenos1",
    "cupo_tmenos1"
  )
  
  for (col in (rez_var)){
    master[is.na(get(col)), (col) := 0]}
  
  #Verificar que no hay duplicados
  if (nrow(master[duplicated(cod_int) > 0])) {
    warning("Hay duplicados luego de la union de master t con master t-1")
  }
  
 
  #Variables rezagadas de tenencia de productos
  print("Creando rezagos de tenencia")
  files <- list.files(path = staging_ten, pattern = ".rds")
  files <- os.path.join(staging_ten, files)
  
  position <-
    paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1)) %>% as.numeric
  
  df_position <-
    data.frame(files = files, position = position)
  
  if (yearmonth == 201901) {
    files <- df_position[df_position$position == 201812,]
  } else {
    files <- df_position[df_position$position == yearmonth - 1,]
  }
  
  files <- as.character(files[[1]])
  
  #Leer datostc de datos
  tenencia <- readRDS(files)
  
  #Seleccionar variables
  
  rez_var <- c(
    "numero_identificacion", 
    "cant_ctas_nomina", 
    "cant_ctas_ahorro",
    "cant_ctas_corriente",
    "cant_crediservice", 
    "cant_cdt", 
    "cant_ordinario", 
    "cant_libranza", 
    "cant_libre_destino", 
    "cant_vehiculo", 
    "cant_vivienda",
    "cant_microcredito", 
    "cant_leasing", 
    "cant_total_prod"
  )
  
  datapast <- copy(tenencia)
  
  #Nombre variables
  
  # setnames(
  #   groups_dcast,
  #   old = c("1", "2", "3", "4"),
  #   new = c("grupo1", "grupo2", "grupo3", "grupo4")
  # )
  
  setnames(datapast, "cant_ctas_nomina", "cant_ctas_nomina_tmenos1")
  setnames(datapast, "cant_ctas_ahorro", "cant_ctas_ahorro_tmenos1")
  setnames(datapast, "cant_ctas_corriente", "cant_ctas_corriente_tmenos1")
  setnames(datapast, "cant_crediservice", "cant_crediservice_tmenos1")
  setnames(datapast, "cant_cdt", "cant_cdt_tmenos1")
  setnames(datapast, "cant_ordinario", "cant_ordinario_tmenos1")
  setnames(datapast, "cant_libranza", "cant_libranza_tmenos1")
  setnames(datapast, "cant_libre_destino", "cant_libre_destino_tmenos1")
  setnames(datapast, "cant_vehiculo", "cant_vehiculo_tmenos1")
  setnames(datapast, "cant_vivienda", "cant_vivienda_tmenos1")
  setnames(datapast, "cant_microcredito", "cant_microcredito_tmenos1")
  setnames(datapast, "cant_leasing", "cant_leasing_tmenos1")
  setnames(datapast, "cant_total_prod", "cant_total_prod_tmenos1")
  
  #Unir la tabla master en t con la master en t-1
  print("Uniendo la tabla master en t con la tabla de tenencia en t-1")  
  master <-
    data.table(left_join(master, datapast, by = c("nro_id" = "numero_identificacion")))
  
  #Limpieza de las variables rezagadas. Convertir los NA's en 0.
  rez_var <- c(
    "cant_ctas_nomina_tmenos1", 
    "cant_ctas_ahorro_tmenos1",
    "cant_ctas_corriente_tmenos1",
    "cant_crediservice_tmenos1", 
    "cant_cdt_tmenos1", 
    "cant_ordinario_tmenos1", 
    "cant_libranza_tmenos1", 
    "cant_libre_destino_tmenos1", 
    "cant_vehiculo_tmenos1", 
    "cant_vivienda_tmenos1",
    "cant_microcredito_tmenos1", 
    "cant_leasing_tmenos1", 
    "cant_total_prod_tmenos1"
  )
  
  for (col in (rez_var)){
    master[is.na(get(col)), (col) := 0]}
  
  #Verificar que no hay duplicados
  if (nrow(master[duplicated(cod_int) > 0])) {
    warning("Hay duplicados luego de la union de master t con master t-1")
  }
  
  
  #Crear variable evolucion de la utilizacion y evolucion del saldo
  #' Se debe tener en cuenta que cuando el resultado de la division es NA, es porque se divide 0 entre 0
  #' Mientras que cuando el resultado de la division es "Inf" es porque se esta dividiendo un numero positivo entre 0
  #' En este caso, importante tener en cuenta que cuando la tarjeta era nueva en el mes anterior, no hay realmente una 
  #' utilizacion de la tarjeta, por tanto, se le asigna el valor de 1.
  
  #Evolucion de la utilizacion
  master[, util_evoluc := ifelse(is.na(util/util_tmenos1) | (antiguedad == 1 & (util/util_tmenos1) == "Inf"), "1",
                                  ifelse(antiguedad > 1 & (util/util_tmenos1) == "Inf", util,
                                  (util/util_tmenos1)))]
  
 
  #Evolucion del saldo
  master[, sdo_evoluc := ifelse(is.na(sdo_capt_total/sdo_capt_total_tmenos1) | (antiguedad == 1 & (sdo_capt_total/sdo_capt_total_tmenos1) == "Inf"), "1",
                                 ifelse(antiguedad > 1 & (sdo_capt_total/sdo_capt_total_tmenos1) == "Inf", sdo_capt_total,
                                        (sdo_capt_total/sdo_capt_total_tmenos1)))]
  
  
  #Evolucion del cupo
  master[, cupo_evoluc := ifelse(is.na(cupo/cupo_tmenos1) | (antiguedad == 1 & (cupo/cupo_tmenos1) == "Inf"), "1",
                                 ifelse(antiguedad > 1 & (cupo/cupo_tmenos1) == "Inf", cupo,
                                        (cupo/cupo_tmenos1)))]
  
  
  #Evolucion de tenencia de productos
  master[, cant_total_prod_evoluc := ifelse(is.na(cant_total_prod/cant_total_prod_tmenos1) | (antiguedad == 1 & (cant_total_prod/cant_total_prod_tmenos1) == "Inf"), "1",
                                 ifelse(antiguedad > 1 & (cant_total_prod/cant_total_prod_tmenos1) == "Inf", cant_total_prod,
                                        (cant_total_prod/cant_total_prod_tmenos1)))]
  
  
  #Convertir las variables de evolucion a numericas
  evol_vars <- c("util_evoluc", "sdo_evoluc", "cupo_evoluc")
  master <- master[, (evol_vars) := lapply(.SD, as.numeric), .SDcols = evol_vars]  ##Volver numericas las variables de evolucion
  
  #############################################
  save <- paste0("master_", position[position == yearmonth])
  saveRDS(master, file = os.path.join(master_path, paste0(save, ".rds")))
  print("Proceso finalizado")
}



