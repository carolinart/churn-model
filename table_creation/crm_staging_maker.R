#Posible forma de la funcion

#'
#' #clean a process original bases to create month staging table
#' #' @param month_to_create : month to process i = month
#' #' @param threshold : threshold to clean financial variables
#' #' @param original_path : path field where original  base_list[[i]] places
#' #' @param staging_path : path field where staging  base_list[[i]] places
#' #' @return : staging table
#'
#' #' @param yearmonth : yyyymm

###################### Función ####################


crm_staging_maker <-
  function(yearmonth, original_path, staging_path) {
   

    #Leer archivos formato csv y extraer caracteres numéricos
    files <- list.files(path = original_path, pattern = ".csv")
    position <-
      paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1),
             sapply(str_extract_all(files, "[0-9]+"), "[[", 2))  %>% as.numeric
    
    # comparing cut months from user to master files created
    if (FALSE %in% (yearmonth %in% position)) {
      stop("Does not exist  table for some of the months specified")
    }
    
    df_position <-
      data.frame(files = files, position = position)
    
    if(staging_path == "data/staging/crm"){
      inactivos <- c(grep("INACTIVOS", df_position$files))
      index(df_position)
      df_position <- df_position[-inactivos,]
    }
    
    files <- df_position[df_position$position == yearmonth,]
    files <- as.character(files[[1]])
    
    #Cambiar formato de fechas, nombres en min
    crm <-
      fread(
        paste0(original_path, "/",files),
        colClasses = "character",
        na.strings = c("", "NA")
      )
    
    names(crm) <- tolower(names(crm))
    
    #Formato fechas
    crm[, periodo := paste0(periodo, "01")]
    crm[, periodo := as.Date(periodo, format = "%Y%m%d")]
    
    #Unicamente para periodos anteriores a 2019-01
    if (yearmonth < 201901) {
      crm[, crm_fecha_nacimiento := as.Date(crm_fecha_nacimiento, format = "%d/%m/%Y")]
      crm[, crm_fecha_vinculacion_banco := as.Date(crm_fecha_vinculacion_banco, format = "%d/%m/%Y")]
    } else{
      print("Procesando ... ")
    }
    
    #Crecion de edad y antiguedad
    crm[, crm_edad := round(interval(start = as.Date(crm_fecha_nacimiento),
                                     end = periodo) /
                              duration(num = 1, units = "years"))]
    
    crm[, crm_antiguedad := round(interval(
      start = as.Date(crm_fecha_vinculacion_banco),
      end = periodo
    ) /
      duration(num = 1, units = "months"))]
    
    #Verificación de fechas
    subset(
      crm %>% dplyr::select(crm_fecha_vinculacion_banco, periodo, crm_antiguedad),
      is.na(crm_antiguedad)
    )
    


    # #diccionario para reducir segmetacion de grupos comerciales
    dic_segmento <-
      fread(
        os.path.join(dictionary_path, "dic_segmento.csv"),
        colClasses = "character",
        header = T
      )
    
    #Creacion de diccionario con el nombre de los segmentos principales
    dic_segmento$segmento <-
      factor(dic_segmento$segmento, ordered = FALSE)
    
    # #union de dic y bases
    crm <-
      merge(crm, dic_segmento, by = "crm_nombre_segmento", all.x = T)
    
    crm <- crm[segmento != "Pj"]  ##Not take into account "Pj"
    setnames(crm, "segmento", "crm_segmento")
    
    #Control de calidad
    print(nrow(crm))
    
    #Filtrar
    crm <- crm[crm_genero != "D"]
    print(nrow((crm)))
    
    
    #De acuerdo a la grafica se pueden hacer propuesas tentativas de filtro por varaibles con mayor %na's
    var_drop_old <-
      c(
        "crm_nombre_nivel_educativo",
        "crm_codigo_nivel_educativo",
        "crm_codigo_estado_civil",
        "crm_nombre_estado_civil",
        "crm_codigo_tipo_vivienda",
        "crm_codigo_ocupacion",
        "crm_valor_ventas",
        "crm_declara_renta",
        "crm_nombre_segmento"
      )
    
    
    var_drop_new <-
      c(
        "crm_nombre_nivel_educativo",
        "crm_codigo_nivel_educativo",
        "crm_codigo_estado_civil",
        "crm_nombre_estado_civil",
        "crm_codigo_tipo_vivienda",
        "crm_num_personas_cargo_men18",
        "crm_num_personas_cargo_may18",
        "crm_codigo_subsegmento",
        "crm_condicion",
        "crm_codigo_ocupacion",
        "crm_valor_ventas",
        "crm_declara_renta",
        "crm_nombre_segmento"
      )
    
    #Loop para seleccionar variables
    if (yearmonth <= 201812) {
      keep_vars <-
        names(crm)[names(crm) %!in% var_drop_old]
    } else{
      keep_vars <- names(crm)[names(crm) %!in% var_drop_new]
    }
    
    #Creacion dt nuevo con variable selecionadas
    crm <- crm[, mget(keep_vars)]
    
    #Control de calidad
    print(nrow(crm))
    
    #Eliminar personas con NA en estrato
    obs_sin_informacion <- crm[is.na(crm_estrato)]
    crm <- crm[!(is.na(crm_estrato))]
    
    #Control de calidad
    print(nrow(crm))
    
    #Limitar datos atipicos de edad > 100 y antiguedad = 0
    print("Edad a 100")
    
    crm <- crm[crm_edad < 100]
    
    #Control de calidad
    print(nrow(crm))
    
    #Se imputan las variables financieras de las observaciones
    finVars <-
      c(
        "crm_valor_activos",
        "crm_valor_ing_bru_mes",
        "crm_valor_egreso_mes",
        "crm_valor_pasivos"
      )
    
    
    #Variables numericas
    crm[, (finVars) := lapply(.SD, as.numeric), .SDcols = finVars]
    
    
    #Imputación
    
    aux.income <-
      crm[, lapply(.SD, unique), by = .(crm_estrato, crm_numero_identificacion),  .SDcols = finVars]
    aux.income <- aux.income[, .(
      median.ingresos = median(crm_valor_ing_bru_mes[crm_valor_ing_bru_mes > 100000], na.rm = T),
      median.activos = median(crm_valor_activos[crm_valor_activos > 100000], na.rm = T),
      median.pasivos = median(crm_valor_pasivos[crm_valor_pasivos > 4], na.rm = T),
      median.egresos = median(crm_valor_egreso_mes[crm_valor_egreso_mes > 100000], na.rm = T)
    ),
    by = .(crm_estrato)]
    crm <- merge(crm, aux.income, by = c("crm_estrato"))
    crm[is.na(crm_valor_ing_bru_mes) |
          crm_valor_ing_bru_mes < 100000,
        crm_valor_ing_bru_mes := median.ingresos]
    crm[is.na(crm_valor_activos) | crm_valor_activos < 100000,
        crm_valor_activos := median.activos]
    crm[is.na(crm_valor_pasivos) | crm_valor_pasivos == 0,
        crm_valor_pasivos := median.pasivos]
    crm[is.na(crm_valor_egreso_mes) |
          crm_valor_egreso_mes <= 100000,
        crm_valor_egreso_mes := median.egresos]
    #Topes para variables: Para valores muy altos de las variables, se selecciona el percentil 99 y se le asigna este valor
    percentil_99 <-
      crm[, lapply(.SD, function(x)
        quantile(x, prob = 0.99, na.rm = TRUE)), .SDcols = finVars]
    crm[crm_valor_ing_bru_mes > percentil_99$crm_valor_ing_bru_mes, crm_valor_ing_bru_mes := percentil_99$crm_valor_ing_bru_mes]
    crm[crm_valor_activos > percentil_99$crm_valor_activos, crm_valor_activos := percentil_99$crm_valor_activos]
    crm[crm_valor_pasivos > percentil_99$crm_valor_pasivos, crm_valor_pasivos := percentil_99$crm_valor_pasivos]
    crm[crm_valor_egreso_mes > percentil_99$crm_valor_egreso_mes, crm_valor_egreso_mes := percentil_99$crm_valor_egreso_mes]
    
    #Borrando variables
    
    var_drop <-
      c(
        "periodo",
        "crm_tipo_identificacion",
        "crm_fecha_nacimiento",
        "crm_ciiu",
        "crm_nombre_ocupacion",
        "crm_codigo_segmento_comercial",
        "crm_grupo_segmento",
        "median.ingresos",
        "median.activos",
        "median.pasivos",
        "median.egresos"
      )
    
    crm[, (var_drop) := NULL]
    
    rm(obs_sin_informacion, percentil_99, aux.income, dic_segmento)
    gc()
    
    # Añadir municipio
    dic_munic <-  os.path.join(dictionary_path, "ciudad.csv") %>%
      fread(colClasses = "character", header = T)
    
    #Reducir nombres
    names(dic_munic) <- tolower(names(dic_munic))
    
    #asignar codigos
    
    crm[, codigo_depto := as.character(substr(crm_codigo_ciudad_ppal, 1, 2))]
    crm[, codigo_municipio := as.character(substr(crm_codigo_ciudad_ppal, 3, 100))]
    
    var <- c("codigo_depto",
             "codigo_municipio")
    
    crm[, (var) := lapply(.SD, as.numeric), .SDcols = var]
    dic_munic[, (var) := lapply(.SD, as.numeric), .SDcols = var]
    
    crm <-
      merge(
        crm,
        dic_munic,
        by = c("codigo_depto", "codigo_municipio"),
        all.x = T
      )
    crm <-
      crm[!is.na(departamento)] #Hay valores en la data que no corresponden a valores numericos (codigos)
    
    #Eliminar valores duplicados. Se seleccionan id con fecha de vinculación mas reciente.
    crm <-
      unique(crm[order(crm_fecha_vinculacion_banco)], by = "crm_numero_identificacion", fromLast =
               TRUE)
    
    
    #Limpiando
    
    var_drop <-
      c(
        "crm_codigo_ciudad_ppal",
        "crm_fecha_vinculacion_banco",
        "crm_grupo_ocupacion",
        "codigo_depto",
        "codigo_municipio",
        "municipio"
      )
    
    crm[, (var_drop) := NULL]
    
    
    rm(dic_munic)
    gc()
    
    setnames(crm, "departamento", "crm_depto")
    print(nrow(crm))
    
    
    #Asignar clases a las variables
    id_vars <- c("crm_numero_identificacion")
    
    var_num <-
      c(
        "crm_valor_activos",
        "crm_valor_ing_bru_mes",
        "crm_valor_egreso_mes",
        "crm_valor_pasivos",
        "crm_edad",
        "crm_antiguedad"
      )
    
    #Variables numericas
    crm[, (var_num) := lapply(.SD, as.numeric), .SDcols = var_num]
    no_factor <- c(id_vars, var_num)
    
    
    #Variables factor
    factor_vars <- names(crm)[!(names(crm) %in% no_factor)]
    crm[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
    crm <- crm[!is.na(crm_numero_identificacion)]
    
    
    #############guardar############
    
    save <- paste0("crm_", unique(position[position == yearmonth]))
    saveRDS((crm), file = os.path.join(staging_path, paste0(save, ".rds")))
    
    print("Proceso finalizado")
    
  }

