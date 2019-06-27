###################### Función ####################

# Cleaning and treatment of original bases to create month staging table
#' @param original_path : path where original data is alocated
#' @param staging_path : path where rds is saved
#' @return :  returns a cleaned staging table
#' @param yearmonth : yyyymm


datostc_staging_maker <-
  function(yearmonth, original_path, staging_path) {
    
    #Leer archivos formato csv y extraer caracteres numéricos
    files <- list.files(path = original_path, pattern = ".csv")
    position <-
      paste0(sapply(str_extract_all(files, "[0-9]+"), "[[", 1)) %>% as.numeric
    
    # comparing cut months from user to master files created
    if (FALSE %in% (yearmonth %in% position)) {
      stop("Does not exist table for some of the months specified")
    }
    
    df_position <-
      data.frame(files = files, position = position)
    
    files <- df_position[df_position$position == yearmonth,]
    files <- as.character(files[[1]])
    
    #Leer datos
    print("Cargando data")
    datostc <-
      fread(
        os.path.join(original_path, files),
        colClasses = "character",
        na.strings = c("", "NA")
      )
    
    #Confirmar variables
    meta <- os.path.join(meta_path, "datostc/meta.csv") %>% fread()
    
    if (sum(meta$variables %!in% names(datostc)) > 0) {
      stop("Los datos tc no contiene alguna de las variables necesarias")
    }
    
    datostc <- datostc[, mget(meta$variables)]
    rm(meta)
    gc()
    
    #Cambiar nombre periodo
    setnames(datostc, "PERIODO_ACTUAL", "PERIODO")
    names(datostc) <- tolower(names(datostc)) #Reducir letra
    
    #Control calidad
    print("Previo a filtros")
    print(nrow(datostc))
    
    #Filtrar personas naturales (TIPO PERSONA = PN), se retiran NA's y PJ
    datostc <-
      datostc[tipo_persona == "PN" &
                segmento_producto == "PN" &
                tipo_id %in% c("1", "2")]
    
    #Control calidad
    print("Seleccionando unicamente persona natural")
    print(nrow(datostc))
    
    
    #Valores duplicados correspondientes a e-card se excluyen
    print("Eliminando duplicados")
    datostc <- datostc[producto != "Infinite e-Card"]
    
    #Eliminar duplicados de codigo interno
    datostc <- unique(datostc[order(antiguedad)], by = "cod_int", fromLast =
                        TRUE)
    
    
    #Control calidad
    print(nrow(datostc))
    
    #Limitar numero de días de mora por poliricas de cobranzas y campañas
    print(nrow(datostc))
    print("dias mora = 0")
    datostc <- datostc[dias_mora == 0]
    #Filtar saldos en mora y carteras vencidas
    print("cartera vencida = 0")
    datostc <- datostc[cartera_vencida == 0]
    #Control de calidad
    print(nrow(datostc))
    print("saldo mora = 0")
    datostc <- datostc[saldo_mora == 0]
    #Control de calidad
    print(nrow(datostc))
    
    #Eliminar
    datostc[is.na(prioridad_bloqueo_final), prioridad_bloqueo_final := "SIN BLOQUEO"]
    datostc <- datostc[prioridad_bloqueo_final != "MOROSIDAD FLP"]
    
    #Calcular utilizacion
    numeric_vars <-
      c("cartera_capital_total","cupo")
    
    datostc[, (numeric_vars) := lapply(.SD, as.numeric), .SDcols = numeric_vars]
    datostc <- datostc[cupo != 0]
    datostc[, util := round((cartera_capital_total / cupo), 2)]
    
    #Crear variable dummy de sobregiro
    datostc[, sobregiro := "1"]
    datostc[, sobregiro := ifelse(util > 1, "1", "0")]
    
    
    #Control calidad
    print(nrow(datostc))
    
    #Remover variables
    drop <-
      c(
        "tipo_id",
        "tipo_cta",
        "renta",
        "producto",
        "franquicia_producto",
        "segmento_producto",
        "dias_mora",
        "tipo_bloqueo_cod_bloqueo",
        "prioridad_bloqueo_final",
        "cod_bloqueo_status_fd",
        "bloqueos_status",
        "tipo_persona",
        "cartera_vencida",
        "saldo_mora",
        "utilizacion"
      )
    
    datostc[, (drop) := NULL]
    
    
    #Control calidad
    print(nrow(datostc))
    
    #Crear variables para la target 
    ##Cancelacion voluntaria y sin cancelacion (cancelacion involuntaria incluida en esta categoria)
    datostc <- datostc[, tipo_cancelacion := ifelse(is.na(tipo_cancelacion), "Sin Cancelacion",
                                                           ifelse(tipo_cancelacion == "Cancelación Voluntaria", "Cancelacion Voluntaria",
                                                                  "Cancelacion Involuntaria"))]
    
    #Eliminar cancelacion involuntaria (temporalmente)
    datostc <- datostc[tipo_cancelacion != "Cancelacion Involuntaria"]
                                                  
    
    #Control de calidad
    print(nrow(datostc))
    
    #Asignacion de formato a variables fecha
    date_vars <-
      c("f_ultimo_aumento",
        "f_venci",
        "f_ult_av",
        "f_ult_compra",
        "f_ult_pago")
    
    datostc[, (date_vars) := lapply(.SD, function(x) {
      x <- as.Date(x, format = "%d/%m/%Y")
    }), .SDcols = date_vars]
    
    
    #Asignar dia mes
    datostc[, periodo := paste0(periodo, "01")]
    datostc[, periodo := as.Date(periodo, format = "%Y%m%d")]
    
    #Calculo de días desde fecha:
    #Ultimo aumento
    datostc[, mes_ult_aumento := round(interval(start = as.Date(f_ultimo_aumento),
                                                end = as.Date(periodo)) /
                                         duration(num = 1, units = "months"))]
    
    datostc <-
      datostc[is.na(mes_ult_aumento), mes_ult_aumento := 0]
    
    #Fecha vencimiento
    datostc[, mes_venci := round(interval(start = as.Date(periodo),
                                          end = f_venci) /
                                   duration(num = 1, units = "months"))]
    
    #Fecha dias avance
    datostc[, dias_avance := round(interval(start = f_ult_av,
                                            end = as.Date(periodo)) /
                                     duration(num = 1, units = "days"))]
    
    datostc <-
      datostc[is.na(dias_avance), dias_avance := 0]
    
    datostc <-
      datostc[dias_avance<0, dias_avance := abs(dias_avance)]
    
    
    #Fecha ultima compra
    datostc[, dias_ult_compra := round(interval(start = f_ult_compra,
                                                end = as.Date(periodo) ) /
                                         duration(num = 1, units = "days"))]
    
    datostc <-
      datostc[is.na(dias_ult_compra), dias_ult_compra := 0]
    
    datostc <-
      datostc[dias_ult_compra<0, dias_ult_compra := abs(dias_ult_compra)]
    
    
    #Fecha ultimo pago
    datostc[, dias_ult_pago := round(interval(start = f_ult_pago,
                                              end = as.Date(periodo) ) /
                                       duration(num = 1, units = "days"))]
    
    datostc <-
      datostc[is.na(dias_ult_pago), dias_ult_pago := 0]
    
    datostc <-
      datostc[dias_ult_pago<0, dias_ult_pago := abs(dias_ult_pago)]
    
    #Remover variables
    drop <-
      c("f_ultimo_aumento",
        "f_venci",
        "cod_ciud_dir_ppal",
        "f_ult_av",
        "f_ult_compra",
        "f_ult_pago")
    
    datostc[, (drop) := NULL]
    
    #Control calidad
    print(nrow(datostc))
    
    #Asignacion variables factor
    factor_vars <-
      c("sub_producto",
        "tipo_tarj",
        "ciclo",
        "tipo_cancelacion")
    
    datostc[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
    
    #Variables numericas
    no_numeric <- c(factor_vars, "nro_id", "cod_int", "periodo")
    numeric_vars <-
      names(datostc)[!(names(datostc) %in% no_numeric)]
    
    #Aplicar clase numerico a numeric_vars
    datostc[, (numeric_vars) := lapply(.SD, as.numeric), .SDcols = numeric_vars]
    
    #Control de calidad
    print(nrow(datostc))
    
    #Guardar como RDS
    save <- paste0("datostc_", position[position == yearmonth])
    saveRDS((datostc), file = os.path.join(staging_path, paste0(save, ".rds")))
    
    print("Proceso finalizado")
    
  }
