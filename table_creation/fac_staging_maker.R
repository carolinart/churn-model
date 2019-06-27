###################### Función ####################

# Cleaning and treatment of original bases to create month staging table
#' @param original_path : path field where original  base_list[[i]] places
#' @param staging_path : path field where staging  base_list[[i]] places
#' @return : staging table
#' @param yearmonth : yyyymm


fac_staging_maker <-
  function(yearmonth, original_path, staging_path) {
    
    
    #Leer archivos formato csv y extraer caracteres numéricos
    files <- list.files(path = paste0(external_path, "/FacturacionTC"), pattern = ".csv")
    
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
    print("Cargando data de facturacion")
    facturacion <-
      fread(
        paste0(external_path, "/FacturacionTC/", files),
        colClasses = "character",
        na.strings = c("", "NA")
      )
    
    #Reducir tamaño names
    names(facturacion) <- tolower(names(facturacion))
    
    #Confirmar variables
    meta <- os.path.join(meta_path, "facturacion/meta.csv") %>% fread()
    if (sum(meta$variables %!in% names(facturacion)) > 0) {
      print("Facturacion no contiene alguna de las variables necesarias")
    }
    
    #Seleccionar variables con metadata
    facturacion <- facturacion[, mget(meta$variables)]
    rm(meta)
    gc()
    
    facturacion <- facturacion[!(cant == "**")]
    
    #Conversion variables numericas
    num_vars <- c("plazo", "cant", "valor")
    facturacion[, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]
    
    print(paste("La tabla de facturacion tiene", nrow(facturacion),"columnas"))
    
    #Conversion de variables factor
    fact_vars <- c("clave", "tipo")
    facturacion[, (fact_vars) := lapply(.SD, as.factor), .SDcols = fact_vars]
    
    rm(fact_vars)
    gc()
    
    #Cambio nombres de variables
    setnames(facturacion, old = "codin", new = "cod_int")
    setnames(facturacion, old = "plazo", new = "cuotas")
    
    
    #Cargar diccionario MCC (contiene info sobre los codigos de comercio de las trx)
    dicc_mcc <- as.data.table(read_xlsx(path = paste0(dictionary_path, "/MCC.xlsx"), col_names = T))
    names(dicc_mcc) <- tolower(names(dicc_mcc))
    
    #Conversion tipo de variables
    mcc_vars <- c("mcc", "desc_mcc")
    dicc_mcc[, (mcc_vars) := lapply(.SD, as.factor), .SDcols = mcc_vars]
    
    
    #Ver cuantos mcc no cruzan con facturacion
    no_cruce <- facturacion[mcc %!in% dicc_mcc$mcc]
    if((nrow(no_cruce)>0)==TRUE){print(paste("No cruzan", nrow(no_cruce), "filas de la(s) categoría(s)", unique(no_cruce[, clave]), "de tipo", unique(no_cruce[, tipo])))}
    
    #Union de facturacion con el diccionario mcc
    fac <- merge(facturacion, dicc_mcc, by = "mcc", all.x = T) 
    
    
    
    if(no_cruce[, clave] == "Compras" & no_cruce[, tipo] == "INTERNACIONAL"){facturacion[is.na(desc_mcc) := "Compras Internacionales"]}
    
    
    
 
    ################ Etapa de creacion de variables #################
   
    #' Se requiere la creacion de las siguientes variables:
    #' Valores de compra: (media, mediana y moda)
    #' Cuotas: (media, mediana y moda)
    #' Numero de transacciones
    #' Dummy de compra de cartera
    #' Numero de compras nacionales
    #' Maxima cuota de compras nacionales
    #' Numero de compras internacionales
    
    ##Funcion para hallar la moda
    #' @usage mode(x)
    mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    ##'Crear dummy de compra de cartera. Para ello en la tabla que se crea a continuacion se debe hacer la suma de compra
    ##'de cartera y luego crear la dummy
    facturacion[, dummy_ccartera := ifelse(clave == "Ccartera", "1", "0")]
    facturacion$dummy_ccartera <- as.numeric(facturacion$dummy_ccartera)
    
    ##Creacion variables numero de compras nacionales
      #1) Crear dummy de compra nacional
    facturacion[, dummy_comp_nal := ifelse(clave == "Compras" & tipo == "NACIONAL", "1", "0")]
    facturacion$dummy_comp_nal <- as.numeric(facturacion$dummy_comp_nal)
   
  
    ##Creacion variable numero de compras internacionales
      #1) Crear dummy de compras internacionales 
    facturacion[, dummy_comp_int := ifelse(clave== "Compras" & tipo == "INTERNACIONAL", "1", "0")]
    facturacion$dummy_comp_int <- as.numeric(facturacion$dummy_comp_int)
    
    print("Creando tabla de facturacion con variables necesarias.")
    fac_tc <- facturacion[, .(
      ##Numero de transacciones
      trx = sum(cant),
      ##Promedio del valor
      vlr_pro = mean(valor),
      ##Mediana del valor
      vlr_median = median(valor),
      ##Moda del valor
      vlr_mode = mode(valor),
      ##Promedio de cuotas
      cuotas_pro = mean(cuotas),
      ##Mediana de las cuotas
      cuotas_median = median(cuotas),
      ##Moda de las cuotas
      cuotas_mode = mode(cuotas),
      ##Maxima cuota 
      max_cuota = max(cuotas),
      ##Añadir dummy de compra de cartera
      dummy_ccartera = sum(dummy_ccartera),
      ##Numero de compras nacionales
      total_comp_nal = sum(dummy_comp_nal),
      ##Numero de compras internacionales
      total_comp_int = sum(dummy_comp_int)
    ), by = .(cod_int)][order(cod_int,-trx)]
    
    print(paste("Con respecto a la tabla de facturacion original, al agrupar por codigo interno quedan", nrow(fac_tc), "filas"))
    
    ##Crear dummy de compra de cartera
    fac_tc[, dummy_ccartera := ifelse(dummy_ccartera > 0, "1", "0")]
    
    
    #Verificar que no haya duplicados de codigo interno
    ifelse((fac_tc[duplicated(cod_int)] %>% nrow()) == 0,
           "No hay duplicados en la version 1 de la tabla final. Todo good.",
           "Hay duplicados en la version 1 de la tabla final :(")
    
   
    #Comprobar que no haya duplicados
    ifelse((fac_tc[duplicated(cod_int)] %>% nrow()) == 0,
           "No hay duplicados en la tabla final. Todo good.",
           "Hay duplicados en la tabla final :(")
    

    #############################################################################
    ##Save data
    save <- list(paste0("fact_", position[position == yearmonth]))
    saveRDS((fac_tc), file = os.path.join(staging_path, paste0(save, ".rds")))
    print("Proceso finalizado")
    
  }

