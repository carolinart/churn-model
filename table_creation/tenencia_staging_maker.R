# Cleaning and treatment of original bases to create month staging table
#' @param original_path : path field where original  base_list[[i]] places
#' @param staging_path : path field where staging  base_list[[i]] places
#' @return : staging table
#' @param yearmonth : yyyymm

tenencia_staging_maker <-
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
    
    files <- df_position[df_position$position == yearmonth, ]
    files <- as.character(files[[1]])
    
    #Leer data
    print("Cargando data")
    tenencia <-
      fread(
        paste0(original_path, "/", files),
        colClasses = "character",
        na.strings = c("", "NA")
      )
    
    names(tenencia) <- tolower(names(tenencia))
    
    #conversion numerica
    num_vars <-
      c(
        "cant_tarj_credito",
        "cant_ctas_nomina",
        "cant_ctas_ahorro",
        "cant_crediservice",
        "cant_cdt",
        "cant_ordinario",
        "cant_libranza",
        "cant_libre_destino",
        "cant_otros",
        "cant_vehiculo",
        "cant_ctas_corriente",
        "cant_leasing",
        "cant_vivienda",
        "cant_fomento",
        "cant_microcredito",
        "cant_activo_pyme"
      )
    
    tenencia <-
      tenencia[, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]
    
    #Convertir los NA's a 0
    for (col in (num_vars)) {
      tenencia[is.na(get(col)), (col) := 0]
    }
    print(paste("Hay", nrow(tenencia), "filas"))
    
    
    #Filtrar solamente cedulas y cedulas de extranjería
    tenencia <-
      tenencia[tipo_identificacion == "C" | tipo_identificacion == "E"]
    
    print(paste("Después de filtrar cédulas hay", nrow(tenencia), "filas"))
    
    #Eliminar variables
    drop_vars <-
      c(
        "cant_tarj_credito",
        "tipo_identificacion",
        "periodo",
        "cant_otros",
        "cant_fomento",
        "cant_activo_pyme"
      )
    tenencia <- tenencia[, (drop_vars) := NULL]
    
    #Eliminar constructor por escasas observaciones de este producto
    if ("cant_constructor" %in% names(tenencia) == TRUE) {
      tenencia[, cant_constructor := NULL]
    }
    
    #Agregar la tabla por numero de id
    print("Agregando la tabla por numero ID")
    tenenciap <-
      tenencia[, lapply(.SD, sum), by = numero_identificacion]
    print(paste("Después de agregar la tabla de tenencia por número de ID, se obtienen", nrow(tenenciap), "filas"))
    
    #Crear columna de la suma de productos
    print("Creando columna de total de otros productos")
    tenenciap <-
      tenenciap[, cant_total_prod := rowSums(tenenciap[, c(2:12)])]
    
   
    ######################
    #Guardar tabla
    save <- paste0("tenencia_", position[position == yearmonth])
    saveRDS(tenenciap, file = os.path.join(staging_path, paste0(save, ".rds")))
    print("Proceso finalizado")
  }
