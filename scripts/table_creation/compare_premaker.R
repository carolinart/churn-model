


#' #clean a process original bases to create month staging table
#' #' @param original_path : path field where original  base_list[[i]] places
#' #' @param staging_path : path field where staging  base_list[[i]] places
#' #' @return : staging table



###################### Función ####################

compare_premaker <-
  function(original_path,
           staging_path,
           month_to_create = NULL) {
    #Compara la data en origina contra staging para halla posibles tablas faltantes
    
    ####original####
    files_original <- list.files(original_path)
    position_original <-
      as.vector(sapply(files_original, extraer_numeros))
    files_original <-
      data.frame(files = files_original , position = position_original)
    
    if(staging_path == "data/staging/crm"){
    inactivos <- c(grep("INACTIVOS", files_original$files))
    files_original <- files_original[-inactivos,]
    }
    
    
    if(staging_path == "data/staging/facturacion"){
    files_original <- files_original[-(1:17),]
    }
    
        if(staging_path == "data/staging/tenencia"){
      files_original <- files_original[-(1:10),]
    }
    
    #Cambiar segun los rezagos :V
    if(original_path == "data/original/datos_tc"){
      files_original <- files_original[-(1),]
    }
    
    ####staging####
    files_staging <- list.files(staging_path)
    position_staging <-
      sapply(str_extract_all(files_staging, "[0-9]+"), "[[", 1) %>% as.numeric
    files_staging <-
      data.frame(files = files_staging , position = position_staging)
    
    ####compare####
    compare <-
        files_original$position[which(files_original$position %!in% files_staging$position)]
    
    
    if (length(compare) == 0) {
      warning("Files Complete")
    }
    
    # compare <- compare[position == month_to_create]
    compare <- as.list(compare)
    
    #Evaluar deacuedo al origen del archivo.
    if (staging_path == "data/staging/crm") {
      #Llamar función para crear crm_staging

      for (i in compare) {
        print(paste0("Creando staging mes ausente ", i))
        crm_staging_maker(i, original_path, staging_path)
      }
      print("Archivos completos")
    }

    if (staging_path == "data/staging/datos_tc") {

      for (i in compare) {
        print(paste0("Creando staging mes ausente ", i))
        datostc_staging_maker(i, original_path, staging_path)
      }

      print("Archivos completos")
    }
    if (staging_path == "data/staging/facturacion") {
      
      
      for (i in compare) {
        print(paste0("Creando staging mes ausente ", i))
        fac_staging_maker(i, original_path, staging_path)
      }
      print("Archivos completos")
      
    }
    
    if (staging_path == "data/staging/tenencia") {

    for (i in compare) {
        print(paste0("Creando staging mes ausente ", i))
        tenencia_staging_maker(i, original_path, staging_path)
      }
      print("Archivos completos")

    }

    if (staging_path == "data/master/train") {

      for (i in compare) {
        print(paste0("Creando staging mes ausente ", i))
        master_maker(i, staging_path)
      }

    }

    # if (staging_path == "data/master/score") {
    # 
    #   for (i in compare) {
    #     print(paste0("Creando staging mes ausente ", i))
    #     master_marker(i, original_path = os.path.join(master_path, "score"))
    #   }
    # 
    # 
    # }
    
    print("Archivos completos")
    print("xd")
  }
