original_path <- "data/original/master_file"

#Variables de interés para el modelo
var_int <-
  c(
    "COD_INT",
    "TIPO_CLIENTE",
    "TIPO_CARTERA",
    "PERIODO",
    "ANTIGUEDAD",
    "RANGO_ANTIGUEDAD",
    "CUPO",
    "RANGO_CUPO",
    "UTILIZACION",
    "RANGO_UTILIZACION",
    "DIAS_MORA",
    "RANGO_EDAD", 
    "EDAD",
    "FRANQUICIA", 
    "RENTA", 
    "MARCA_COMPARTIDA", 
    "TIPO_TARJ",
    "CUOMANO",
    "CARTERA_CAPITAL_TOTAL",
    "CARTERA_VIGENTE", 
    "SDO_TOTAL",
    "PAGO_MINIMO",
    "RANGO_PAGO_MINIMO",
    "SALDO_MORA",
    # "DEPARTAMENTO",
    "GENERO",
    # "MUNICIPIO",
    # "DESCRIPCION_TIPO_VIVIENDA",
    "DESCRIPCION_NIVEL_EDUCATIVO",
    # "DESCRIPCION_ESTADO_CIVIL",
    # "DESCRIPCION_SEG_COMERCIAL",
    # "DESCRIPCION_OCUPACION",
    # "CLASIFIC_ACTIVIDAD_ECONOMICA",
    "AA_ESTRATO", 
    # "AA_VLR_ACTIVOS",
    # "AA_VLR_ING_BRU_MES",
    # "AA_VLR_EGRESO_MES",
    # "AA_VLR_PASIVOS",
    "SCORE_COMP_CALIF_ACT")

staging_path <- "data/staging/master_file"

#' Master vp staging maker
#'
#' @param original_path : path where original data is alocated
#' @param var_int 
#' @param staging_path 
#'
#' @return : return a cleaned staging table
#' @export
#'
#' @examples


master_staging_maker <- function(original_path, var_int, staging_path) {
  
  #Leer archivos formato csv y extraer caracteres numéricos
  fileList <- list.files(path = original_path, pattern = ".csv")
  position <-
    sapply(str_extract_all(fileList, "[0-9]+"), "[[", 1) %>% as.numeric
  
  base_list <- list()
  
  
  #Leer bases de datos
  for(i in 1:length(fileList)){
    #  dt <- fread(i, colClasses = "character", na.strings=c("","NA")) 
    base_list[[i]] <- fread(os.path.join(original_path, fileList[i]) , colClasses = "character", na.strings=c("","NA"))
    base_list[[i]][, PERIODO := position[i]]
    base_list[[i]] <- base_list[[i]][, mget(var_int)]
  }
  
  base <- rbindlist(base_list)
  names(base) <- tolower(names(base))
  rm(base_list)
  
  #Ver significancia de las variables
  summary(factor(base$tipo_cartera))
  
  #Filtrar solamente cartera de consumo
  base <- base[tipo_cartera == "Consumo"]
  
  #Crear columna month id para identificar los meses
  base[, month.id := as.numeric(factor(periodo, levels = unique(periodo)))]
  
  
  ###########################################################################################
  
  #Convertir formato yyyymm de periodo a fecha formato yearmon
  base[, fecha := as.Date(paste0(as.character(periodo), '01'), format='%Y%m%d')]
  base[, fecha := as.yearmon(fecha)]
  base0 <- copy(base[, .(cod_int, periodo, fecha, month.id)])
  
  
  #Cálculo variables
  base[, month.id := as.numeric(factor(periodo, levels = unique(periodo)))]
  
  base1 <- base0[, .(cod_int, month.id)]
  base1[, month.id := month.id - 2]
  base1[, var.churn := 1]
  
  base0 <- merge(base, base1, by = c("cod_int", "month.id"), all.x = T)
  
  
  #Eliminar últimos dos periodos
  base_target <- base0[ periodo != '201901' & periodo !=  '201902']
  base_target[, .N, by = periodo]
  
  
  #Definir target
  base_target[var.churn == "1", var.churn := 0]   
  base_target[is.na(var.churn), var.churn := 1]     ##Los NA son "churn"
  
  
  #Tratamiento a nombre variable churn
  setnames(base_target, "var.churn", "Y")
  
  var_int <- tolower(var_int[-c(1:3)])
  
  master <- copy(base_target[, mget(c(var_int, "Y"))])
  
  #########################################################################################
  
  #Variables categoricas
  # factor_vars <- grep( "rango", var_int, value = T)
  factor_vars <-
    c(
      # factor_vars
      "franquicia",
      "renta",
      "marca_compartida",
      "tipo_tarj",
      # "departamento",
      "genero",
      # "descripcion_estado_civil",
      # "descripcion_seg_comercial",
      # "descripcion_ocupacion",
      # "clasific_actividad_economica",
      "aa_estrato",
      "score_comp_calif_act",
      "descripcion_nivel_educativo",
      "rango_antiguedad",
      "rango_cupo",
      "rango_utilizacion",
      "rango_edad",
      "rango_pago_minimo"
      # "descripcion_tipo_vivienda"
    )
  
  #Variables categoricas y numericas
  master[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
  
  numeric_vars <- names(master)[!(names(master) %in% factor_vars)]
  
  #Hallar numero de utilización (i.e quitarle el "%" y dejarlo en numero)
  master[, util := as.numeric(str_extract_all(master$utilizacion, "[0-9]+"))/100]
  
  
  #Aplicar clase numerico a numeric_vars
  master[, (numeric_vars) := lapply(.SD, as.numeric), .SDcols = numeric_vars]

  master[, utilizacion := NULL]
  
  #Count NA
  sapply(master, function(x){sum(is.na(x))}) 
  na_var <- sapply(master, function(x){sum(is.na(x))}) 
  na_var <- na_var[na_var > 0]
  na_var <- names(na_var)
  
  master[, lapply(.SD, function(x){sum(is.na(x))}), by = periodo,  .SDcols = na_var]
  (master[, .N] - nrow(na.omit(master)))/master[, .N]
  
  
  #Omitir los missing values -> master limpio
  master_lp <- na.omit(master)
  
    ## falta tratamiento de datos
  
  saveRDS(master_lp, file = os.path.join(staging_path, "master_vp_cleaned.rds"))

  }

