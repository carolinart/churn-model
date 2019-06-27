# Sets the locale depending on the system that executes the code
if (Sys.info()["sysname"] == "Linux") {
  Sys.setlocale(category = "LC_TIME", locale = "en_US.utf-8")
} else if (Sys.info()["sysname"] == "Windows") {
  Sys.setlocale(category = "LC_ALL", locale = "English")
} else{
  stop(paste("Configure locales for system with name:", Sys.info()["sysname"]))
}


#' Function which is intended for printing strings in the R console using the C syntax
pprint <- function(...) {
  cat(sprintf(...), "\n")
}

#' Canonizes a path given a kind of slash.
#' @param path: path to be canonized (character)
#' @param slash: slash symbol to be used to build the path (character; "/" by default for
#' assuring multi-plataform compatibilities) (character)
#' @return: the path canonized (character)
normalize_path = function(path, slash = "/") {
  path = sub(pattern = "\\/\\/", replacement = slash, path)
  path = sub(pattern = "\\/", replacement = slash, path)
  path = sub(pattern = "\\\\", replacement = slash, path)
  return(path)
}

#' Builds a path from chunks
#' @params ...: All the chunks of paths to be loaded.
#' @return: the path joined and normalized (character)
os.path.join <- function(...) {
  normalize_path(file.path(...), slash = "/")
}

#' Loads in the global environment all the paths parametrized in the settings.json file
#' @return: None (void)
load_paths <- function() {
  paths <- fromJSON("settings.json")$path
  data_path    <<- paths$data_path
  dictionary_path <<- os.path.join(data_path, "dictionary")
  original_path       <<- os.path.join(data_path, "original")
  staging_path   <<- os.path.join(data_path, "staging")
  meta_path <<- os.path.join(data_path, "metadata")
  master_path <<- os.path.join(data_path, "master")
  results_path   <<- paths$results_path
  log_path <<- paths$log_path
  scripts_path   <<- paths$scripts_path
  models_path <<- paths$models_path
  external_path <<- paths$external_data
  performance_path <<- os.path.join(results_path, "performance")
  prediction_path <<- os.path.join(results_path, "predictions")
}

#' load_model_parameters
#' Load model parameters selected from settings by user
#' @return None (void)
load_model_parameters <- function() {
  modeling <- fromJSON("settings.json")$modeling
  train_months <<- as.Date(modeling$train_months)
  dev_months <<- as.Date(modeling$dev_months)
  test_months <<- as.Date(modeling$test_months)
  model_alias <<- modeling$model_alias
}

#' load_scoring_parameters
#' Load scoring parameters selected from settings by user
#' @return None (void)
load_scoring_parameters <- function() {
  scoring <- fromJSON("settings.json")$scoring
  model_alias_scoring <<- scoring$model_alias
  date_to_score <<- scoring$date_to_score
  performance_calculation <<- scoring$performance_calculation
}

#' load_creation_parameters
#' Load creation parameters selected from settings by user
#' @return None (void)
load_creation_parameters <- function() {
  month_process <- fromJSON("settings.json")$month_process
  month_to_create <<- month_process$month_to_create
}



#' load_common_libraries
#' Load model libraries
#' @return None (void)
load_common_libraries <- function() {
  import("jsonlite")
  import("data.table")
  import("dplyr")
  import("lubridate")
  import("stringr")
  import("zoo")
  import("ggplot2")
  import("Matrix")
  import("stringi")
  import("xgboost")
  import("h2o")
  import("DALEX")
  import("gridExtra")
  import("pROC")
  import("readxl")
  import("gtools")
  import("forcats")
  import("Metrics")
  import("Ckmeans.1d.dp")
  import("caret")
  import("vip")
  import("scales")
  import("rsample")
  import("MASS")
}

#' Checks if a library is currently installed, installs it if not, and imports it.
#' @return: None (void)
import <- function(...) {
  gb = lapply(..., function(x) {
    if (!x %in% installed.packages()) {
      pprint("Library '%s' not found. Installing...", x)
      install.packages(x)
    }
    library(x, character.only = TRUE)
    pprint("Library '%s' imported", x)
  })
}

#' Sets the environment of the by importing the necessary modules, loading the
#' necessary libraries and loading the parametrized paths
#' @return: None (void)
set_environment <- function() {
  '%!in%' <<-  Negate('%in%')
  load_common_libraries()
  load_paths()
  load_model_parameters()
  load_scoring_parameters()
  load_creation_parameters()
  job <<- fromJSON("settings.json")$job
  source(os.path.join(scripts_path, "text_tools.R"))
  source(os.path.join(scripts_path, "file_tools.R"))
  
  # data transformation functions
  
  table_creation <- "table_creation"
  import_module(os.path.join(table_creation, "create_table.R"))
  import_module(os.path.join(table_creation, "crm_staging_maker.R"))
  import_module(os.path.join(table_creation, "datostc_staging_maker.R"))
  import_module(os.path.join(table_creation, "fac_staging_maker.R"))
  import_module(os.path.join(table_creation, "master_maker.R"))
  import_module(os.path.join(table_creation, "compare_premaker.R"))
  import_module(os.path.join(table_creation, "tenencia_staging_maker.R"))
  
  
  # modeling functions
  models <- "model"
  import_module(os.path.join(models, "common_reports.R"))
  import_module(os.path.join(models, "score_mensual.R"))
  import_module(os.path.join(models, "create_model.R"))
  
  
  
  
  # loadDataParameters()
  # Load configuration file and create log
  config <<- fromJSON("settings.json")
  jsontest = toJSON(config, pretty = TRUE, auto_unbox = TRUE)
  write(jsontest, file = os.path.join(config$paths$log_path,
                                      paste0("log", Sys.Date(), ".json")))
  
}


#Extraer numeros
extraer_numeros <-  function(a) {
  b <- strsplit(a, "")
  c <- c()
  
  for (i in 1:length(b[[1]])) {
    numeros <- c(as.character(0:9))
    if (b[[1]][i] %in% numeros) {
      c <- c(c, b[[1]][i])
    }
  }
  
  c <-  as.numeric(paste(c, collapse = ""))
  return(c)
}

