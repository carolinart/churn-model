



create_monthly_tables <- function(month_to_create) {
  # create CRM tables
  print("create CRM tables")
  compare_premaker(
    original_path = paste0(external_path, "/CRM_Persona_Natural"),
    staging_path = os.path.join(staging_path, "crm"),
    month_to_create
  )
  
  print("create datostc tables")
  # create  datos tc
  compare_premaker(
    original_path = "data/original/datos_tc",
    staging_path = os.path.join(staging_path, "datos_tc"),
    month_to_create
  )
  
  print("create facturacion tables")
  # create facturacion tables
  compare_premaker(
    original_path = paste0(external_path, "/FacturacionTC"),
    staging_path = os.path.join(staging_path, "facturacion"),
    month_to_create
  )
  
  print("create tenencia tables")
  # create facturacion tables
  compare_premaker(
    original_path = paste0(external_path, "/Tenencia_Productos"),
    staging_path = os.path.join(staging_path, "tenencia"),
    month_to_create
  )
  
  print("create master_train tables")
  # create master_train tables
  compare_premaker(
    original_path = "data/original/datos_tc",
    staging_path = os.path.join(master_path, "train"),
    month_to_create
  )
  
  # print("create master_score tables")
  # # create master_score tables
  # compare_premaker(original_path = "data/original/datos_tc/", staging_path = os.path.join(master_path, "score"), month_to_create)
}