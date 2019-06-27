setwd("//bdbemcfs/Analytics/proyectos_practica/modelo_churn/data/original/master_file")

fileList <- list.files(path=getwd(), pattern=".csv")
position <- sapply(str_extract_all(fileList, "[0-9]+"), "[[", 1) %>% as.numeric

# levels_month.id <- c("201801", "201802", "201803", "201804", "201805",
#                      "201806", "201807", "201808", "201809","201810", 
#                      "201811", "201812", "201901", "201902",
#                      fileList[position])

levels_month.id <- c("201808", "201809","201810", "201811", "201812", "201901", "201902",
                     fileList[position])

base_list <- list()

for(i in 1:length(fileList)){
  #  dt <- fread(i, colClasses = "character", na.strings=c("","NA")) 
  base_list[[i]] <- fread(fileList[i], colClasses = "character", na.strings=c("","NA"))
  base_list[[i]][, PERIODO := position[i]]
  base_list[[i]] <- base_list[[i]][, .(COD_INT, TIPO_CTA, TIPO_CLIENTE, TIPO_CARTERA, PERIODO)]
}

base <- rbindlist(base_list)
names(base) <- tolower(names(base))
rm(base_list)

summary(factor(base$tipo_cartera))
base <- base[tipo_cartera == "Consumo"]
base[, month.id := as.numeric(factor(periodo, levels = unique(levels_month.id)))]

#Convertir formato yyyymm de periodo a fecha formato yearmon
base[, fecha := as.Date(paste0(as.character(periodo), '01'), format='%Y%m%d')]
base[, fecha := as.yearmon(fecha)]
base0 <- copy(base[, .(cod_int, periodo, fecha, month.id)])

base1 <- base0[, .(cod_int, month.id)]
base1[, month.id := month.id - 1]
base1[, var.churn := 1]


#Cálculo variables
base[, month.id := as.numeric(factor(periodo, levels = unique(levels_month.id)))]

base1 <- base0[, .(cod_int, month.id)]
base1[, month.id := month.id - 1]
base1[, var.churn := 1]

base0 <- merge(base0, base1, by = c("cod_int", "month.id"), all.x = T)

base1 <- base0[, .(cod_int, month.id)]
base1[, month.id := month.id + 1]
base1[, var.stock := 1]

base0 <- merge(base0, base1, by = c("cod_int", "month.id"), all.x = T)

#Gráfica evolución total TC
tc_periodo <- base0[, .N, by = fecha][order(fecha)]

# tc_periodo <- base0[, .N, by = fecha][order(fecha)]
tc_master <-  tc_periodo[, N := as.numeric(N)]


rm(list=setdiff(ls(), "tc_master"))

