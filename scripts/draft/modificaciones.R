

datos$trx[is.na(datos$trx)] <- 0
datos$vlr_pro[is.na(datos$vlr_pro)] <- 0
datos$vlr_mode[is.na(datos$vlr_mode)] <- 0
datos$vlr_median[is.na(datos$vlr_median)] <- 0
datos$cuotas_pro[is.na(datos$cuotas_pro)] <- 0
datos$cuotas_mode[is.na(datos$cuotas_mode)] <- 0
datos$cuotas_median[is.na(datos$cuotas_median)] <- 0
print(nrow(datos))


