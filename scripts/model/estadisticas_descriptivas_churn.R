#' Starting - Primero leer main.R hasta set_environment()
#' Este script crea las graficas de estadisticas descriptivas del modelo de churn,
#' estas se guardan automaticamente en la carpeta señalada en @param plots_path como
#' churn_YYYMMDD al dia actual

#Descargar librerias
library(Matrix)
library(rsample)
library(breakDown)
library(cowplot)
library(scales)
library(MASS)
library(stringi)


#Paths para leer data
master_path <- "data/master/train"
plots_path <- "documentos/descriptive_plots"


# Nombres carpeta de plots
print("Making plot's folder")

plot_alias <-
  paste0("churn_", today() %>% format(., "%Y%m%d"))

plots_path <-
  os.path.join(plots_path, plot_alias)
dir.create(plots_path)


# Cargar data de master
master_files <- list.files(master_path, full.names = T)
datos_list <- list()
for (i in 1:length(master_files)) {
  print(paste("Loading datos from", master_files[i]))
  datos_list[[i]] <- readRDS(master_files[i])
}

datos <- rbindlist(datos_list, use.names = T, fill = T)
rm(datos_list)
gc()

#Creacion variable target (dummy)
datos[, target := ifelse(tipo_cancelacion_futuro == "Sin Cancelacion", 0, 1)]
datos[, tipo_cancelacion_futuro := NULL]


####Target plots####
# Porcentaje del target
name_f <- "perc_target"
p <- datos %>%
  dplyr::group_by(target) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::mutate(percent = prop.table(Count) * 100) %>%
  ggplot(aes(reorder(target,-percent), percent), fill = target) +
  geom_col(fill = c("#00AFBB", "#E7B800")) +
  geom_text(aes(label = paste0(
    sprintf("%.2f%%", percent), "\n", comma(Count)
  )),
  hjust = 0.5,
  vjust = 0.5,
  size = 3) +
  theme_bw() +
  labs(
    caption = "Nota: Porcentaje arriba, número total de canceladas abajo",
    x = "Meses",
    y = "Proporción",
    title = "Porcentaje agregado de tarjeta de crédito  \n con cancelación voluntaria a dos meses"
  )
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Porcentaje de churn por mes
name_f <- "per_target_mes"
plot_base <- datos %>%
  dplyr::group_by(periodo, target) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::mutate(percent = prop.table(Count) * 100) %>% data.table()

p <-
  ggplot(plot_base[target == 1], aes(periodo, percent), fill = periodo) +
  geom_col(fill = "#00AFBB") +
  geom_text(aes(label = paste0(
    sprintf("%.2f%%", percent), "\n", comma(Count)
  )),
  hjust = 0.5,
  vjust = 1,
  size = 3) +
  theme_bw() +
  labs(
    caption = "Nota: Porcentaje arriba, número total de canceladas abajo",
    x = "Meses",
    y = "Proporción",
    title = "Porcentaje agregado de tarjeta de crédito  \n con cancelación voluntaria a dos meses por mes"
  )

ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#####Variables categoricas####
#Subproducto
name_f <- "subproducto"

base <- datos[, .(count = .N), by = .(target, sub_producto)]
base[, total := sum(count), by = .(target)]
base[, weight := count / total]

p <-
  base[target == 1][order(sub_producto)] %>% ggplot(aes(x = sub_producto, y = weight)) +
  geom_col(aes(fill = sub_producto), position = "dodge")  +
  geom_text(aes(label = paste0(
    sprintf("%.2f%%", weight), "\n", comma(count)
  )), size = 3, vjust = 0) +
  labs(
    caption = "Nota: Porcentaje arriba, número total de canceladas abajo",
    title = "Porcentaje de tarjetas canceladas voluntariamente por subproducto",
    x = "Subproducto",
    y = "Proporción"
  ) +
  theme_bw() +
  scale_x_discrete(element_blank()) +
  scale_fill_discrete(guide = guide_legend(title = "Subproducto")) +
  theme(axis.text.y = element_text(size = 6.5))
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Estrato
name_f <- "estrato"

base <- datos[, .(count = .N), by = .(target, crm_estrato)]
base[, total := sum(count), by = .(target)]
base[, weight := count / total]

p <-
  base[target == 1][order(crm_estrato)] %>% ggplot(aes(x = crm_estrato, y = weight)) +
  geom_col(aes(fill = crm_estrato), position = "dodge")  +
  geom_text(aes(label = paste0(
    sprintf("%.2f%%", weight), "\n", comma(count)
  )), size = 3, vjust = 0) +
  labs(
    caption = "Nota: Porcentaje arriba, número total de canceladas abajo",
    title = "Porcentaje de tarjetas canceladas voluntariamente por estrato",
    x = "Estrato",
    y = "Proporción"
  ) +
  theme_bw() +
  scale_x_discrete(element_blank()) +
  scale_fill_discrete(guide = guide_legend(title = "Estrato")) +
  theme(axis.text.y = element_text(size = 6.5))
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Sexo
name_f <- "sexo"

base <- datos[, .(count = .N), by = .(target, crm_genero)]
base[, total := sum(count), by = .(target)]
base[, weight := count / total]

p <-
  base[target == 1][order(crm_genero)] %>% ggplot(aes(x = crm_genero, y = weight)) +
  geom_col(aes(fill = crm_genero), position = "dodge")  +
  geom_text(aes(label = paste0(
    sprintf("%.2f%%", weight), "\n", comma(count)
  )), size = 3, vjust = 0) +
  labs(
    caption = "Nota: Porcentaje arriba, número total de canceladas abajo",
    title = "Porcentaje de tarjetas canceladas voluntariamente por sexo",
    x = "Sexo",
    y = "Proporción"
  ) +
  theme_bw() +
  scale_x_discrete(element_blank()) +
  scale_fill_discrete(guide = guide_legend(title = "Sexo")) +
  theme(axis.text.y = element_text(size = 6.5))
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Segmento
name_f <- "segmento"

base <- datos[, .(count = .N), by = .(target, crm_segmento)]
base[, total := sum(count), by = .(target)]
base[, weight := count / total]

p <-
  base[target == 1][order(crm_segmento)] %>% ggplot(aes(x = crm_segmento, y = weight)) +
  geom_col(aes(fill = crm_segmento), position = "dodge")  +
  geom_text(aes(label = paste0(
    sprintf("%.2f%%", weight), "\n", comma(count)
  )), size = 3, vjust = 0) +
  labs(
    caption = "Nota: Porcentaje arriba, número total de canceladas abajo",
    title = "Porcentaje de tarjetas canceladas voluntariamente por segmento",
    x = "Segmento",
    y = "Proporción"
  ) +
  theme_bw() +
  scale_x_discrete(element_blank()) +
  scale_fill_discrete(guide = guide_legend(title = "Segmento")) +
  theme(axis.text.y = element_text(size = 6.5))
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))



####Variables continuas####
#Antiguedad de la tarjeta
name_f <- "antig_tc"
medianas <-
  datos[, .(antiguedad = round(median(antiguedad/12), digits = 0)), by = target]

p <- ggplot(datos, aes(x = factor(target), y = antiguedad/12)) +
  geom_boxplot(aes(fill = factor(target))) +
  scale_fill_discrete(guide = guide_legend(title = "Target")) +
  ylim(0, 20) +
  labs(
    caption = "Nota: Con límites de antiguedad de 0 a 20",
    title = "Porcentaje de tarjetas canceladas voluntariamente \n por antiguedad de la tarjeta",
    x = "Target",
    y = "Antiguedad (años)"
  ) +
  theme_bw() +
  geom_text(
    data = medianas,
    aes(label = antiguedad),
    position = position_dodge(width = 1),
    vjust = -3.5
  )
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Cupo
name_f <- "cupo"
medianas <-
  datos[, .(cupo = round(median(cupo), digits = 0)), by = target]

p <- ggplot(datos, aes(x = factor(target), y = cupo)) +
  geom_boxplot(aes(fill = factor(target))) +
  # scale_y_continuous(labels = comma)+
  scale_fill_discrete(guide = guide_legend(title = "Target")) +
  ylim(0, 7000000) +
  labs(
    caption = "Nota: Con límites de cupo de 0 a 7.000.000",
    title = "Porcentaje de tarjetas canceladas voluntariamente por cupo",
    x = "Target",
    y = "Cupo"
  ) +
  theme_bw() +
  geom_text(
    data = medianas,
    aes(label = comma(cupo)),
    position = position_dodge(width = 1),
    vjust = -0.1
  )
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Edad
name_f <- "edad"
medianas <-
  datos[, .(crm_edad = round(median(crm_edad), digits = 2)), by = target]

p <- ggplot(datos, aes(x = factor(target), y = crm_edad)) +
  geom_boxplot(aes(fill = factor(target))) +
  scale_fill_discrete(guide = guide_legend(title = "Target")) +
  labs(title = "Porcentaje de tarjetas canceladas voluntariamente por edad del cliente",
       x = "Target",
       y = "Edad (años)") +
  theme_bw() +
  geom_text(
    data = medianas,
    aes(label = comma(crm_edad)),
    position = position_dodge(width = 1),
    vjust = -0.1
  )
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Antiguedad del cliente
name_f <- "antig_cliente"
medianas <-
  datos[, .(crm_antiguedad = round(median(crm_antiguedad/12), digits = 0)), by = target]

p <- ggplot(datos, aes(x = factor(target), y = crm_antiguedad/12)) +
  geom_boxplot(aes(fill = factor(target))) +
  ylim(0, 50) +
  scale_fill_discrete(guide = guide_legend(title = "Target")) +
  labs(
    caption = "Nota: Con límites de antiguedad de 0 a 50",
    title = "Porcentaje de tarjetas canceladas voluntariamente por antiguedad del cliente",
    x = "Target",
    y = "Antiguedad (meses)"
  ) +
  theme_bw() +
  geom_text(
    data = medianas,
    aes(label = comma(crm_antiguedad)),
    position = position_dodge(width = 1),
    vjust = -4
  )
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Saldo capital total
name_f <- "saldo_capt_total"
medianas <-
  datos[, .(sdo_capt_total = round(median(sdo_capt_total), digits = 2)), by = target]

p <- ggplot(datos, aes(x = factor(target), y = sdo_capt_total)) +
  geom_boxplot(aes(fill = factor(target))) +
  ylim(0, 3000000) +
  scale_fill_discrete(guide = guide_legend(title = "Target")) +
  labs(caption="Nota: Con límites de saldo de 0 a 3'000.000",
    title = "Porcentaje de tarjetas canceladas voluntariamente por saldo capital total",
    x = "Target",
    y = "Saldo capital total") +
  theme_bw() +
  geom_text(
    data = medianas,
    aes(label = comma(sdo_capt_total)),
    position = position_dodge(width = 1),
    vjust = -0.1
  )
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))


#Pago minimo
name_f <- "pago_minimo"
medianas <-
  datos[, .(pago_minimo = round(median(pago_minimo), digits = 2)), by = target]

p <- ggplot(datos, aes(x = factor(target), y = pago_minimo)) +
  geom_boxplot(aes(fill = factor(target))) +
  ylim(0, 300000) +
  scale_fill_discrete(guide = guide_legend(title = "Target")) +
  labs(caption="Nota: Con límites de pago mínimo de 0 a 300.000",
       title = "Porcentaje de tarjetas canceladas voluntariamente por pago minimo",
       x = "Target",
       y = "Pago minimo") +
  theme_bw() +
  geom_text(
    data = medianas,
    aes(label = comma(pago_minimo)),
    position = position_dodge(width = 1),
    vjust = -0.1
  )
ggsave(plot = p,
       file = os.path.join(plots_path, paste0(name_f, ".png")))

