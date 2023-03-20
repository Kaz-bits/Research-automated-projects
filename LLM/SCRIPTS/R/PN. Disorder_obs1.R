# Paqueterías
library(ggplot2)

# Funciones
bins <- function(x) {
  if ((2 * IQR(x, na.rm = TRUE) / length(x)^(1/3)) > 0.05) {
    round(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3), digits = 2)
  } else {
    round(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3), digits = 2)
  }
}

# Cargar archivo de metapredict
df_meta <- read.csv(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/METAPREDICT/library_disor_META.csv", 
                    header = FALSE)[-196, ]

# Cargar datos de delta FRET
df_delta <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
                       header = TRUE, sep = "\t")

# Vector vacío
temp1 <- c()

# Calcular porcentaje de desorden de las secuencias
for (a in 1:nrow(df_meta)) {
  # Convertir datos de metapredict a vector
  temp <- unname(unlist(df_meta[a, 2:ncol(df_meta)]))
  
  # Remover los NA
  temp <- temp[!is.na(temp)]
  
  # Determinar residuos desordenados y ordenados
  temp <- ifelse(temp > 0.5, "D", "O")
  
  # Calcular la cantidad de residuos desordenados
  disor <- table(temp)[[1]]
  
  # Calcular el porcentaje de desorden
  percent <- (disor * 100) / (length(temp))
  
  # Guardar valores
  temp1[length(temp1) + 1] <- percent

}

# Redondear valores a dos cifras
temp1 <- round(temp1, 2)

# Crear un data frame para almacenar los datos
df_meta1 <- data.frame("ID" = df_meta$V1, "disor_perc" = temp1)


# Determinar la cantidad de biosensores de respuesta
# alta, media y baja

# Biosensores de respuesta alta, baja e intermedia 
# de acuerdo con los cuartiles
temp_25 <- unname(quantile(df_delta$mean_delta)[2]) # 25%
temp_75 <- unname(quantile(df_delta$mean_delta)[4]) # 75%

# Cuartiles de 25, 50 y 75%
temp_75_delta <- df_delta[df_delta$mean_delta > temp_75, ] # Alta
temp_25_delta <- df_delta[df_delta$mean_delta < temp_25, ] # Baja
temp_50_delta <- df_delta[df_delta$mean_delta <= temp_75 &
                                     df_delta$mean_delta >= temp_25, ]

# Agregar columnas de respuesta a cada data frame
temp_25_delta$Response <- "Baja"
temp_50_delta$Response <- "Media"
temp_75_delta$Response <- "Alta"

# Juntar dataframes anteriores
df_delta <- rbind(temp_25_delta, temp_50_delta, temp_75_delta)

# Ordenar datos de df_delta
df_delta <- df_delta[order(df_delta$construct, decreasing = FALSE), ]

# Añadir a df_meta1 la columna "Response" de df_delta
df_meta1$Response <- df_delta$Response

# Añadir a df_meta1 la columna "mean_delta" de df_delta
df_meta1$mean_delta <- df_delta$mean_delta

# Cambiar orden de la variable categórica
df_meta1$Response <- factor(df_meta1$Response, c("Alta", "Media", "Baja"))

# Prueba t-student para grupos
# Genera grupos
a <- df_meta1[df_meta1$Response == "Alta", ]$mean_delta
b <- df_meta1[df_meta1$Response == "Media", ]$mean_delta
c <- df_meta1[df_meta1$Response == "Baja", ]$mean_delta

# Nivel de significancia
signif_level <- c(ns = 1, "*" = 0.05, "**" = 0.01, 
                  "***" = 0.001, "****" = 0.0001)

# Realizar comparaciones
p1 <- t.test(a, b)[[3]] # alta vs media
p2 <- t.test(a, c)[[3]] # alta vs baja
p3 <- t.test(b, c)[[3]] # media vs baja

# Juntar los valores de "p" de cada condición
a <- c(p1, p2, p3)

# Evaluar el valor de p de las concentraciones
# Para 0 vs 200
i <- c(a[1] > 0.05, a[1] <= 0.05, a[1] <= 0.01, a[1] <= 0.001, a[1] <= 0.0001)
# Para 0 vs 400
h <- c(a[2] > 0.05, a[2] <= 0.05, a[2] <= 0.01, a[2] <= 0.001, a[2] <= 0.0001)
# Para 0 vs 600
j <- c(a[3] > 0.05, a[3] <= 0.05, a[3] <= 0.01, a[3] <= 0.001, a[3] <= 0.0001)

#Extraer los simbolos de significancia del valor de probabilidad anterior
p1_box <- (names(signif_level[i]))[length((names(signif_level[i])))] #p1
p2_box <- (names(signif_level[h]))[length((names(signif_level[h])))] #p2
p3_box <- (names(signif_level[j]))[length((names(signif_level[j])))] #p3


# Boxplot con valores de desorden y respuesta de delta FRET
ggplot() +
  geom_boxplot(data = df_meta1, aes(x = Response, y = mean_delta, 
                                   group = Response, fill = Response), show.legend = FALSE) +
  theme_classic() +
  labs(x = NULL, y = expression(Delta*"FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 2)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF")) +
  annotate(geom = "text", x = 1, y = 2, label = p1_box, size = 5) +
  annotate(geom = "text", x = 2, y = 0.7, label = p2_box, size = 5)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/boxplot_delta_fret.pdf", 
       device = "pdf", width = 3, height = 4, units = "in", dpi = 400)

ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/boxplot_delta_fret.png", 
       device = "png", width = 3, height = 4, units = "in", dpi = 400)


# Generar strip chart con datos de nivel de desorden
ggplot() +
  geom_jitter(data = df_meta1, aes(x = Response, y = disor_perc, 
                                   color = Response), show.legend = FALSE) +
  theme_classic() +
  labs(x = NULL, y = "Desorden (%)") +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  scale_color_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))


# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/stripchart_delta_fret.pdf", 
       device = "pdf", width = 3, height = 4, units = "in", dpi = 400)

ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/stripchart_delta_fret.png", 
       device = "png", width = 3, height = 4, units = "in", dpi = 400)


# Gráfico de correlación delta FRET vs Desorden
bins(df_meta1$mean_delta)
bins(df_meta1$disor_perc)

# Graficar histograma en 2D
ggplot(data = df_meta1) +
  geom_density_2d_filled(aes(x = disor_perc, y = mean_delta), 
                  bins = 10.28, show.legend = FALSE) +
  theme_bw() +
  labs(x = "Desorden (%)", y = expression(Delta*"FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(option = "A")

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/density_2d_disor_fret.pdf", 
       device = "pdf", width = 4, height = 4, units = "in", dpi = 400)

ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/density_2d_disor_fret.png", 
       device = "png", width = 4, height = 4, units = "in", dpi = 400)

