# Paqueterías
library(ggplot2)
library(ggpubr)

# Cargar archivo de metapredict
df_meta <- read.csv(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/METAPREDICT/library_plddt_META.csv", 
                    header = FALSE)[-196, ]

# Cargar datos de delta FRET
df_delta <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
                       header = TRUE, sep = "\t")

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


# Ordenar datos de AlphaFold
# Vector vacío
temp1 <- c()

# Crear data frame
df <- data.frame(matrix(ncol = 4, nrow = 0))

# Calcular porcentaje de plddt de las secuencias
for (a in 1:nrow(df_meta)) {
  # Convertir datos de metapredict a vector
  temp <- unname(unlist(df_meta[a, 2:ncol(df_meta)]))
  
  # Remover los NA
  temp <- temp[!is.na(temp)]
  
  # Generar 4 vectores vacíos
  temp_plddt1 <- c()
  temp_plddt2 <- c()
  temp_plddt3 <- c()
  temp_plddt4 <- c()
  
  # Obtener porcentaje de pLDDT
  for (plddt in temp) {
    
    # pLDDT mayor a 90
    if (plddt > 90) {
      temp_plddt1[length(temp_plddt1) + 1] <- plddt
    }
    
    # pLDDT menor a 90 y mayor e igual a 70
    if (plddt >= 70 && plddt < 90) {
      temp_plddt2[length(temp_plddt2) + 1] <- plddt
    }
    
    # pLDDT menor a 70 y mayor e igual a 50
    if (plddt >= 50 && plddt < 70) {
      temp_plddt3[length(temp_plddt3) + 1] <- plddt
    }
    
    # pLDDT menor a 50
    if (plddt < 50) {
      temp_plddt4[length(temp_plddt4) + 1] <- plddt
    }
  }
  
  # Obtener porcentajes
  i <- round((length(temp_plddt1) * 100) / (length(temp)), 2)
  h <- round((length(temp_plddt2) * 100) / (length(temp)), 2)
  j <- round((length(temp_plddt3) * 100) / (length(temp)), 2)
  k <- round((length(temp_plddt4) * 100) / (length(temp)), 2)
  
  # Juntar porcentajes en orden
  vec <- c(i, h, j, k)
  
  # Agregar datos a un data frame
  df[nrow(df) + 1, ] <- vec
  
}


# Renombrar columnas del data frame
names(df) <- c("plddt1", "plddt2", "plddt3", "plddt4")

# Agregar identificador
df$ID <- df_meta$V1

# Cambiar orden de las columnas
df <- df[, c(5, 1:4)]

# Añadir columna Response de "df_delta" a "df"
df$Response <- df_delta$Response

# Añadir columna mean_delta de "df_delta" a "df"
df$mean_delta <- df_delta$mean_delta

# Guardar datos
write.csv(x = df, file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/library_alphafold.csv", 
          quote = FALSE, row.names = FALSE)


# Crear data frames individuales
# Muy confiable
df1 <- df[, c(1, 2, 6, 7)]
df1$Confidence <- "Muy alto"
names(df1)[2] <- "plddt"

# Confiable
df2 <- df[, c(1, 3, 6, 7)]
df2$Confidence <- "Alto"
names(df2)[2] <- "plddt"

# Poco confiable
df3 <- df[, c(1, 4, 6, 7)]
df3$Confidence <- "Bajo"
names(df3)[2] <- "plddt"

# Muy confiable
df4 <- df[, c(1, 5, 6, 7)]
df4$Confidence <- "Muy bajo"
names(df4)[2] <- "plddt"

# Juntar data frames
df <- rbind(df1, df2, df3, df4)

# Ordenar factores
df$Confidence <- factor(df$Confidence, levels = c("Muy alto", "Alto",
                                                  "Bajo", "Muy bajo"))

# Gráfico de distribución boxplot
p <- ggplot(data = df) + 
  geom_boxplot(aes(x = Response, y = plddt, fill = Response), 
              show.legend = FALSE, width = 0.5) +
  theme_classic() +
  labs(x = "Respuesta", y = "pLDDT (%)") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(plot = p, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/boxplot_alphafold.pdf", 
       device = "pdf", width = 3, height = 4, units = "in", dpi = 400)

ggsave(plot = p, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/boxplot_alphafold.png", 
       device = "png", width = 3, height = 4, units = "in", dpi = 400)



# Gráfico de distribución violinplot
p1 <- ggplot(data = df) + 
  geom_violin(aes(x = Response, y = plddt, fill = Response), 
               show.legend = FALSE, width = 1) +
  theme_classic() +
  labs(x = "Respuesta", y = "pLDDT (%)") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(plot = p1, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/violinplot_alphafold.pdf", 
       device = "pdf", width = 3, height = 4, units = "in", dpi = 400)

ggsave(plot = p1, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/violinplot_alphafold.png", 
       device = "png", width = 3, height = 4, units = "in", dpi = 400)



# Gráfico de distribución stripplot
p2 <- ggplot(data = df) + 
  geom_jitter(aes(x = Response, y = plddt, color = Response), 
              show.legend = FALSE) +
  theme_classic() +
  labs(x = "Respuesta", y = "pLDDT (%)") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(plot = p2, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/stripplot_alphafold.pdf", 
       device = "pdf", width = 3, height = 4, units = "in", dpi = 400)

ggsave(plot = p2, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/stripplot_alphafold.png", 
       device = "png", width = 3, height = 4, units = "in", dpi = 400)


# Acomodar datos
df$Response <- factor(df$Response, levels = c("Alta", "Media", "Baja"), 
                      labels = c("High", "Medium", "Low"))

# Juntar gráficos
p3 <- ggplot(data = df) + 
  geom_violin(aes(x = Response, y = plddt, fill = Response), 
              show.legend = FALSE, width = 1) +
  geom_boxplot(aes(x = Response, y = plddt), 
               show.legend = FALSE, width = 0.1) +
  theme_classic() +
  labs(x = "Respuesta", y = "pLDDT (%)") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(plot = p3, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/boxplot_violin_alphafold.pdf", 
       device = "pdf", width = 3.5, height = 4, units = "in", dpi = 400)

ggsave(plot = p3, filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/boxplot_violin_alphafold.png", 
       device = "png", width = 3.5, height = 4, units = "in", dpi = 400)

# Unir gráficos
ggarrange(p3, p2)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/boxplot_violin_strip_alphafold.pdf", 
       device = "pdf", width = 6, height = 4, units = "in", dpi = 400)

ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/boxplot_violin_strip_alphafold.png", 
       device = "png", width = 6, height = 4, units = "in", dpi = 400)


# Gráfico de correlación delta FRET vs Desorden
bins(df$mean_delta)
bins(df$plddt)

# Graficar histograma en 2D
ggplot(data = df) +
  geom_density_2d_filled(aes(x = plddt, y = mean_delta), 
                         bins = 8.95, show.legend = FALSE) +
  theme_bw() +
  labs(x = "pLDDT (%)", y = expression(Delta*"FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(option = "A")

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/density_2d_plddt_fret.pdf", 
       device = "pdf", width = 4, height = 4, units = "in", dpi = 400)

ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/density_2d_plddt_fret.png", 
       device = "png", width = 4, height = 4, units = "in", dpi = 400)


