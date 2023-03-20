# Paqueterías
library(ggplot2)
library(ggpubr)

# Cargar archivo de secuencias
df <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/DATA/BASES DE DATOS/IDRBS_Library_200.txt", 
                 header = FALSE)

# Cargar datos de delta FRET
df_delta <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
                       header = TRUE, sep = "\t")

# Obtener los IDs
temp <- substr(df[seq(1, nrow(df), 2), ], 2, 10)

# Obtener las secuencias
temp1 <- df[seq(2, nrow(df), 2), ]

# Guardarlas en un nuevo data frame
df <- data.frame("ID" = temp, "Secuencia" = temp1)

# Añadir mean_delta a "df"
df$mean_delta <- df_delta$mean_delta


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


# Obtener la frecuencia de todos los aminoácidos----
# Vectores vacíos
temp <- c()
temp1 <- c()

# Data frame
temp_df <- data.frame(matrix(ncol = 5, nrow = 0))
names(temp_df) <- c("Num", "Seq", "ID", "man_delta", "Response")

# Juntar los datos de residuos de aminoácidos
for (a in 1:nrow(df)) {
  
  # Extraer la secuencia
  seq <- unlist(strsplit(df[a, 2], split = ""))
  
  # Obtener cantidad de residuos
  num <-  seq(1, length(seq))
  
  # Añadir elementos al data frame
  df1 <- data.frame("Num" = num, 
                    "Seq" = seq,
                    "ID" = df[a, 1], 
                    "mean_delta" = df[a, 3],
                    "Response" = df_delta[a, 4])
  
  # Añadir elementos a un data frame
  temp_df <- rbind(temp_df, df1)
  
}

# Obtener lista de biosensores
temp_list <- df$ID

# Data frame
temp_df1 <- data.frame(matrix(ncol = 4, nrow = 0))
names(temp_df1) <- c("Seq", "Freq", "ID", "Response")


for (i in temp_list) {
  
  # Obtener las frecuencias de aminoácidos
  temp_df_aa <- as.data.frame(table(temp_df[temp_df$ID == i, 2]))
  
  # Añadir columna con ID
  temp_df_aa$ID <- i
  
  # Obtener respuesta del biosensor
  temp_df_aa$Response <- temp_df[temp_df$ID == i, 5][1]
  
  # Cambiar nombres a las columnas
  names(temp_df_aa) <- c("Seq", "Freq", "ID", "Response")
  
  # Juntar data frames
  temp_df1 <- rbind(temp_df1, temp_df_aa)
  
}














# Datos para el IDRBS-163
# Extraer la secuencia
i <- unlist(strsplit(df[163, 2], split = ""))

# Añadir elementos al data frame
df1 <- data.frame("Num" = seq(1, length(i)), "Seq" = i)

ggplot(data = df1, aes(x = Num, y = Seq)) +
  geom_tile(fill = "black", size = 0.6, 
            height = 1, width = 0.3) +
  theme_linedraw() +
  ggtitle("IDRBS-163") +
  theme(axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/tiles_aa_163.pdf",
       device = "pdf", width = 4, height = 4, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/tiles_aa_163.png",
       device = "png", width = 4, height = 4, units = "in", dpi = 400)



# Datos para el IDRBS-091
# Extraer la secuencia
i <- unlist(strsplit(df[91, 2], split = ""))

# Añadir elementos al data frame
df1 <- data.frame("Num" = seq(1, length(i)), "Seq" = i)

ggplot(data = df1, aes(x = Num, y = Seq)) +
  geom_tile(fill = "black", size = 0.6, 
            height = 1, width = 0.3) +
  theme_linedraw() +
  ggtitle("IDRBS-091") +
  theme(axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/tiles_aa_091.pdf",
       device = "pdf", width = 4, height = 4, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/tiles_aa_091.png",
       device = "png", width = 4, height = 4, units = "in", dpi = 400)



# Heat map de frecuencia de aminoácidos de la biblioteca----
ggplot(data = temp_df1, aes(x = Seq, y = ID, fill = Freq)) +
  geom_tile() +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(name = NULL,
                      low = "#EFFF96", high = "#5E59FF",
                      breaks = c(0, 20, 40, 60, 80), 
                      labels = c("0", "20","40", "60","80"),
                      limits = c(0, 80)) 

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/heatmap_dist_aa_200.pdf",
       device = "pdf", width = 8, height = 40, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/heatmap_dist_aa_200.png",
       device = "png", width = 8, height = 40, units = "in", dpi = 400)





# Lista de biosensores
temp_list <- df$ID[c(163, 181, 193, 154, 23, 184, 144, 
                     162, 12, 188, 40, 145, 161,
                     76, 178, 3, 67, 112, 45, 91)]
# Data frame vacío
temp <- data.frame(matrix(ncol = 4, nrow = 0))
names(temp) <- c("Num", "Seq", "ID", "Response")

# Obtener 20 biosensores
for (i in temp_list) {
  
  # Mostrar solamente 20 biosensores (respuesta alta, media y baja)
  temp1 <- temp_df1[temp_df1$ID == i, ]
  
  # Juntar data frames
  temp <- rbind(temp, temp1)
    
}

# Heat map de frecuencia de aminoácidos
ggplot(data = temp, aes(x = Seq, y = ID, fill = Freq)) +
  geom_tile(aes(fill = as.numeric(Freq))) +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(guide = "legend", name = NULL,
                      low = "#FFFFFF", high = "#F100FC", 
                      breaks = c(0, 20, 40, 60, 80),
                      limits = c(0, 80)) 

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/heatmap_dist_aa_20_hlm.pdf",
       device = "pdf", width = 7, height = 6, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/heatmap_dist_aa_20_hlm.png",
       device = "png", width = 7, height = 6, units = "in", dpi = 400)




# Biosensores de respuesta alta----
# Heat map de frecuencia de aminoácidos
ggplot(data = temp_df1[temp_df1$Response == "Alta", ], 
       aes(x = Seq, y = ID, fill = Freq)) +
  geom_tile() +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(name = NULL,
                      low = "#EFFF96", high = "#5E59FF",
                      breaks = c(0, 20, 40, 60, 80), 
                      labels = c("0", "20","40", "60","80"),
                      limits = c(0, 80)) 

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/heatmap_dist_aa_high.pdf",
       device = "pdf", width = 8, height = 25, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/heatmap_dist_aa_high.png",
       device = "png", width = 8, height = 25, units = "in", dpi = 400)


# Biosensores de respuesta media----
# Heat map de frecuencia de aminoácidos
ggplot(data = temp_df1[temp_df1$Response == "Media", ], 
       aes(x = Seq, y = ID, fill = Freq)) +
  geom_tile() +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(name = NULL,
                      low = "#EFFF96", high = "#5E59FF",
                      breaks = c(0, 20, 40, 60, 80), 
                      labels = c("0", "20","40", "60","80"),
                      limits = c(0, 80)) 

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/heatmap_dist_aa_medium.pdf",
       device = "pdf", width = 8, height = 25, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/heatmap_dist_aa_medium.png",
       device = "png", width = 8, height = 25, units = "in", dpi = 400)


# Biosensores de respuesta baja----
# Heat map de frecuencia de aminoácidos
ggplot(data = temp_df1[temp_df1$Response == "Baja", ], 
       aes(x = Seq, y = ID, fill = Freq)) +
  geom_tile() +
  theme_bw() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(name = NULL,
                      low = "#EFFF96", high = "#5E59FF",
                      breaks = c(0, 20, 40, 60, 80), 
                      labels = c("0", "20","40", "60","80"),
                      limits = c(0, 80)) 

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/heatmap_dist_aa_low.pdf",
       device = "pdf", width = 8, height = 25, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/heatmap_dist_aa_low.png",
       device = "png", width = 8, height = 25, units = "in", dpi = 400)










