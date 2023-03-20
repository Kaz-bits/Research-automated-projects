# Paqueterías
library(ggplot2)
library(ggpubr)

# Cargar archivo de DELTA FRET
df_fret_200_delta <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/DATA/BASES DE DATOS/delta_seq_parrot.tsv", 
                                header = TRUE, sep = "\t")

# Determinar la cantidad de biosensores de respuesta
# alta, media y baja

# Biosensores de respuesta alta, baja e intermedia 
# de acuerdo con los cuartiles
temp_25 <- unname(quantile(df_fret_200_delta$mean_delta)[2]) # 25%
temp_75 <- unname(quantile(df_fret_200_delta$mean_delta)[4]) # 75%

# Cuartiles de 25, 50 y 75%
temp_75_delta <- df_fret_200_delta[df_fret_200_delta$mean_delta > temp_75, ] # Alta
temp_25_delta <- df_fret_200_delta[df_fret_200_delta$mean_delta < temp_25, ] # Baja
temp_50_delta <- df_fret_200_delta[df_fret_200_delta$mean_delta <= temp_75 &
                                     df_fret_200_delta$mean_delta >= temp_25, ]

# Agregar columnas de respuesta a cada data frame
temp_25_delta$Response <- "Low"
temp_50_delta$Response <- "Medium"
temp_75_delta$Response <- "High"

# Juntar dataframes anteriores
df_fret_200_delta <- rbind(temp_25_delta, temp_50_delta, temp_75_delta)

# Obtener lista de biosensores
temp_list <- list.files("/media/kaz-bits/TOSHIBA EXT/NCPR/NCPR_Constanza/")

# Extraer los biosensores de respuesta alta
temp_list_high <- temp_list[temp_75_delta$construct]

# Extraer los biosensores de respuesta media
temp_list_med <- temp_list[temp_50_delta$construct]

# Extraer los biosensores de respuesta baja
temp_list_low <- temp_list[temp_25_delta$construct]


# Data frame vacío
ncpr <- data.frame(matrix(nrow = 1, ncol = 6))

# Juntar los datos de biosensores de respuesta alta
for (i in temp_list_high) {
  
  # Cargar archivo
  temp <- read.csv(file = file.path("/media/kaz-bits/TOSHIBA EXT/NCPR/NCPR_Constanza", i))
  
  # Colocar nombres en ncpr
  names(ncpr) <- names(temp)
  
  # Juntar datos
  ncpr <- rbind(ncpr, temp)
  
}

# Eliminar primer renglón
ncpr <- ncpr[-1, ]

# Calcular la cantidad de residuos positivos
pos <- length(ncpr[ncpr$Charge == "Positivo", ]$Charge)

# Calcular la cantidad de residuos negativos
neg <- length(ncpr[ncpr$Charge == "Negativo", ]$Charge)

# Calcular la cantidad de residuos no cargados
noch <- length(ncpr[ncpr$Charge == "No charge", ]$Charge)

# Juntar vectores
a <- c(pos, neg, noch)

# Gráfico con cantida de cargas
high <- ggplot(data = ncpr) +
  geom_bar(aes(x = Charge, fill = Charge), 
           color = "black", size = 0.8, width = 0.5,
           show.legend = FALSE) +
  theme_classic() +
  labs(x = NULL, y = "Cantidad") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(a), 550), 
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("#FF6464", "#CFCFCF", "#3771FF"))


# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/charge_dist_high.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/charge_dist_high.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)



# Data frame vacío
ncpr <- data.frame(matrix(nrow = 1, ncol = 6))

# Juntar los datos de biosensores de respuesta alta
for (i in temp_list_med) {
  
  # Cargar archivo
  temp <- read.csv(file = file.path("/media/kaz-bits/TOSHIBA EXT/NCPR/NCPR_Constanza", i))
  
  # Colocar nombres en ncpr
  names(ncpr) <- names(temp)
  
  # Juntar datos
  ncpr <- rbind(ncpr, temp)
  
}

# Eliminar primer renglón
ncpr <- ncpr[-1, ]

# Calcular la cantidad de residuos positivos
pos <- length(ncpr[ncpr$Charge == "Positivo", ]$Charge)

# Calcular la cantidad de residuos negativos
neg <- length(ncpr[ncpr$Charge == "Negativo", ]$Charge)

# Calcular la cantidad de residuos no cargados
noch <- length(ncpr[ncpr$Charge == "No charge", ]$Charge)

# Juntar vectores
a <- c(pos, neg, noch)

# Gráfico con cantida de cargas
med <- ggplot(data = ncpr) +
  geom_bar(aes(x = Charge, fill = Charge), 
           color = "black", size = 0.8, width = 0.5,
           show.legend = FALSE) +
  theme_classic() +
  labs(x = NULL, y = "Cantidad") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank()) +
  scale_y_continuous(breaks = seq(0, max(a), 550), 
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("#FF6464", "#CFCFCF", "#3771FF"))


# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/charge_dist_low.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/charge_dist_low.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)




# Data frame vacío
ncpr <- data.frame(matrix(nrow = 1, ncol = 6))

# Juntar los datos de biosensores de respuesta alta
for (i in temp_list_low) {
  
  # Cargar archivo
  temp <- read.csv(file = file.path("/media/kaz-bits/TOSHIBA EXT/NCPR/NCPR_Constanza", i))
  
  # Colocar nombres en ncpr
  names(ncpr) <- names(temp)
  
  # Juntar datos
  ncpr <- rbind(ncpr, temp)
  
}

# Eliminar primer renglón
ncpr <- ncpr[-1, ]

# Calcular la cantidad de residuos positivos
pos <- length(ncpr[ncpr$Charge == "Positivo", ]$Charge)

# Calcular la cantidad de residuos negativos
neg <- length(ncpr[ncpr$Charge == "Negativo", ]$Charge)

# Calcular la cantidad de residuos no cargados
noch <- length(ncpr[ncpr$Charge == "No charge", ]$Charge)

# Juntar vectores
a <- c(pos, neg, noch)

# Gráfico con cantida de cargas
low <- ggplot(data = ncpr) +
  geom_bar(aes(x = Charge, fill = Charge), 
           color = "black", size = 0.8, width = 0.5,
           show.legend = FALSE) +
  theme_classic() +
  labs(x = NULL, y = "Cantidad") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank()) +
  scale_y_continuous(breaks = seq(0, max(a), 300), 
                     expand = c(0, 0)) +
  scale_fill_manual(values = c("#FF6464", "#CFCFCF", "#3771FF"))


# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/charge_dist_low.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/charge_dist_low.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)



# Juntar gráficos
ggarrange(high, med, low, ncol = 3, nrow = 1, 
          labels = c("A", "B", "C"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/library_ncpr_dist.pdf",
       device = "pdf", width = 7, height = 3.5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/library_ncpr_dist.png",
       device = "png", width = 7, height = 3.5, units = "in", dpi = 400)


