# Paqueterías
library(ggplot2)
library(ggpubr)

# Cargar archivo de DELTA FRET
df_fret_200_delta <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
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

# Proporción de aminoácidos de respuesta alta
ggplot(data = ncpr, aes(x = Res, y = ..count../sum(..count..))) +
  geom_bar(show.legend = FALSE, fill = "#00A9CC", color = "black",
           alpha = 0.8, size = 0.8) +
  theme_classic() +
  labs(x = "Residuo", y = "Proporción") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_y_continuous(expand = c(0, 0))
