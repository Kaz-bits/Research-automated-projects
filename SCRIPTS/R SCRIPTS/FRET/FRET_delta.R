# Paqueterías
library(ggplot2)
library(ggpubr)

# Función para obtener el delta FRET
FRET.delta <- function(dir.bios, format.plot = "pdf") {
  
  # Obtener el nombre de cada biosensor
  temp_files <- list.files(dir.bios)
  
  # Crear vector vacío para guardar datos
  temp_mean <- c()
  temp_sd <- c()
  temp_names <- c()
  
  # Analizar cada biosensor
  for (bios in temp_files) {
    
    # Obtener el directorio del biosensor
    temp_path <- file.path(dir.bios, bios, "DATA", paste0(bios, ".csv"))
    
    # Verificar si existe el archivo del biosensor
    if (file.exists(temp_path) == TRUE) {
      
      # Nombres de biosensores presentes
      temp_names[length(temp_names) + 1] <- bios 
      
      # Cargar archivo
      fret_ratio <- read.csv(file = file.path(dir.bios, bios, "DATA", paste0(bios, ".csv")), 
                             header = TRUE)
      
      # Obtener datos a 0 mM
      i <- fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1, ]$Normalized
      
      # Obtener datos a 1500 mM
      h <- fret_ratio[fret_ratio$Treatment == 1500, ]$Normalized
      
      # Obtener la media 
      temp_mean[length(temp_mean) + 1] <- mean(h - i)
      
      # Obtener la desviación estándar 
      temp_sd[length(temp_sd) + 1] <- sd(h - i)
      
    }
    
  }
  
  # Construir dataframe para guardar datos 
  fret_delta <- data.frame(matrix(nrow = length(temp_names), 
                                  ncol = 3))
  
  # Agregar nombres a las columnas
  names(fret_delta)[1] <- "construct"
  names(fret_delta)[2] <- "mean_delta"
  names(fret_delta)[3] <- "sd_delta"
  
  
  # Agregar columna de media
  fret_delta$mean_delta <- temp_mean
  
  # Agregar columna de desviación estándar
  fret_delta$sd_delta <- temp_sd
  
  # Agregar columna con los nombres
  fret_delta$construct <- temp_names
  
  # Guardar archivo con los datos de delta FRET
  write.csv(x = fret_delta, 
            file = file.path(dir.bios, paste0("all_biosensors.csv")), 
            quote = FALSE, 
            row.names = FALSE)
  
  
  # Construir gráfico de puntos
  plot <- ggplot() +
    geom_vline(xintercept = unname(quantile(fret_delta$mean_delta)[2]), 
               lty = 2) +
    geom_vline(xintercept = median(fret_delta$mean_delta), 
               lty = 2, color = "blue") +
    geom_vline(xintercept = unname(quantile(fret_delta$mean_delta)[4]), 
               lty = 2) +
    geom_point(data = fret_delta, aes(x = mean_delta, 
                                      y = as.factor(construct)), 
               size = 4, shape = 21, 
               fill = "white", stroke = 1.5) +
    geom_errorbar(data = fret_delta, aes(x = mean_delta, 
                                         y = as.factor(construct), 
                                         xmin = mean_delta - sd_delta, 
                                         xmax = mean_delta + sd_delta), width = 0.2) +
    labs(x = expression(Delta * "FRET"), 
         y = "IDR") +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid = element_blank())  + 
    coord_cartesian(xlim = c(0, 0.5))
  
  # Guardar gráfico 
  ggsave(plot = plot, 
         filename = file.path(dir.bios, paste0("all_biosensors.", format.plot)), 
         device = format.plot, 
         width = 5, height = 4, units = "in", dpi = 450)
  
}


