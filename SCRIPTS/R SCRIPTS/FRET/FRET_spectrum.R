# Cargar paqeterías necesarias
library(ggplot2)
library(ggpubr)

# Función para generar espectros 
FRET.spectrum <- function(dir.sptr, format.plot = "pdf") {
  
  
  # Obtener el nombre de la carpeta de las réplicas
  temp_files <- list.files(dir.sptr)
  
  # Cargar los archivos 
  
  for (bios in temp_files) {
    
    # Obtener el nombre de cada biosensor 
    temp_files <- list.files(path = file.path(dir.sptr, bios, "DATA"), 
                             pattern = "plate")
    
    
    # Cargar datos de placa 1 y réplica 1
    # Verificar si existe el archivo
    temp_path <- file.path(dir.sptr, bios, "DATA", temp_files[1])
    if (file.exists(temp_path) == TRUE) {
      
      # Cargar archivo
      sptr_1 <- read.csv(file = file.path(dir.sptr, bios, "DATA", temp_files[1]),
                         header = TRUE)
      # Eliminar columnas para evitar repeticiones
      sptr_1$Replicate <- NULL
      sptr_1$Plate <- NULL
      sptr_1$Construct <- NULL
      
    } else {
      
      next
      
    }

    
    # Verificar si existe el archivo
    temp_path <- file.path(dir.sptr, bios, "DATA", temp_files[2])
    if (file.exists(temp_path) == TRUE) {
      
    # Cargar datos de placa 2 y réplica 1
    sptr_2 <- read.csv(file = temp_path,
                       header = TRUE)
    
    # Eliminar columnas para evitar repeticiones
    sptr_2$Wavelength..nm. <- NULL
    
    } else {
      
      next
      
    }
    
    
    # Verificar si existe el archivo
    temp_path <- file.path(dir.sptr, bios, "DATA", temp_files[3])
    if (file.exists(temp_path) == TRUE) {
      
    # Cargar datos de placa 1 y réplica 2
    sptr_3 <- read.csv(file = temp_path,
                       header = TRUE)
    # Eliminar columnas para evitar repeticiones
    sptr_3$Replicate <- NULL
    sptr_3$Plate <- NULL
    sptr_3$Construct <- NULL
    
    } else {
      
      next
      
    }
    
    
    # Verificar si existe el archivo
    temp_path <- file.path(dir.sptr, bios, "DATA", temp_files[4])
    if (file.exists(temp_path) == TRUE) {
      
    # Cargar datos de placa 2 y réplica 2
    sptr_4 <- read.csv(file = temp_path,
                       header = TRUE)
    # Eliminar columnas para evitar repeticiones
    sptr_4$Wavelength..nm. <- NULL
    
    } else {
      
      next
      
    }
    
    
    # Verificar si existe el archivo
    temp_path <- file.path(dir.sptr, bios, "DATA", temp_files[5])
    if (file.exists(temp_path) == TRUE) {
      
    # Cargar datos de placa 1 y réplica 3
    sptr_5 <- read.csv(file = temp_path,
                       header = TRUE)
    # Eliminar columnas para evitar repeticiones
    sptr_5$Replicate <- NULL
    sptr_5$Plate <- NULL
    sptr_5$Construct <- NULL
    
    } else {
      
      next
      
    }
    
    
    # Verificar si existe el archivo
    temp_path <- file.path(dir.sptr, bios, "DATA", temp_files[6])
    if (file.exists(temp_path) == TRUE) {
      
    # Cargar datos de placa 2 y réplica 3
    sptr_6 <- read.csv(file = file.path(dir.sptr, bios, "DATA", temp_files[6]),
                       header = TRUE)
    # Eliminar columnas para evitar repeticiones
    sptr_6$Wavelength..nm. <- NULL
    
    } else {
      
      next
      
    }
    

    # Generar gráfico de espectros
    # Generar paleta de colores
    color_lines <- c("#add8e6", "#92b8dc", "#7698d1", 
                     "#5b78c7", "#3f58bd", "#2438b2", 
                     "#0818a8")
    
    # Generar vector para agregar a la leyenda
    NaCl_lines <- c("0", "0.2", "0.4", "0.6", 
                    "0.8", "1.0", "1.5")
    
    # Juntar data frames de la misma réplica
    # Datos de réplica 1
    temp_fret_N <- cbind(sptr_1, sptr_2)
    
    #Generar espectro
    plot_sptr <- ggplot(data = temp_fret_N[seq(1, nrow(temp_fret_N), 5), ]) + 
      #NaCl_0 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_1,
                    color = "0"), size = 0.8) +
      #NaCl_200 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_2,
                    color = "0.2"), size = 0.8) +
      #NaCl_400 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_3,
                    color = "0.4"), size = 0.8) +
      #NaCl_600 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_4,
                    color = "0.6"), size = 0.8) +
      #NaCl_800 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_6,
                    color = "0.8"), size = 0.8) +
      #NaCl_1000 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_7,
                    color = "1.0"), size = 0.8) +
      #NaCl_1000 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_8,
                    color = "1.5"), size = 0.8) +
      # Agregar las modificaciones
      #Modificaciones extra
      theme_bw() +
      labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.grid = element_blank()) +
      coord_cartesian(ylim = c(0.3, 2)) +
      scale_y_continuous(breaks = seq(0.3, 2,0.2)) +
      scale_color_manual(name = "[NaCl] (M)",
                         values = color_lines, 
                         label = NaCl_lines) +
      annotate(geom = "text", x = 505, y = 1.8, 
               label = paste0("IDRBS-", bios), size = 4.5)
    
    # Generar nombre del gráfico
    name_plot <- paste0("plot_sptr_", bios, "_", temp_fret_N$Replicate[1], ".",
                        format.plot)
    
    # Guardar geáfico
    ggsave(plot = plot_sptr, 
           filename = file.path(dir.sptr, bios, "PLOTS", name_plot), 
           device = format.plot, width = 6, height = 4, units = "in", 
           dpi = 450)
    
    # Eliminar variable temp_fret_N
    remove(... = temp_fret_N)
    
    
    # Datos de réplica 2
    temp_fret_N <- cbind(sptr_3, sptr_4)
    
    #Generar espectro
    plot_sptr <- ggplot(data = temp_fret_N[seq(1, nrow(temp_fret_N), 5), ]) + 
      #NaCl_0 mM
      #NaCl_0 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_1,
                    color = "0"), size = 0.8) +
      #NaCl_200 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_2,
                    color = "0.2"), size = 0.8) +
      #NaCl_400 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_3,
                    color = "0.4"), size = 0.8) +
      #NaCl_600 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_4,
                    color = "0.6"), size = 0.8) +
      #NaCl_800 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_6,
                    color = "0.8"), size = 0.8) +
      #NaCl_1000 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_7,
                    color = "1.0"), size = 0.8) +
      #NaCl_1000 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_8,
                    color = "1.5"), size = 0.8) +
      # Agregar las modificaciones
      #Modificaciones extra
      theme_bw() +
      labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.grid = element_blank()) +
      coord_cartesian(ylim = c(0.3, 2)) +
      scale_y_continuous(breaks = seq(0.3, 2,0.2)) +
      scale_color_manual(name = "[NaCl] (M)",
                         values = color_lines, 
                         label = NaCl_lines) +
      annotate(geom = "text", x = 505, y = 1.8, 
               label = paste0("IDRBS-", bios), size = 4.5)
    
    # Generar nombre del gráfico
    name_plot <- paste0("plot_sptr_", bios, "_", temp_fret_N$Replicate[1], ".",
                        format.plot)
    
    # Guardar geáfico
    ggsave(plot = plot_sptr, 
           filename = file.path(dir.sptr, bios, "PLOTS", name_plot), 
           device = format.plot, width = 6, height = 4, units = "in", 
           dpi = 450)
    
    # Eliminar variable temp_fret_N
    remove(... = temp_fret_N)
    
    
    # Datos de réplica 3
    temp_fret_N <- cbind(sptr_5, sptr_6)
    
    #Generar espectro
    plot_sptr <- ggplot(data = temp_fret_N[seq(1, nrow(temp_fret_N), 5), ]) + 
      #NaCl_0 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_1,
                    color = "0"), size = 0.8) +
      #NaCl_200 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_2,
                    color = "0.2"), size = 0.8) +
      #NaCl_400 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_3,
                    color = "0.4"), size = 0.8) +
      #NaCl_600 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_4,
                    color = "0.6"), size = 0.8) +
      #NaCl_800 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_6,
                    color = "0.8"), size = 0.8) +
      #NaCl_1000 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_7,
                    color = "1.0"), size = 0.8) +
      #NaCl_1000 mM
      geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_8,
                    color = "1.5"), size = 0.8) +
      # Agregar las modificaciones
      #Modificaciones extra
      theme_bw() +
      labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.grid = element_blank()) +
      coord_cartesian(ylim = c(0.3, 2)) +
      scale_y_continuous(breaks = seq(0.3, 2,0.2)) +
      scale_color_manual(name = "[NaCl] (M)",
                         values = color_lines, 
                         label = NaCl_lines) +
      annotate(geom = "text", x = 505, y = 1.8, 
               label = paste0("IDRBS-", bios), size = 4.5)
    
    # Generar nombre del gráfico
    name_plot <- paste0("plot_sptr_", bios, "_", temp_fret_N$Replicate[1], ".",
                        format.plot)
    
    # Guardar geáfico
    ggsave(plot = plot_sptr, 
           filename = file.path(dir.sptr, bios, "PLOTS", name_plot), 
           device = format.plot, width = 6, height = 4, units = "in", 
           dpi = 450)
    
    # Eliminar variable temp_fret_N
    remove(... = temp_fret_N)

  }
}
