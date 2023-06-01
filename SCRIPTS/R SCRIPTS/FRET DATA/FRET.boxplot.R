# Cargar paqeterías necesarias
library(ggplot2)
library(ggpubr)

# Función para generar boxplots
FRET.boxplot <- function(dir.fret, 
                         format.plot = "pdf", 
                         ymax_text = 2.5, 
                         ymax_name = 2.85,
                         ymax_axis = 3, 
                         ymin_axis = 0.85,
                         yaxis_ticks = 0.5, 
                         bios_name = TRUE) {
  
  # Obtener el nombre de cada biosensor
  temp_files <- list.files(dir.fret)
  
  for (bios in temp_files) {
    
    # Obtener el directorio del biosensor
    temp_path <- file.path(dir.fret, bios, "DATA", paste0(bios, ".csv"))
    
    # Verificar si existe el archivo del biosensor
    if (file.exists(temp_path) == TRUE) {
      
      # Cargar archivo
      fret_ratio <- read.csv(file = file.path(dir.fret, bios, "DATA", 
                                              paste0(bios, ".csv")), 
                             header = TRUE)
      
      # Generar paleta de colores
      color_boxplot <- c("#add8e6", "#92b8dc", "#7698d1", 
                         "#5b78c7", "#3f58bd", "#2438b2", 
                         "#0818a8")
      
      # Pruebas estadísticas
      # Prueba t.test para 0 vs 200
      p1 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 200,]$Normalized)[[3]]
      
      
      # Prueba t.test para 0 vs 400
      p2 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 400,]$Normalized)[[3]]
      
      
      # Prueba t.test para 0 vs 600
      p3 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 600,]$Normalized)[[3]]
      
      # Prueba t.test para 0 vs 800
      p4 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 800,]$Normalized)[[3]]
      
      # Prueba t.test para 0 vs 1000
      p5 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 1000,]$Normalized)[[3]]
      
      # Prueba t.test para 0 vs 1500
      p6 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 1500,]$Normalized)[[3]]
      
      # Nivel de significancia
      signif_level <- c(ns = 1, "*" = 0.05, "**" = 0.01, 
                        "***" = 0.001, "****" = 0.0001)
      
      
      # Juntar los valores de "p" de cada condición
      a <- c(p1, p2, p3, p4, p5, p6)
      
      
      # Evaluar el valor de p de las concentraciones
      # Para 0 vs 200
      i <- c(a[1] > 0.05, a[1] <= 0.05, a[1] <= 0.01, a[1] <= 0.001, a[1] <= 0.0001)
      # Para 0 vs 400
      h <- c(a[2] > 0.05, a[2] <= 0.05, a[2] <= 0.01, a[2] <= 0.001, a[2] <= 0.0001)
      # Para 0 vs 600
      j <- c(a[3] > 0.05, a[3] <= 0.05, a[3] <= 0.01, a[3] <= 0.001, a[3] <= 0.0001)
      # Para 0 vs 800
      k <- c(a[4] > 0.05, a[4] <= 0.05, a[4] <= 0.01, a[4] <= 0.001, a[4] <= 0.0001)
      # Para 0 vs 1000
      l <- c(a[5] > 0.05, a[5] <= 0.05, a[5] <= 0.01, a[5] <= 0.001, a[5] <= 0.0001)
      # Para 0 vs 1500
      m <- c(a[6] > 0.05, a[6] <= 0.05, a[6] <= 0.01, a[6] <= 0.001, a[6] <= 0.0001)
      
      #Extraer los simbolos de significancia del valor de probabilidad anterior
      p1_box <- (names(signif_level[i]))[length((names(signif_level[i])))] #p1
      p2_box <- (names(signif_level[h]))[length((names(signif_level[h])))] #p2
      p3_box <- (names(signif_level[j]))[length((names(signif_level[j])))] #p3
      p4_box <- (names(signif_level[k]))[length((names(signif_level[k])))] #p4
      p5_box <- (names(signif_level[l]))[length((names(signif_level[l])))] #p5
      p6_box <- (names(signif_level[m]))[length((names(signif_level[m])))] #p6
      
      # Construir gráfico
      # Convertir columna de tratamiento a factor
      fret_ratio$Treatment <- as.factor(fret_ratio$Treatment)
      
      # Gráfico de cajas  
      plot <- ggplot(data = fret_ratio) +
        # Gráfico de cajas
        geom_boxplot(aes(x = Treatment, y = Normalized, 
                         group = Treatment), fill = color_boxplot,
                     outlier.shape = NA) +
        # Gráfico de puntos
        geom_jitter(aes(x = Treatment, y = Normalized, 
                        group = Treatment), color = "black") +
        # Modificaciones extra
        labs(x = "[NaCl] (mM)", y = "Normalized\nDxAm/DxDm") +
        theme_bw() +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              panel.grid = element_blank()) +
        coord_cartesian(ylim = c(ymin_axis, ymax_axis)) +
        scale_y_continuous(breaks = seq(ymin_axis, ymax_axis, yaxis_ticks)) +
        annotate(geom = "text", x = 2, y = ymax_text, label = p1_box) +
        annotate(geom = "text", x = 3, y = ymax_text, label = p2_box) +
        annotate(geom = "text", x = 4, y = ymax_text, label = p3_box)  +
        annotate(geom = "text", x = 5, y = ymax_text, label = p4_box) +
        annotate(geom = "text", x = 6, y = ymax_text, label = p5_box) +
        annotate(geom = "text", x = 7, y = ymax_text, label = p6_box)  +
        # Condicional para el nombre
        if (bios_name == TRUE) {
          
          # Agregar nombres de acuerdo con número de biosensor
          annotate(geom = "text", x = 4, y = ymax_name, label = paste0("IDRBS-", bios),
                   size = 5)
          
        } else {
          
          # Agregar nombre de acuerdo a las carpetas de FRET
          annotate(geom = "text", x = 4, y = ymax_name, label = bios,
                   size = 5)
        }
      
      # Guardar gráfico
      ggsave(plot = plot, filename = file.path(dir.fret, bios, "PLOTS", paste0(bios, ".", format.plot)),
             device = format.plot, width = 6, height = 4, units = "in", 
             dpi = 400) 
      
      
    } else {
      
      next
      
      
    }
  }
}
