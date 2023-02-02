# Paqueterías
library(ggplot2)
library(ggpubr)
library(ggrepel)

# Función para generar gráficos de correlación de parámetros de CIDER
cider_plots <- function(file_dir, 
                        value, 
                        response, 
                        xaxis_name, 
                        save_dir) {
  
  # Cargar archivo de DELTA FRET
  df_fret_200 <- read.csv(file = file.path(file_dir), header = TRUE)
  
  # Obtener datos de acuerdo a la respuesta buscada
  temp_h <- df_fret_200[df_fret_200$Response == response, ]

  # Calcular correlaciones de la respuesta
  temp <- signif(cor(temp_h$mean_delta,
                     temp_h[, as.character(value)], 
                     method = "pearson"), digits = 2)
  

  # Obtener valor de p de la correlación
  temp1 <- signif(cor.test(temp_h$mean_delta,
                           temp_h$value, 
                           method = "pearson")[[3]], digits = 2)
  print(temp)
  # Mostrar valor de correlación
  print(message("Hola Constanza, el valor de r de Pearson es: ", temp))
  
  # Mostrar valor de p value de la correlación
  print(message("y el valor p-value es de: ", temp1))
  
  # Realizar gráfico
  p <- ggplot() +
    geom_jitter(data = df_fret_200, aes(x = as.numeric(value), 
                                        y = mean_delta, 
                                        color = response)) +
    geom_smooth(data = df_fret_200, aes(x = as.numeric(value), 
                                        y = mean_delta, 
                                        group = response),
                method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
    labs(x = xaxis_name, 
         y = expression(Delta * "FRET")) +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid = element_blank()) +
    coord_cartesian(xlim = c(min(df_fret_200$value),
                             max(df_fret_200$valuee)), 
                    ylim = c(0,2.5)) +
    annotate(geom = "text", x = 62, y = 2.4, 
             label = paste0("r = ", temp), size = 4.5) +
    annotate(geom = "text", x = 61, y = 2.15,
             label = paste0("p = ", temp1), size = 4.5) +
    scale_color_manual(name = "Respuesta",
                       values = c("blue", "red", "gold"))
  
  # Guardar gráfico en pdf
  ggsave(plot = p, filename = file.path(save_dir, paste0(value, ".pdf")), device = "pdf", 
         width = 5, height = 3, units = "in", dpi = 450)
  
  # Guardar gráfico en png
  ggsave(plot = p, filename = file.path(save_dir, paste0(value, ".png")), device = "png", 
         width = 5, height = 3, units = "in", dpi = 450)
  
}

# Ejecutar la función cider plots
cider_plots(file_dir = "D:/ALL/DATA/DATA FOR R/LIBRARY/df_fret_hml_delta.csv",
            value = FCR, 
            response = "High", 
            xaxis_name = "Kappa", 
            save_dir = "D:/ALL/PLOTS/CORRELATIONS/")
