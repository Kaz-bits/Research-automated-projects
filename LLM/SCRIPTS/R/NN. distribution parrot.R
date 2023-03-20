# Paqueterías
library(ggplot2)
library(dplyr)
library(ggpubr)

# Funciones requeridas
bins <- function(x) {
  if ((2 * IQR(x, na.rm = TRUE) / length(x)^(1/3)) > 0.05) {
    c(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3))
  } else {
    round(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3), digits = 2)
  }
}


# Cargar archivo
NN_200 <- read.table(file = "/media/kazbits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
         header = TRUE)

# Verificar la distribución de los datos
# Determinación del ancho de banda----
binp <- bins(NN_200$mean_delta)

# Histograma de Delta FRET
ggplot() + 
  # Gráfico del proteoma
  geom_histogram(data = NN_200, aes(y = ..ncount.., x = mean_delta), 
                 alpha = 0.8, color = "black", fill = "steelblue",
                 binwidth = 0.1, boundary = 0) +
  geom_density(data = NN_200, aes(y = ..scaled.., x = mean_delta), 
               linewidth = 1, lty = 2, color = "red") +
  # Agregar modificaciones finales
  labs(x = expression(Delta*"FRET"), 
       y = "Frecuencia normalizada") +
  theme_linedraw() +
  theme(panel.grid = element_blank(),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14)) + 
  coord_cartesian(xlim = c(0, max(NN_200$mean_delta))) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.2)) + 
  scale_x_continuous(expand = c(0.1, 0), breaks = seq(0, max(NN_200$mean_delta), 0.2))



# Mediana de los datos
median(NN_200$mean_delta)

# Media de los datos
mean(NN_200$mean_delta)

# Moda de los datos
table(round(NN_200$mean_delta, digits = 1))

# Generar muestras aleatorias con una media de 0.34 (mediana)
set.seed(1)
NN_200_sample <- sample_n(NN_200, size = 160)

# Verificar la distribución 
# Histograma de Delta FRET
ggplot() + 
  # Gráfico del proteoma
  geom_histogram(data = NN_200_sample, aes(y = ..ncount.., x = mean_delta), 
                 alpha = 0.8, color = "black", fill = "steelblue",
                 binwidth = 0.1, boundary = 0) +
  geom_density(data = NN_200, aes(y = ..scaled.., x = mean_delta), 
               linewidth = 1, lty = 2, color = "red") +
  # Agregar modificaciones finales
  labs(x = expression(Delta*"FRET"), 
       y = "Frecuencia normalizada") +
  theme_linedraw() +
  theme(panel.grid = element_blank(),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14)) + 
  coord_cartesian(xlim = c(0, max(NN_200_sample$mean_delta))) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.2)) + 
  scale_x_continuous(expand = c(0.1, 0), breaks = seq(0, max(NN_200_sample$mean_delta), 0.2))

qqnorm(NN_200_sample$mean_delta)
qqline(NN_200_sample$mean_delta)
