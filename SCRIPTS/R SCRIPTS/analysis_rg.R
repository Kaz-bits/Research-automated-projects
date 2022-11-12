# Paqueterías
library(ggplot2)

# Cargar archivo
data_rg <- readxl::read_excel(path = "/home/cesar/Downloads/Analisis_rg.xlsx", 
                              col_names = FALSE)[, -c(10:14)]

# Extraer los datos de cada LEA del archivo anterior
# Crear data frame para guardar los datos

rg <- data.frame(matrix(nrow = 35, ncol = 3))

# Agregar nombres a las columnas
names(rg)[1] <- "Rg" 
names(rg)[2] <- "sd"
names(rg)[3] <- "Protein"

# Datos de LEA_5
rg$Protein[1:7] <- "LEA_5"
rg$Rg[1:7] <- as.numeric(data_rg[data_rg$...1 == "At3g51810_1",]$...5)
rg$sd[1:7] <- as.numeric(data_rg[data_rg$...1 == "At3g51810_1",]$...6)


# Datos de LEA_4 At1g
rg$Protein[8:14] <- "LEA_4_At1g"
rg$Rg[8:14] <- as.numeric(data_rg[data_rg$...1 == "At1g52690_1",]$...5)
rg$sd[8:14] <- as.numeric(data_rg[data_rg$...1 == "At1g52690_1",]$...6)


# Datos de PvLEA18
rg$Protein[15:21] <- "PvLEA18"
rg$Rg[15:21] <- as.numeric(data_rg[data_rg$...1 == "At2g23120_PvLEA18",]$...5)
rg$sd[15:21] <- as.numeric(data_rg[data_rg$...1 == "At2g23120_PvLEA18",]$...6)


# Datos de LEA_4 At2g
rg$Protein[22:28] <- "LEA_4_At2g"
rg$Rg[22:28] <- as.numeric(data_rg[data_rg$...1 == "At2g42540_1",]$...5)
rg$sd[22:28] <- as.numeric(data_rg[data_rg$...1 == "At2g42540_1",]$...6)


# Datos de LEA_1
rg$Protein[29:35] <- "LEA_1"
rg$Rg[29:35] <- as.numeric(data_rg[data_rg$...1 == "At5g06760_2",]$...5)
rg$sd[29:35] <- as.numeric(data_rg[data_rg$...1 == "At5g06760_2",]$...6)

# Agregar datos aleatorios para graficar en el eje "x"
rg$x_axis <- rep(c(3, 2, 1, 0, -1, -2, -3), 5)
rg$x_axis[29:35] <- c(-3, -2, -1, 0, 1, 2, 3)

# Generar gráfico de líneas con desviación estándar
ggplot(data = rg, aes(y = Rg, x = x_axis, color = Protein)) + 
  # Agregar líneas
  geom_line(show.legend = FALSE, size = 0.7) +
  # Agregar puntos
  geom_point(show.legend = FALSE, size = 1.6) +
  # Modificaciones extra
  theme_classic() +
  labs(y = "Normalized Rg") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12)) +
  scale_color_manual(name = "", 
                     values = c("#CC0C00", "#7C878E", 
                                "#84BD00", "#FFCD00",
                                "#00B5E2")) +
  coord_cartesian(ylim = c(-0.8, 0)) +
  scale_y_continuous(breaks = seq(-0.8, 0, 0.10))
  

# Guardar gráfico
ggsave(filename = "~/Desktop/rg_analysis.pdf", device = "pdf", 
       width = 6, height = 5, units = "in", dpi = 450)
