# Paqueterías----
library(ggplot2)
library(ggpubr)

# Cargar archivo con todos los biosensores
delta_200 <- read.csv(file = "E:/Screening biblioteca3/FRET/all_biosensors.csv",
                             header = TRUE)

# Obtención DELTA FRET----

# Determinar la cantidad de Constructs
bios_n <- length(names(table(delta_200$Construct)))

# Generar un dataframe vacio para  colocar los datos
# del delta fret
df_fret_200 <- data.frame(matrix(nrow = bios_n, ncol = 3))

# Agregar nombres a las columnas
names(df_fret_200)[1] <- "Construct"
names(df_fret_200)[2] <- "Mean_Delta"
names(df_fret_200)[3] <- "sd"

# Añadir Constructs al dataframe creado
df_fret_200$Construct <- names(table(delta_200$Construct))

# Añadir media de DELTA FRET
temp <- c() # vector vacío
for (a in (names(table(delta_200$Construct)))) {
  
  i <- delta_200[delta_200$Construct == a &
                   delta_200$Treatment == 0 & 
                   delta_200$Plate == 1, ]$DxAm.DxDm
  
  h <- delta_200[delta_200$Construct == a & 
                        delta_200$Treatment == 1500, ]$DxAm.DxDm
  
  # Realizar delta FRET promedio
  temp[length(temp) + 1] <- mean(h-i)
  
}

# Añadir columna con delta FRET promedio al dataframe
df_fret_200$Mean_Delta <- temp

# Añadir desviación estándar de DELTA FRET
temp <- c() # vector vacío
for (a in (names(table(delta_200$Construct)))) {
  
  i <- delta_200[delta_200$Construct == a &
                   delta_200$Treatment == 0 & 
                   delta_200$Plate == 1, ]$DxAm.DxDm
  
  h <- delta_200[delta_200$Construct == a & 
                   delta_200$Treatment == 1500, ]$DxAm.DxDm
  
  # Realizar delta FRET promedio
  temp[length(temp) + 1] <- sd(h-i)
  
}

# Añadir columna con delta FRET promedio al dataframe
df_fret_200$sd <- temp


# Añadir una columna extra que contenga los valores
# promedio de FRET inicial (0M)

temp <- c() # vector vacío
for (a in (names(table(delta_200$Construct)))) {
  
  i <- delta_200[delta_200$Construct == a &
                   delta_200$Treatment == 0 & 
                   delta_200$Plate == 1, ]$DxAm.DxDm
  
  # Realizar delta FRET promedio
  temp[length(temp) + 1] <- mean(i)
  
}

# Añadir columna de FRET inicial (0M)
df_fret_200$FRET_0M <- temp



# Guardar archivo
write.csv(x = df_fret_200, file = "E:/Screening biblioteca3/FRET/FRET_delta_200.csv", 
          row.names = FALSE, quote = FALSE)



# Gráfico de puntos con error----

ggplot() +
  geom_vline(xintercept = unname(quantile(df_fret_200$Mean_Delta)[2]), 
             lty = 2) +
  geom_vline(xintercept = median(df_fret_200$Mean_Delta), 
             lty = 2, color = "blue") +
  geom_vline(xintercept = unname(quantile(df_fret_200$Mean_Delta)[4]), 
             lty = 2) +
  geom_point(data = df_fret_200, aes(y = Mean_Delta, 
                                        x = as.factor(construct)), 
             size = 4, shape = 21, 
             fill = "white", stroke = 1.5) +
  geom_errorbar(data = df_fret_200, aes(y = Mean_Delta, 
                                           y = as.factor(construct), 
                                           ymin = Mean_Delta - sd, 
                                           ymax = Mean_Delta + sd)) +
  labs(x = expression(Delta * "FRET"), 
       y = "IDR") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank())  + 
  coord_cartesian(xlim = c(-0.5, 2))


# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/test.pdf", 
       device = "pdf", 
       width = 30, height = 5, units = "in", dpi = 600)
