# Paqueterías ----
library(ggplot2)
library(dplyr)
library(drc)
library(broom)

# Cargar archivo ----
df_fret_200 <- read.csv(file = "E:/Screening biblioteca3/FRET/all_FRET_biosensors.csv", 
                         header = TRUE, sep = ",")

# Construir modelo no lineal ----

# Obtener los nombres de los constructos del
# archivo cargado
list_names <- names(table(df_fret_200$Construct))

# Realizar una iteración para obtener los datos de
# cada constructo en la variable "temp"
temp_newdata <- data.frame()
for (a in list_names) {
  
  # Obtener data frame individual
  temp <- df_fret_200[df_fret_200$Construct == a, ]
  
  # Cambiar ceros por 0.01
  temp[temp$Treatment == 0, ]$Treatment <- 0.01
  
  # Cambiar ceros por 0.01 para transformación logarítmica
  temp <- temp %>% mutate(Treatment = ifelse((Treatment == 0),
                                               yes = 0.01, 
                                               no = Treatment))
  
  # Calcular el promedio del valor de normalización para cada
  # tratamiento por cada réplica
  
  temp_summary <- temp %>% group_by(Treatment, Replicate, Construct) %>% 
    summarise("Mean" = mean(Normalized))
  
  # Predecir modelo no lineal
  fit <- drm(formula = Mean ~ Treatment, 
             data = temp_summary, 
             fct = LL.4())
  
  # Crear nuevos datos para la curva del modelo
  newdata <- expand.grid(Treatment = exp(seq(0, 10, length = 100)))
  
  # Predecir intervalos de confianza
  pm <- predict(fit, newdata = newdata, interval = "confidence")
  
  # Datos nuevos con las predicciones al dataframe
  newdata$p <- pm[, 1]
  newdata$pmin <- pm[, 2]
  newdata$pmax <- pm[, 3]
  newdata$Construct <- a
  
  # Juntar todos los datos en un dataframe
  temp_newdata <- rbind(newdata, temp_newdata)
  
}



# Graficar la curva con los datos con SED1 en azul
ggplot() +
  geom_line(data = temp_newdata, 
            aes(x = (Treatment/1000), y = p, 
                group = Construct, color = Construct),
            size = 0.7, color = "gray", 
            lty = 1, alpha = 0.3, show.legend = FALSE) +
  geom_line(data = temp_newdata[temp_newdata$Construct == 201, ], 
            aes(x = (Treatment/1000), y = p, 
                group = Construct),
            size = 0.8, color = "blue", 
            lty = 1, alpha = 0.6, show.legend = FALSE) +
  theme_classic() +
  labs(x = "[NaCl] (M)", 
       y = "Normalized \nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(xlim = c(0, 1.55), 
                  ylim = c(0.95, 3)) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.25),
                     expand = c(0,0))

#Guardar gráfico
ggsave(filename = "E:/Screening biblioteca3/FRET/DRC_all_bios_SED1.pdf",
       device = "pdf", width = 6, height = 5, units = "in", 
       dpi = 450) 



# Graficar la curva con los datos (colores)
ggplot() +
  geom_line(data = temp_newdata, 
            aes(x = (Treatment/1000), y = p, 
                group = Construct, color = Construct),
            size = 0.7, 
            lty = 1, alpha = 0.3, show.legend = FALSE) +
  theme_classic() +
  labs(x = "[NaCl] (M)", 
       y = "Normalized \nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(xlim = c(0, 1.55), 
                  ylim = c(0.95, 3)) +
  scale_x_continuous(expand = c(0,0))

#Guardar gráfico
ggsave(filename = "E:/Screening biblioteca3/FRET/DRC_all_bios_colores.pdf",
       device = "pdf", width = 6, height = 5, units = "in", 
       dpi = 450) 




# Graficar la curva con los datos de CAHS y P53
ggplot() +
  geom_line(data = temp_newdata, 
            aes(x = (Treatment/1000), y = p, 
                group = Construct, color = Construct),
            size = 0.7, color = "gray", 
            lty = 1, alpha = 0.3, show.legend = FALSE) +
  geom_line(data = temp_newdata[temp_newdata$Construct == 163, ], 
            aes(x = (Treatment/1000), y = p, 
                group = Construct),
            size = 0.8, color = "red", 
            lty = 1, alpha = 0.6, show.legend = FALSE) +
  geom_line(data = temp_newdata[temp_newdata$Construct == 7, ], 
            aes(x = (Treatment/1000), y = p, 
                group = Construct),
            size = 0.8, color = "blue", 
            lty = 1, alpha = 0.6, show.legend = FALSE) +
  theme_classic() +
  labs(x = "[NaCl] (M)", 
       y = "Normalized \nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(xlim = c(0, 1.55), 
                  ylim = c(0.95, 3)) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.25),
                     expand = c(0,0))

#Guardar gráfico
ggsave(filename = "E:/Screening biblioteca3/FRET/DRC_all_bios_P53_CAHS.pdf",
       device = "pdf", width = 6, height = 5, units = "in", 
       dpi = 450) 

