# Paqueterías ----
library(ggplot2)
library(ggpubr)
library(dplyr)
library(drc)
library(broom)

# Cargar archivo ----
df_fret_200 <- read.csv(file = "C:/Users/HP/Desktop/FRET R SCRIPTS/all_biosensors.csv", 
                         header = TRUE, sep = ",")


# Contruir modelo no lineal ----

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
  fit <- drm(Mean ~ Treatment, data = temp_summary, 
             fct = LL.4())
  
  # Crear nuevos datos para la curva del modelo
  newdata <- expand.grid(Treatment = exp(seq(log(0.001), log(2), length = 100)))
  
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



# Graficar la curva con los datos

ggplot(temp_summary, aes(x = Treatment, y = Mean)) +
  geom_line(data = temp_newdata, 
            aes(x = Treatment, y = p, group = Construct, color = Construct),
            size = 1, lty = 1, show.legend = FALSE) +
  theme_classic() +
  labs(x = "[NaCl] (M)", 
       y = "Normalized \nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

#Guardar gráfico
ggsave(filename = "PLOTS/FRET/Dose_response/DRFT vs SED1/DRFT_factors.pdf",
       device = "pdf", width = 6, height = 5, units = "in", 
       dpi = 600) 


