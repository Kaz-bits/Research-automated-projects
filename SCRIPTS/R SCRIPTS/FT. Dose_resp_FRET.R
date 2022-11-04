# Paqueterías ----
library(ggplot2)
library(ggpubr)
library(dplyr)
library(drc)
library(broom)

# Cargar archivo ----
df_fret_21 <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/All_factors_FRET.txt", 
                         header = TRUE, sep = ",")


# Contruir modelo no lineal ----

# Arregar los datos de cada constructo
temp_SED1_nls <- df_fret_21[df_fret_21$Constructo == "SED1",]
temp_SED1_nls[temp_SED1_nls$Tratamiento == 0, ]$Tratamiento <- 0.01



# Obtener los nombres de los constructos del
# archivo cargado

list_names <- names(table(df_fret_21$Constructo))

# Realizar una iteración para obtener los datos de
# cada constructo en la variable "temp"

temp_newdata <- matrix(nrow = 100, ncol = 5)
for (a in list_names) {
  
  # Obtener data frame individual
  temp <- df_fret_21[df_fret_21$Constructo == a, ]
  
  # Cambiar ceros por 0.01
  temp[temp$Tratamiento == 0, ]$Tratamiento <- 0.01
  
  # Cambiar ceros por 0.01 para transformación logarítmica
  temp <- temp %>% mutate(Tratamiento = ifelse((Tratamiento == 0),
                                               yes = 0.01, 
                                               no = Tratamiento))
  
  # Calcular el promedio del valor de normalización para cada
  # tratamiento por cada réplica
  
  temp_summary <- temp %>% group_by(Tratamiento, Replica, Constructo) %>% 
    summarise("mean_Response" = mean(Normalización))
  
  # Predecir modelo no lineal
  fit <- drm(mean_Response ~ Tratamiento, data = temp_summary, 
             fct = LL.5())
  
  # Crear nuevos datos para la curva del modelo
  newdata <- expand.grid(Tratamiento = exp(seq(log(0.001), log(2), length = 100)))
  
  # Predecir intervalos de confianza
  pm <- predict(fit, newdata = newdata, interval = "confidence")
  
  # Datos nuevos con las predicciones al dataframe
  newdata$p <- pm[, 1]
  newdata$pmin <- pm[, 2]
  newdata$pmax <- pm[, 3]
  newdata$Construct <- a
  
  # Juntar todos los datos en un dataframe
  
  
}
 
 

# Graficar la curva con los datos

ggplot(temp_summary, aes(x = Tratamiento, y = mean_Response)) +
  geom_line(data = newdata_SED1, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_140, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_137, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_082, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_054, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_142, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_138, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_139, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "gray") +
  geom_line(data = newdata_144, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "red") +
  geom_line(data = newdata_045, aes(x = Tratamiento, y = p),
            size = 1, lty = 1, color = "blue") +
  theme_classic() +
  labs(x = "[NaCl] (M)", 
       y = "Normalized \nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(xlim = c(0, 1.02), 
                  ylim = c(0.9, 2.3)) +
  scale_y_continuous(breaks = seq(0.95, 2.3, 0.2),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 1.02, 0.2),
                     expand = c(0, 0)) +
  scale_color_manual(name = "Replicates", 
                     values = c("#3FA0FF", "#264DFF", "#290AD8"))
#Guardar gráfico
ggsave(filename = "PLOTS/FRET/Dose_response/DRFT vs SED1/DRFT_factors.pdf",
       device = "pdf", width = 6, height = 5, units = "in", 
       dpi = 600) 
 

 
