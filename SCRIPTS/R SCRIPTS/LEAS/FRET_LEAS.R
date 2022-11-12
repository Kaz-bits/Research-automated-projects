# Paqueterias
library(ggplot2)
library(dplyr)
library(readxl)
library(drc)
library(dr4pl)

# Cargar archivo
df_leas <- readxl::read_excel(path = "C:/Users/HP/Desktop/FRET R SCRIPTS/LEAS/Fretlevadura_leas_scriptLEAs.xlsx", 
                              col_names = TRUE)


# Obtener los nombres de los construccionos del
# archivo cargado
list_names <- names(table(df_leas$construccion))

# Realizar una iteración para obtener los datos de
# cada construcciono en la variable "temp"
temp_newdata <- data.frame()
for (a in list_names) {
  
  # Obtener data frame individual
  temp <- df_leas[df_leas$construccion == a, ]
  
  # Cambiar ceros por 0.01
  temp[temp$tratamiento == 0, ]$tratamiento <- 0.01
  
  # Cambiar ceros por 0.01 para transformación logarítmica
  temp <- temp %>% mutate(tratamiento = ifelse((tratamiento == 0),
                                             yes = 0.01, 
                                             no = tratamiento))
  
  # Calcular el promedio del valor de normalización para cada
  # tratamiento por cada réplica
  
  temp_summary <- temp %>% group_by(tratamiento, construccion) %>% 
    summarise("Mean" = mean(normalizado))
  
  # Predecir modelo no lineal
  fit <- drm(formula = Mean ~ tratamiento, 
             data = temp_summary, 
             fct = LL.3())
  
  # Crear nuevos datos para la curva del modelo
  newdata <- expand.grid(tratamiento = exp(seq(0, 1.5, length = 100)))
  
  # Predecir intervalos de confianza
  pm <- predict(fit, newdata = newdata, interval = "confidence")
  
  # Datos nuevos con las predicciones al dataframe
  newdata$p <- pm[, 1]
  newdata$pmin <- pm[, 2]
  newdata$pmax <- pm[, 3]
  newdata$construccion <- a
  
  # Juntar todos los datos en un dataframe
  temp_newdata <- rbind(newdata, temp_newdata)
  
}


# Graficar la curva con los datos con SED1 en azul
ggplot() +
  geom_line(data = temp_newdata, 
            aes(x = (tratamiento/1000), y = p, 
                group = construccion, color = construccion),
            size = 0.8, 
            lty = 1, alpha = 0.6) +
  labs(x = "NaCl (M)", y = "normalized DxAm/DxDm", color = NULL) +
  theme_classic() +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank() , 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(linetype=NULL)) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  coord_cartesian(xlim = c(0, 1.5))

#Guardar gráfico
ggsave(filename = "C:/Users/HP/Desktop/FRET R SCRIPTS/LEAS/DCR_LEAS.pdf",
       device = "pdf", width = 6, height = 5, units = "in", 
       dpi = 450) 



# Cargar archivo
df_leas <- readxl::read_excel(path = "C:/Users/HP/Desktop/FRET R SCRIPTS/LEAS/Fretlevadura_leas_scriptLEAs.xlsx", 
                              col_names = TRUE)

# Calcular el modelo de la curva con la paquetería drpl4
model <- dr4pl(df_leas$normalizado ~ df_leas$tratamiento)
summary(model)$coefficients

ggplot(data = df_leas, aes(x = tratamiento, 
                           y = normalizado, 
                           group = construccion,
                           color = construccion)) +
  geom_point() +
  geom_smooth() +
  xlab("NaCl (M)") +
  ylab("normalized DxAm/DxDm") +
  labs(color = NULL) +
  theme_classic() +
  theme(axis.text=element_text(size = 12),
        axis.title=element_text(size = 14), 
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(linetype = NULL)) +
  scale_y_continuous(limits = c (0.9, 1.7), 
                     breaks = seq(from = 0.9, to = 1.7, by = 0.2)) +
  scale_x_continuous(limits = c (0, 2.0), 
                     breaks = seq(from = 0, to = 2.0, by = 0.2))
  
