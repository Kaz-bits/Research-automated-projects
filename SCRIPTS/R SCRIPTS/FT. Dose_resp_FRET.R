# Paqueterías ----
library(ggplot2)
library(ggpubr)
library(dplyr)
library(drc)^
library(broom)

# Cargar archivo ----
df_fret_21 <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/All_factors_FRET.txt", 
                         header = TRUE, sep = ",")


# Contruir modelo no lineal ----

# Obtener los nombres de los constructos del
# archivo cargado

list_names <- names(table(df_fret_21$Constructo))

# Realizar una iteración para obtener los datos de
# cada constructo en la variable "temp"

for (a in list_names) {
  
  # Obtener data frame individual
  temp <- df_fret_21[df_fret_21$Constructo == a, ]
  
  # Cambiar ceros por 0.01
  temp[temp$Tratamiento == 0, ]$Tratamiento <- 0.01
      
} 

# Contruir data frame vacío
temp_summary <- data.frame(matrix(nrow = 1, ncol = 4))

# Modificar nombres del data frame
names(temp_summary)[1] <- "Tratamiento"
names(temp_summary)[2] <- "Replica"
names(temp_summary)[3] <- "mean_Response"
names(temp_summary)[4] <- "Constructo"

# Contruir vectores vacíos para almacenar información
temp_1 <- c() #vector vacío 
temp_2 <- c() #vector vacío
temp_3 <- c() #vector vacío
temp_4 <- c() #vector vacío

# Obtener las concentraciones del experimento
temp_conc <- as.numeric(names(table(temp$Tratamiento)))

# Obtener las réplicas del experimento
temp_reps <- as.numeric(names(table(temp$Replica)))
for (a in 1) {
  for (b in temp_conc) {
  for (c in temp_reps) {
    
    # Añadir concentraciones
    temp_summary[(nrow(temp_summary) + 1) - 0, 1] <- b
    
    # Añadir réplicas
    temp_summary[(nrow(temp_summary) + 1) - 1, 2] <- c
    
    # Añadir promedio de la respuesta
    temp_summary[(nrow(temp_summary) + 1) - 1, 3] <- mean(temp[temp$Tratamiento == a & temp$Replica == b, ]$Normalización)
    
    # Añadir constructos
    temp_summary[(nrow(temp_summary) + 1) - 1, 4] <- temp$Constructo[1]
    
  }
  }
  
  # Guardar archivos
  write.table(x = temp_summary, 
              file = paste0("/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/DRFT/", 
                            temp_summary$Constructo[1], 
                            "_DRFT.txt"))
}


# Agregar columnas al data frame vacío
temp_summary$Tratamiento <- temp_1
temp_summary$Replica <- temp_2
temp_summary$mean_Response <- temp_3
temp_summary$Constructo <- temp_4



for (b in temp_reps) {
  for (c in temp_conc) {
    
    # Extraer tratamientos y réplicas
    temp_1[length(temp_1) + 1] <- c
    temp_2[length(temp_2) + 1] <- b
    
    # Obtener media de cada tratamiento por réplica
    temp_3[length(temp_3) + 1] <- mean(temp[temp$Tratamiento == c & 
                                              temp$Replica == b, ]$Normalización)
    
    # Guardar constructo
    temp_4 <- temp$Constructo[1:(nrow(temp)/3)]
    
  }
}



# Contruir modelo no lineal ----

# Obtener los nombres de los constructos del
# archivo cargado

list_names <- names(table(df_fret_21$Constructo))

# Realizar una iteración para obtener los datos de
# cada constructo en la variable "temp"


  # Obtener data frame individual
  temp <- df_fret_21[df_fret_21$Constructo == "138", ]
  
  # Cambiar ceros por 0.01
  temp[temp$Tratamiento == 0, ]$Tratamiento <- 0.01
  
  # Arregar los datos de cada constructo
  temp_SED1_nls <- df_fret_21[df_fret_21$Constructo == "SED1",]
  temp_SED1_nls[temp_SED1_nls$Tratamiento == 0, ]$Tratamiento <- 0.01
  
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
  newdata$p <- pm[,1]
  newdata$pmin <- pm[,2]
  newdata$pmax <- pm[,3]
  
  # Juntar todos los datos en un dataframe
  
  #newdata_SED1 <- newdata
  #newdata_144 <- newdata
  #newdata_045 <- newdata
  #newdata_140 <- newdata
  #newdata_082 <- newdata
  #newdata_054 <- newdata
  #newdata_137 <- newdata
  #newdata_142 <- newdata
  #newdata_139 <- newdata
  #newdata_138 <- newdata
  
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
 
