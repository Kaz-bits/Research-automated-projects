#Paqueterías----

library(ggplot2)
library(ggpubr)

#Análisis de dualidad----

#Generar una variable con todos los nombres de los archivos 
#por analizar

list_xlsx <- dir(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/",
                pattern = "*.xlsx")


#Analizar cada uno de los archivos de la variable "list_xlxs" y 
#Colocar el directorio donde se encuentran todos los archivos

setwd() #cambiar directorio

temp_dual <- c() #vector vacío
for (a in list_xlsx) {
  
  temp_dual <- readxl::read_xlsx(path = a, col_names = TRUE, sheet = 2) 
  
  #Agregar condicional al porcentaje de desroden
  
  for (b in 1:nrow(temp_dual)) {
    for (c in ((temp_dual[b,6])*1)) {
      if (c < 10) {
        
        temp_dual[b,9] <- 0
        
      } else {
        
        temp_dual[b,9] <- c
        
      }
    }
  }
}

#Agregar condicional del porcentaje de alfa hélice

temp_dual$condition_50 <- 1

for (a in 1:nrow(temp_dual)) {
  for (b in ((temp_dual[a,3])*100)) {
    if (b < 50) {
      
      temp_dual[a,10] <- 0
      
    } else {
      
      temp_dual[a,10] <- b
      
    }
  }
}


#Calcular el parámetro de dualidad dividiendo el valor de
#alfa hélice de la condicional 10 entre la condicional 50 del desorden

temp_dual$dualidad <- (as.numeric(temp_dual$condition_10))/(temp_dual$condition_50)
temp_dual[is.na(temp_dual)] <- 0


#Agregar un valor lógico para aquellos datos de dualidad diferentes
#de cero

temp_dual$lógico <- (temp_dual$dualidad > 0)

#Cantidad de residuos que poseen un valor de dualidad

length(temp_dual[(temp_dual$dualidad > 0),"dualidad"])

#Guardar el archivo directamente en la carpeta del directorio
#y no en Rstudio

write.table(x = temp_dual, 
            file = "DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/IDFRT_001D.txt",
            quote = FALSE, sep = ",", row.names = FALSE)



#Gráfico de PONDR (desorden)----

#Graficar residuo vs fracción de alfa hélice

ggplot() +
  geom_line(data = temp_dual, aes(x = as.integer(res), y = VLXT, 
                                  colour = "Desorden")) +
  geom_line(data = temp_dual, aes(x = as.integer(res), y = Hel_fracc,
                                  colour = "Alfa hélice")) +
  geom_hline(yintercept = 0.5, size = 1, alpha = 0.6) +
  theme_classic() +
  labs(x = "Número de residuo",
       y = "Fracción alfa hélice") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.key.size = unit(0.6, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, max(temp_dual$res), by = 15), 
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.1), expand = c(0,0)) +
  scale_color_manual(name = "Fracción", values = c("blue", "red"))


#Gŕafico de dualidad----

ggplot() +
  geom_line(data = temp_dual, aes(x = as.integer(res), y = dualidad),
            color = "blue") +
  geom_hline(yintercept = 0.1, 
             size = 1, alpha = 0.6) +
  theme_classic() +
  labs(x = "Número de residuo",
       y = "Dualidad") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, max(temp_dual$res), by = 15), 
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.1), expand = c(0,0))


#Guardar gráficos----

#Guardar gráficos de desorden en carpeta única


ggsave(filename = "PLOTS/DESORDEN (PONDR) - ALFA/IDRFT_001DA.png", 
       device = "png", width = 8, height = 5, dpi = 650, plot = p)




#Determinación de coverage de dualidad para IDRs----

#Generar una variable con todos los nombres de los
#archivos por analizar

list_txt <- dir(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/",
                pattern = "*.txt")

#Cargar cada archivo con los datos de dualidad calculados
#anteriormente. Cambiar antes el directorio al sitio donde
#se encuentren los archivos

setwd("DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/")

temp_dual <- c() #vector vacío
coverage <- c() #vector vacío
for (a in list_txt) {
  
  temp_dual <- read.table(file = a, header = TRUE, sep = ",")
  
  #Contar la cantidad de residuos con dualidad (mayor a 0.1) 
  #y dividirlos entre la longitud de la proteína (cantidad de
  #residuos) y multiplicar por 100 
  
  i <- table(temp_dual$dualidad >= 0.1)[2] #Obtener residuos duales
  h <- table(temp_dual$dualidad >= 0.1)[2] + table(temp_dual$dualidad >= 0.1)[1] #Total
  
  #Obtener el porcentaje dividiendo a entre b y multiplicando por
  #100
  
  coverage[length(coverage) + 1] <- (i/h)*100 #Dualidad coverage
  
}

#Convertir en un dataframe los datos con un ID general

coverage <- data.frame("factor_ID" = c(1:69)[-c(10,13,19,38,43,53,54,62)], 
                       "coverage" = coverage)


