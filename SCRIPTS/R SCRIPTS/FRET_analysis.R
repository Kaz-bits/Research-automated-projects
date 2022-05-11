#Paqueterías----

library(ggplot2)
library(ggpubr)

#Obtener el promedio para cada wavelength----
#Cargar archivo FRET

#cargar el archivo solo con la hoja 1 (datos) --revisar--
fret <- readxl::read_xlsx(path = "DATA/DATA FOR R/FRET/FRET ALTA/IDRFT_alta. Rep1. P1.xlsx", 
                  sheet = 1, col_names = FALSE)[-c(1:10),]

#Agregar los nombres a las columnas para identificarlas
names(fret)[1:length(names(fret))] <- unlist(fret[2,], use.names = FALSE)

#Eliminar el primer renglón 
fret <- fret[-c(1,2),]

#Se tienen 96 columnas (Samples). Cada tres samples
#es una condición, y cada 12 samples comienza de 
#nuevo la primer condición de tratamiento.


#Crear un dataframe con el numero de columnas según
#la cantidad de datos por analizar


temp_fret <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/(3)), 
                               nrow = nrow(fret)))
temp_vec <- c()
for (a in 1:length(temp_fret)) {
  temp_name <- paste0("NaCl_", a)
  temp_vec[length(temp_vec) + 1] <- temp_name
}

#Agregar nombres específicos a cada una de las
#columnas del dataframe
colnames(temp_fret) <- temp_vec


#Obtener promedio de cada condición para cada
#una de las longitudes de onda

temp_df <- data.frame(matrix(ncol = 4, nrow = 91))
for (a in (seq(from = 1, to = length(fret[,-c(1,2)]), by = 3) + 2)) {
   
    #Obtener los valores para cada longitud
    #de onda para cada condición
    temp <- as.numeric(unlist((fret[,a])))
    temp_df[,1] <- temp
    
    temp <- as.numeric(unlist((fret[,a+1])))
    temp_df[,2] <- temp
    
    temp <- as.numeric(unlist((fret[,a+2])))
    temp_df[,3] <- temp
    
    #Obtener la media de las tres observaciones
    i <- (temp_df[,1] + temp_df[,2] + temp_df[,3])
    h <- (length(temp_df)-1)
    
    #Media de las condiciones
    temp <- (i/h)
    temp_df[,4] <- temp
  
    #Agregar cada columna con cada promedio al
    #dataframe llamado "temp_fret"
  
    temp_fret[,(a/3)] <- temp_df[,4]
    
}

#Agregar la longitud de onda en la primer columna
temp_fret[,length(temp_fret) + 1] <- fret$`Wavelength [nm]`
temp_fret <- temp_fret[,c(length(temp_fret),1:(length(temp_fret) - 1))]
names(temp_fret)[1] <- names(fret)[2]


#Normalizar fluorescencia con punto isosbéstico----

temp <- (t(temp_fret[temp_fret[,1] == as.character(515),
                     c(2:length(temp_fret))]))
rownames(temp) <- NULL

#Emplear el vector "temp" creado para normaliza
#cada valor de "temp_fret"

temp_fret_N <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/(3)), 
                                            nrow = nrow(fret)))
temp_vec <- c()
for (a in 1:length(temp_fret_N)) {
  temp_name <- paste0("NaCl_", a)
  temp_vec[length(temp_vec) + 1] <- temp_name
}

#Agregar nombres específicos a cada una de las
#columnas del dataframe
colnames(temp_fret_N) <- temp_vec

#Normalizar los datos
for (a in 1:nrow(temp_fret)) {

  i <- (temp_fret[a,-1])/(temp)
  temp_fret_N[a,] <- i
}

#Agregar la longitud de onda en la primer columna
temp_fret_N[,length(temp_fret_N) + 1] <- fret$`Wavelength [nm]`
temp_fret_N <- temp_fret_N[,c(length(temp_fret_N),1:(length(temp_fret_N) - 1))]
names(temp_fret_N)[1] <- names(fret)[2]

#Guardar archivo con datos normalizados
write.table(x = temp_fret_N, sep = ",", row.names = FALSE, 
            quote = FALSE, file = "DATA/DATA FOR R/FRET/FRET MEDIA/DATA_A/IDRFT_media_Rep1.P1N.")



#Función para generar espectros----

#Función para generar los gráficos con las 
#construcciones de interés
spectra_fret <- function(x) {

  #Evaluar el valor de columna a elegir para graficar
  ifelse (x == c(1:32)[-seq(1,32,4)], 
          warning("La columna no pertenece a la\n misma construcción", 
                  call. = FALSE), 
          print("Generando gráfico..."))
  
  #Variable temporal para determinar la escala del eje
  #de las ordenadas
  temp <- c()
  temp[length(temp) + 1] <- max(temp_fret_N[,x+1])
  temp[length(temp) + 1] <- max(temp_fret_N[,x+2])
  temp[length(temp) + 1] <- max(temp_fret_N[,x+3])
  temp[length(temp) + 1] <- max(temp_fret_N[,x+4])
  
  #Generar gráfico con las condiciones
  #anteriores (columnas)
  plot <- ggplot() + 
    #NaCl_0M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", x)),
                  color = "0"), size = 0.8) +
    #NaCl_0.5M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", x+1)),
                  color = "0.5"), size = 0.8) +
    #NaCl_1M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", x+2)),
                  color = "1"), size = 0.8) +
    #NaCl_1.5M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", x+3)),
                  color = "1.5"), size = 0.8) +
    #Modificaciones extra
    theme_classic() +
    labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    coord_cartesian(ylim = c(0.3, max(temp))) +
    scale_y_continuous(breaks = seq(0.3, max(temp),0.2)) +
    scale_color_manual(name = "[NaCl] (M)",
                       values = c("#ADD8E6", "#87CEFA", 
                                  "#6495ED", "#0818A8"))
  #Generar el gráfico
  return(plot)

}

#Colocar los datos. Solo se admiten lo numeros
#1, 5, 9, 13, 17, 21, 25 y 29 si se emplearon
#ocho construcciones

#Para cada valor de "x", corresponde una construcción
#o biosensor distinto. 
#Para x = 1, construct = 1
#Para x = 5, construct = 2
#Para x = 13, construct = 3

spectra_fret(x = 25)

#Guardar gráficos individualmente

ggsave(plot = plot, filename = "DATA/DATA FOR R/FRET/FRET ALTA/PLOTS/IDRFT_alta_141.png", 
       device = "png", width = 8, height = 5, units = "in", 
       dpi = 650)


ggsave(filename = "DATA/DATA FOR R/FRET/FRET ALTA/PLOTS/IDRFT_alta_081.png", 
       device = "png", width = 8, height = 5, units = "in", 
       dpi = 650)

ggsave(plot = plot, filename = "DATA/DATA FOR R/FRET/FRET ALTA/PLOTS/IDRFT_alta_140.png", 
       device = "png", width = 8, height = 5, units = "in", 
       dpi = 650)


#Guardar todos los gráficos generados en una carpeta
#única con un "for loop"

#Obtener las columnas que se van a graficar solamente
i <- (1:length(temp_fret_N[,-1]))[c(1,5,9,13,17,21,25,29)]
h <-!is.na((1:length(temp_fret_N[,-1]))[c(1,5,9,13,17,21,25,29)])

temp_vec <- (i[h])
#Generar los gráficos condicionalmente
for (a in temp_vec) {

  #Guardar columnas a graficar
  temp <- temp_fret_N[,c(a+1,a+2,a+3,a+4)] 
  #Generar el gráfico 
  ggplot() + 
    #NaCl_0M
    geom_line(data = temp, 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", a)),
                  color = "0"), size = 0.8) +
    #NaCl_0.5M
    geom_line(data = temp, 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", a+1)),
                  color = "0.5"), size = 0.8) +
    #NaCl_1M
    geom_line(data = temp, 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", a+2)),
                  color = "1"), size = 0.8) +
    #NaCl_1.5M
    geom_line(data = temp, 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", a+3)),
                  color = "1.5"), size = 0.8) +
    #Modificaciones extra
    theme_classic() +
    labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    coord_cartesian(ylim = c(0.3, max(temp))) +
    scale_y_continuous(breaks = seq(0.3, max(temp),0.2)) +
    scale_color_manual(name = "[NaCl] (M)",
                       values = c("#ADD8E6", "#87CEFA", 
                                  "#6495ED", "#0818A8"))
  
  #Guardar gráfico
  temp_name <- paste0("IDRFT_media_", a)
  ggsave(plot = plot, filename = paste0("DATA/DATA FOR R/FRET/FRET MEDIA/PLOTS/", temp_name), 
         device = "png", width = 8, height = 5, units = "in", 
         dpi = 650)

}


#Obtención cociente DxAm y DxDm----

#Obtener los datos de fluorescencia del donador a 475





#Obtener los datos de fluorescencia del aceptor a 525









