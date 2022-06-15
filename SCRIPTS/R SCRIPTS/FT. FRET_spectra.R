#Paqueterías----

library(ggplot2)
library(ggpubr)

#Cargar archivo FRET
fret <- readxl::read_xlsx(path = "DATA/DATA FOR R/FRET/Data_FRET/IDRFT_REP1_P2. ALTA. N1.xlsx", 
                          sheet = 2, col_names = FALSE)[-c(1:9),]

#Agregar los nombres a las columnas para identificarlas
names(fret)[1:length(names(fret))] <- unlist(fret[2,], use.names = FALSE)

#Eliminar el primer renglón 
fret <- fret[-c(1,2),]

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

#Normalizar fluorescencia con punto isosbéstico

temp <- (t(temp_fret[temp_fret[,1] == as.character(515),
                     c(2:length(temp_fret))]))
rownames(temp) <- NULL

#Emplear el vector "temp" creado para normalizar
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

#Función para generar espectros----

#Función para generar los gráficos con las 
#construcciones de interés
spectra_fret <- function(construct, name_plot, save_spectra, 
                         name_spectra,dir_spectra) {
  
  #Evaluar el valor de columna a elegir para graficar
  ifelse (construct == c(1:32)[-seq(1,32,4)], 
          warning("La columna no pertenece a la\n misma construcción", 
                  call. = FALSE), 
          print("Generando gráfico..."))
  
  #Variable temporal para determinar la escala del eje
  #de las ordenadas
  temp <- c()
  temp[length(temp) + 1] <- max(temp_fret_N[,construct+1])
  temp[length(temp) + 1] <- max(temp_fret_N[,construct+2])
  temp[length(temp) + 1] <- max(temp_fret_N[,construct+3])
  temp[length(temp) + 1] <- max(temp_fret_N[,construct+4])
  
  #Generar gráfico con las condiciones
  #anteriores (columnas)
  plot <- ggplot() + 
    #NaCl_0M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", construct)),
                  color = "0"), size = 0.8) +
    #NaCl_0.5M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", construct+1)),
                  color = "0.5"), size = 0.8) +
    #NaCl_1M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", construct+2)),
                  color = "1"), size = 0.8) +
    #NaCl_1.5M
    geom_line(data = temp_fret_N[seq(1,nrow(temp_fret_N),5),], 
              aes(x = as.numeric(`Wavelength [nm]`), y = get(paste0("NaCl_", construct+3)),
                  color = "1.5"), size = 0.8) +
    #Modificaciones extra
    theme_bw() +
    labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid = element_blank()) +
    coord_cartesian(ylim = c(0.3, 2)) +
    scale_y_continuous(breaks = seq(0.3, 2,0.2)) +
    scale_color_manual(name = "[NaCl] (M)",
                       values = c("#ADD8E6", "#87CEFA", 
                                  "#6495ED", "#0818A8")) +
    annotate(geom = "text", x = 505, y = 1.8, 
             label = name_plot, size = 4.5)
  
  #Guardar gráfico
  if (save_spectra == TRUE) {
    
    ggsave(plot = plot, filename = file.path(paste0(dir_spectra, name_spectra, ".png")),
           device = "png", width = 6, height = 4, units = "in", 
           dpi = 600) 
    
  } else 
    
    return(plot)
}


#Colocar los datos. Solo se admiten lo numeros
#1, 5, 9, 13, 17, 21, 25 y 29 si se emplearon
#ocho construcciones
