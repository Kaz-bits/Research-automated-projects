#Paqueterías----

library(ggplot2)
library(ggpubr)

#Cargar archivo FRET

#cargar el archivo solo con la hoja 1 (datos) --revisar--
fret <- readxl::read_xlsx(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/FRET/IDRFT_alta. Rep1. P1.xlsx", 
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


#Guardar archivo
#write.table(x = temp_fret_N, 
 #           file = "DATA/DATA FOR R/FRET/Data_FRET/IDRFT_baja_P1.R1.txt",
  #          sep = ",", quote = FALSE, row.names = FALSE)


#Obtención cociente DxAm y DxDm----

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe


#Extraer la cantidad de constructos que tienen los datos
length(fret[,-c(1,2)])/12 #Cantidad de constructos
seq(1,length(fret[,-c(1,2)]),12) #Columnas de cada constructo

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/(length(fret[,-c(1,2)])/12)), 
                             nrow = 0))

#Obtener los renglones con las fluorescencias del donador y
#aceptor
temp_rep1 <- fret[fret$`Wavelength [nm]` == "475" | 
                    fret$`Wavelength [nm]` == "525",]
temp_vec <- c() #vector vacío
#Ejecutar código
for (a in seq(1,length(fret[,-c(1,2)]),12)) {
  
  temp_vec <- temp_rep1[,seq(a+2,a+13)]
  temp_df[nrow(temp_df) + 1,] <- temp_vec[1,] 
  temp_df[nrow(temp_df) + 1,] <- temp_vec[2,] 
  
}


temp_df[,length(temp_df) + 1] <- c(475, 525)
temp_df <- temp_df[,c(length(temp_df),(1:length(temp_df)-1))]
names(temp_df)[1] <- names(fret)[2]
temp_name <- c(rep(paste0("NaCl_", "0M"),3), rep(paste0("NaCl_", "0.5M"),3),
               rep(paste0("NaCl_", "1M"),3), rep(paste0("NaCl_", "1.5M"),3))
#Agreagr nombres a las columnas por condición
names(temp_df)[2:length(temp_df)] <- temp_name

#Una vez obtenido el dataframe con los datos de
#fluorescencia del donador y aceptor, realizar
#la normalización

#Generar dataframe para los datos normalizados

temp_rep1 <- data.frame(matrix(ncol = 7, 
                               nrow = (12*length(fret[,-c(1,2)])/12)))

#Agregar los nombres a las columnas
names(temp_rep1)[1] <- "Tratamiento"
names(temp_rep1)[2] <- "DxAm"
names(temp_rep1)[3] <- "DxDm"
names(temp_rep1)[4] <- "DxAm/DxDm"
names(temp_rep1)[5] <- "Promedio"
names(temp_rep1)[6] <- "Normalización"
names(temp_rep1)[7] <- "Replica"

#Agregar columna de tratamiento
temp_rep1$Tratamiento <- rep(c(0,0.5,1,1.5), each = 3)

#Agregar columna de DxAm. Extraer de "temp_df" todos los datos
#de longitud de onda 525 en orden

temp_vec_525 <- c() #vector vacío
temp_vec <- temp_df[seq(2, nrow(temp_df), by = 2),-1]
for (a in 1:nrow(temp_df[seq(2, nrow(temp_df), by = 2),-1])) {
  
  temp_vec_525 <- c(temp_vec_525, as.numeric(temp_vec[a,]))
  
}


#Agregar columna de DxDm. Extraer de "temp_df" todos los datos
#de longitud de onda 475 en orden

temp_vec_475 <- c() #vector vacío
temp_vec <- temp_df[seq(1, nrow(temp_df), by = 2),-1]
for (a in 1:nrow(temp_df[seq(1, nrow(temp_df), by = 2),-1])) {
  
  temp_vec_475 <- c(temp_vec_475, as.numeric(temp_vec[a,]))
  
}


#Añadir columnas finales
temp_rep1$DxAm <- temp_vec_525
temp_rep1$DxDm <- temp_vec_475

#Agregar el cociente DxAm/DxDm
temp_rep1$`DxAm/DxDm` <- temp_vec_525/temp_vec_475


#Agregar el promedio de DxAM/DxDm del tratamiento a 
#0 NaCl para cada constructo 

#Extraer todos los datos para NaCl 0 de cada constructo
#y sumarlos
h <- temp_rep1[seq(1, nrow(temp_rep1), 12),4]
i <- temp_rep1[(seq(1, nrow(temp_rep1), 12) + 1),4]
j <- temp_rep1[(seq(1, nrow(temp_rep1), 12) + 2),4]

#Sumar todos los datos y dividirlos entre tres. Añadir
#cada valor del vector cada 12 veces para que corresponda
#con el valor de cada constructo a NaCl 0M
temp_rep1$Promedio <- rep((h + i + j)/(3), each = 12)

#Dividir cada valor del promedio con el valor de la columna
#DxAm/DxDm para obtener la normalización
temp_rep1$Normalización <- (temp_rep1$`DxAm/DxDm`)/(temp_rep1$Promedio)

#Añadir la replica con la que se está trabajando (revisar)
temp_rep1$Replica <- 1



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
    theme(axis.title = element_blank(),
          #axis.title.x = element_text(size = 14),
          axis.text = element_text(size = 12),
          #legend.title = element_text(size = 14),
          #legend.text = element_text(size = 12),
          legend.position = "none",
          panel.grid = element_blank(),) +
    coord_cartesian(ylim = c(0.3, max(temp)+0.6)) +
    scale_y_continuous(breaks = seq(0.3, max(temp)+0.6,0.2)) +
    scale_color_manual(name = "[NaCl] (M)",
                       values = c("#ADD8E6", "#87CEFA", 
                                  "#6495ED", "#0818A8")) +
    annotate(geom = "text", x = 505, y = 1.6, 
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

#Para cada valor de "x", corresponde una construcción
#o biosensor distinto. 
#Para x = 1, construct = 1
#Para x = 5, construct = 2
#Para x = 13, construct = 3

#Alta
a1 <- spectra_fret(construct = 1, save_spectra = F, 
             name_spectra = "IDRFT_044_spectra", 
             name_plot = "IDRBS-044",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a2 <- spectra_fret(construct = 5, save_spectra = F, 
             name_spectra = "IDRFT_081_spectra",
             name_plot = "IDRBS-081",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a3 <- spectra_fret(construct = 9, save_spectra = F, 
             name_spectra = "IDRFT_082_spectra", 
             name_plot = "IDRBS-082",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a4 <- spectra_fret(construct = 13, save_spectra = F, 
             name_spectra = "IDRFT_137_spectra",
             name_plot = "IDRBS-137",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a5 <- spectra_fret(construct = 17, save_spectra = F, 
             name_spectra = "IDRFT_138_spectra",
             name_plot = "IDRBS-138",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a6 <- spectra_fret(construct = 21, save_spectra = F, 
             name_spectra = "IDRFT_143_spectra",
             name_plot = "IDRBS-143",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a7 <- spectra_fret(construct = 25, save_spectra = F, 
             name_spectra = "IDRFT_144_spectra",
             name_plot = "IDRBS-144",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a8 <- spectra_fret(construct = 29, save_spectra = F, 
             name_spectra = "IDRFT_145_spectra",
             name_plot = "IDRBS-145",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a9 <- spectra_fret(construct = 1, save_spectra = FALSE, 
             name_spectra = "IDRFT_146_spectra", 
             name_plot = "IDRBS-146",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
a10 <- spectra_fret(construct = 5, save_spectra = FALSE, 
             name_spectra = "IDRFT_165_spectra",
             name_plot = "IDRBS-165",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")

#Media
m1 <- spectra_fret(construct = 5, save_spectra = F, 
             name_spectra = "IDRFT_053_spectra",
             name_plot = "IDRBS-053",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m2 <- spectra_fret(construct = 9, save_spectra = F, 
             name_spectra = "IDRFT_054_spectra",
             name_plot = "IDRBS-054",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m3 <- spectra_fret(construct = 13, save_spectra = F, 
             name_spectra = "IDRFT_064_spectra",
             name_plot = "IDRBS-064",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m4 <- spectra_fret(construct = 17, save_spectra = F, 
             name_spectra = "IDRFT_077_spectra",
             name_plot = "IDRBS-077",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m5 <- spectra_fret(construct = 21, save_spectra = F, 
             name_spectra = "IDRFT_136_spectra", 
             name_plot = "IDRBS-136",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m6 <- spectra_fret(construct = 25, save_spectra = F, 
             name_spectra = "IDRFT_140_spectra",
             name_plot = "IDRBS-140",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m7 <- spectra_fret(construct = 29, save_spectra = F, 
             name_spectra = "IDRFT_141_spectra",
             name_plot = "IDRBS-141",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m8 <- spectra_fret(construct = 1, save_spectra = F, 
             name_spectra = "IDRFT_142_spectra",
             name_plot = "IDRBS-142",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m9 <- spectra_fret(construct = 5, save_spectra = F, 
             name_spectra = "IDRFT_147_spectra",
             name_plot = "IDRBS-147",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m10 <- spectra_fret(construct = 9, save_spectra = F, 
             name_spectra = "IDRFT_148_spectra",
             name_plot = "IDRBS-148",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
m11 <- spectra_fret(construct = 13, save_spectra = F, 
             name_spectra = "IDRFT_149_spectra",
             name_plot = "IDRBS-149",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
#Baja
b1 <- spectra_fret(construct = 9, save_spectra = F, 
             name_spectra = "IDRFT_016_spectra",
             name_plot = "IDRBS-016",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
b2 <- spectra_fret(construct = 13, save_spectra = F, 
             name_spectra = "IDRFT_045_spectra",
             name_plot = "IDRBS-045",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")
b3 <- spectra_fret(construct = 17, save_spectra = F, 
             name_spectra = "IDRFT_139_spectra",
             name_plot = "IDRBS-139",
             dir_spectra = "PLOTS/FRET/Spectrum/Spectrum_new/")

#Juntar gráficos

fig_spectrum <- ggarrange(a2,a3,a4,a5,a6,a7,a8,
                          a9 + rremove("ylab"),a10,
                          m2,m3,m4,m5,m6,m7,m8,m9,
                          m10 + rremove("xlab"),
                          m11,b2,b3, 
                          nrow = 3, ncol = 7, 
                          common.legend = TRUE, 
                          legend = "right")

fig_spectrum <- annotate_figure(fig_spectrum, 
                left = textGrob("normalized fluorescence", 
                                vjust = 0.5, rot = 90, 
                                gp = gpar(cex = 2.5)),
                bottom = textGrob("wavelength (nm)", 
                                  gp = gpar(cex = 2.5), 
                                  hjust = 0.5))

ggsave(plot = fig_spectrum, filename = "PLOTS/fig_spectrum.pdf",
       device = "pdf", width = 18, height = 11, units = "in",
       dpi = 600)





#Cargar los archivos a utilizar generados anteriormente

fret_N_alta1 <- read.table(file = "DATA/DATA FOR R/FRET/Data_FRET/IDRFT_alta_P1.R1.txt", 
           header = TRUE, sep = ",")

fret_N_alta2 <- read.table(file = "DATA/DATA FOR R/FRET/Data_FRET/IDRFT_alta_P2.R1.txt", 
           header = TRUE, sep = ",")

fret_N_media1 <- read.table(file = "DATA/DATA FOR R/FRET/Data_FRET/IDRFT_media_P1.R1.txt", 
                           header = TRUE, sep = ",")

fret_N_media2 <- read.table(file = "DATA/DATA FOR R/FRET/Data_FRET/IDRFT_media_P2.R1.txt", 
                            header = TRUE, sep = ",")

fret_N_baja1 <- read.table(file = "DATA/DATA FOR R/FRET/Data_FRET/IDRFT_baja_P1.R1.txt", 
                            header = TRUE, sep = ",")

#Generar gráficos

construc_names

ggplot() + 
  #NaCl_0M
  geom_line(data = fret_N_alta1[seq(1,nrow(fret_N_alta1),5),], 
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
  theme_classic() +
  labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0.3, max(temp)+0.4)) +
  scale_y_continuous(breaks = seq(0.3, max(temp)+0.4,0.2)) +
  scale_color_manual(name = "[NaCl] (M)",
                     values = c("#ADD8E6", "#87CEFA", 
                                "#6495ED", "#0818A8")) +
  annotate(geom = "text", x = 515, y = 1.6, 
           label = name_plot, size = 4.5)

















#Para graficar, dependerá de la cantidad de 
#constructos. Cada constructo en el archivo 
#fret_all comienza en renglones distintos

fret_boxplot <- function(rep1, name_plot, save_box, 
                         name_box, dir_box) {
  #Análisis estadístico
  
  #Extraer los datos estadísticos por comparar
  t1 <- c(rep1:(rep1+12-(1)))[-length(c(rep1:(rep1+12)))]
  
  #Prueba t-test para 0M y 0.5M
  p1 <- t.test(temp_rep1[t1,][temp_rep1[t1,][,1] == 0,][,6],
               temp_rep1[t1,][temp_rep1[t1,][,1] == 0.5,][,6])[3][[1]]
  #Prueba t-test para 0M y 1.0M
  p2 <- t.test(temp_rep1[t1,][temp_rep1[t1,][,1] == 0,][,6],
               temp_rep1[t1,][temp_rep1[t1,][,1] == 1,][,6])[3][[1]]
  #Prueba t-test para 0M y 1.5M
  p3 <- t.test(temp_rep1[t1,][temp_rep1[t1,][,1] == 0,][,6],
               temp_rep1[t1,][temp_rep1[t1,][,1] == 1.5,][,6])[3][[1]]
  
  #Nivel de significancia
  signif_level <- c(ns = 1, "*" = 0.05, "**" = 0.01, 
                    "***" = 0.001, "****" = 0.0001)
  
  #Obtener los valores de p de cada concentración
  a <- c(p1, p2, p3)
  
  #Evaluar el valor de p de las concentraciones
  #Para 0.5 vs 0.0
  i <- c(a[1] > 0.05, a[1] <= 0.05, a[1] <= 0.01, a[1] <= 0.001, a[1] <= 0.0001)
  #Para 1.0 vs 0.0
  h <- c(a[2] > 0.05, a[2] <= 0.05, a[2] <= 0.01, a[2] <= 0.001, a[2] <= 0.0001)
  #Para 1.5 vs 0.0
  j <- c(a[3] > 0.05, a[3] <= 0.05, a[3] <= 0.01, a[3] <= 0.001, a[3] <= 0.0001)
  
  #Extraer los simbolos de significancia del valor de probabilidad
  p1_box <- (names(signif_level[i]))[length((names(signif_level[i])))] #p1
  p2_box <- (names(signif_level[h]))[length((names(signif_level[h])))] #p2
  p3_box <- (names(signif_level[j]))[length((names(signif_level[j])))] #p3
  
  #Obtener las medias de los valores para colocar los p_value
  #Media de 0.5M
  k <- temp_rep1[c(rep1:(rep1+12-(1))),c(1,6)]
  median_0.5 <- median(k[(temp_rep1[c(rep1:(rep1+12-(1))), c(1,6)] == 0.5),][,2])
  
  #Media de 1.0M
  k <- temp_rep1[c(rep1:(rep1+12-(1))),c(1,6)]
  median_1 <- median(k[(temp_rep1[c(rep1:(rep1+12-(1))), c(1,6)] == 1),][,2])

  #Media de 1.5M
  k <- temp_rep1[c(rep1:(rep1+12-(1))),c(1,6)]
  median_1.5 <- median(k[(temp_rep1[c(rep1:(rep1+12-(1))), c(1,6)] == 1.5),][,2])
  
  #Gráfico de cajas  
  plot <- ggplot() + 
    geom_boxplot(data = temp_rep1[c(rep1:(rep1+12-(1))), c(1,6)], 
                 aes(x = Tratamiento, 
                     y = Normalización, group = Tratamiento),
                 fill = c("#53AEF5", "#538CF5", "#535DF5", "#3E04F2")) +
    geom_jitter(data = temp_rep1[c(rep1:rep1:(rep1+12-(1))), c(1,6)], 
                aes(x = Tratamiento, 
                    y = Normalización, group = Tratamiento)) +
    labs(x = "[NaCl] (M)", y = "DxAm/DxDm\nNormalizado") +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 12),
          panel.grid = element_blank()) +
    coord_cartesian(ylim = c(0.85,2)) +
    scale_y_continuous(breaks = seq(0.85,2,0.25)) +
    annotate(geom = "text", x = 0.5, y = 1.75, 
             label = p1_box, size = 4.5) +
    annotate(geom = "text", x = 1, y = 1.75, 
             label = p2_box, size = 4.5) +
    annotate(geom = "text", x = 1.5, y = 1.75, 
             label = p3_box, size = 4.5)  +
    annotate(geom = "text", x = 0.75, y = 1.95, label = name_plot,
             size = 5)
  #Guardar gráfico
  if (save_box == TRUE) {
    
    ggsave(plot = plot, filename = file.path(paste0(dir_box, name_box, ".png")),
           device = "png", width = 6, height = 4, units = "in", 
           dpi = 600) 
    
  } else 
    
    return(plot)
  
}  

fret_boxplot(rep1 = 37, name_plot = "IDRBS-064", save_box = TRUE, 
             dir_box = "PLOTS/FRET/Boxplots/", 
             name_box = "IDRFT_064_box")
fret_boxplot(rep1 = 61, name_plot = "IDRBS-143", save_box = TRUE, 
             dir_box = "PLOTS/FRET/Boxplots/", 
             name_box = "IDRFT_143_box")
fret_boxplot(rep1 = 1, name_plot = "IDRBS-146", save_box = TRUE, 
             dir_box = "PLOTS/FRET/Boxplots/", 
             name_box = "IDRFT_146_box")


fret_boxplot(rep1 = 49, name_plot = "IDRBS-044", save_box = FALSE)
#Alta
d1 <- fret_boxplot(rep1 = 1, name_plot = "IDRBS-044", save_box = FALSE)
d2 <- fret_boxplot(rep1 = 13, name_plot = "IDRBS-081", save_box = FALSE)
d3 <- fret_boxplot(rep1 = 25, name_plot = "IDRBS-082", save_box = FALSE)
d4 <- fret_boxplot(rep1 = 37, name_plot = "IDRBS-137", save_box = FALSE)
d5 <- fret_boxplot(rep1 = 49, name_plot = "IDRBS-138", save_box = FALSE)
d6 <- fret_boxplot(rep1 = 61, name_plot = "IDRBS-143", save_box = FALSE)
d7 <- fret_boxplot(rep1 = 73, name_plot = "IDRBS-144", save_box = FALSE)
d8 <- fret_boxplot(rep1 = 85, name_plot = "IDRBS-145", save_box = FALSE)
d9 <- fret_boxplot(rep1 = 1, name_plot = "IDRBS-146", save_box = FALSE)
d10 <- fret_boxplot(rep1 = 13, name_plot = "IDRBS-165", save_box = FALSE)

#Media
e1 <- fret_boxplot(rep1 = 13, name_plot = "IDRBS-053", save_box = FALSE)
e2 <- fret_boxplot(rep1 = 25, name_plot = "IDRBS-054", save_box = FALSE)
e3 <- fret_boxplot(rep1 = 37, name_plot = "IDRBS-064", save_box = FALSE)
e4 <- fret_boxplot(rep1 = 49, name_plot = "IDRBS-077", save_box = FALSE)
e5 <- fret_boxplot(rep1 = 61, name_plot = "IDRBS-136", save_box = FALSE)
e6 <- fret_boxplot(rep1 = 73, name_plot = "IDRBS-140", save_box = FALSE)
e7 <- fret_boxplot(rep1 = 85, name_plot = "IDRBS-141", save_box = FALSE)
e8 <- fret_boxplot(rep1 = 1, name_plot = "IDRBS-142", save_box = FALSE)
e9 <- fret_boxplot(rep1 = 13, name_plot = "IDRBS-147", save_box = FALSE)
e10 <- fret_boxplot(rep1 = 25, name_plot = "IDRBS-148", save_box = FALSE)
e11 <- fret_boxplot(rep1 = 37, name_plot = "IDRBS-149", save_box = FALSE)

#Baja
f1 <- fret_boxplot(rep1 = 25, name_plot = "IDRBS-053", save_box = FALSE)
f2 <- fret_boxplot(rep1 = 37, name_plot = "IDRBS-054", save_box = FALSE)
f3 <- fret_boxplot(rep1 = 49, name_plot = "IDRBS-064", save_box = FALSE)



#Juntar gráficos

fig_boxplot <- ggarrange(d2,d3,d4,d5,d6,d7,d8,d9,d10,
                         e2,e3,e4,e5,e6,e7,e8,e9,e10,
                         e11,f2,f3, 
                         nrow = 3, ncol = 7)

fig_boxplot <- annotate_figure(fig_boxplot,
                left = textGrob("normalized DxAm/DxDm", 
                                vjust = 0.5, rot = 90, 
                                gp = gpar(cex = 2.5)),
                bottom = textGrob("[NaCl] (M)",
                                  gp = gpar(cex = 2.5), 
                                  hjust = 0.5))

#Guardar gráfico de boxplots
ggsave(plot = fig_boxplot, filename = "PLOTS/fig_boxplots.pdf",
       device = "pdf", width = 18, height = 11, units = "in",
       dpi = 600)

