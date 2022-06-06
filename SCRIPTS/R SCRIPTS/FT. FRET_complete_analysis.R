#Paqueterías----

library(ggplot2)
library(ggpubr)

#Cargar archivo FRET

#cargar el archivo solo con la hoja 1 (datos) --revisar--
fret <- readxl::read_xlsx(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/FRET/IDRFT_REP1_P1.ALTA.xlsx", 
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



#Obtención cociente DxAm y DxDm----

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe


#Extraer la cantidad de constructos que tienen los datos
length(fret[,-c(1,2)])/12 #Cantidad de constructos
seq(1,length(fret[,-c(1,2)]),12) #Columnas de cada constructo

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/8), 
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
temp_vec <- temp_df[seq(2, 16, by = 2),-1]
for (a in 1:nrow(temp_df[seq(2, 16, by = 2),-1])) {
  
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


#Para las demás replicas, repetir lo mismo con cada uno
#de los archivos

#REPLICA 2----


#Cargar archivo FRET

#cargar el archivo solo con la hoja 1 (datos) --revisar--
fret <- readxl::read_xlsx(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/FRET/IDRFT_REP1_P1.ALTA.xlsx", 
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





#Obtención cociente DxAm y DxDm----

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/8), 
                             nrow = 0))

#Obtener los renglones con las fluorescencias del donador y
#aceptor
temp_rep2 <- fret[fret$`Wavelength [nm]` == "475" | 
                    fret$`Wavelength [nm]` == "525",]
temp_vec <- c() #vector vacío
#Ejecutar código
for (a in seq(1,length(fret[,-c(1,2)]),12)) {
  
  temp_vec <- temp_rep2[,seq(a+2,a+13)]
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

temp_rep2 <- data.frame(matrix(ncol = 7, 
                               nrow = (12*length(fret[,-c(1,2)])/12)))

#Agregar los nombres a las columnas
names(temp_rep2)[1] <- "Tratamiento"
names(temp_rep2)[2] <- "DxAm"
names(temp_rep2)[3] <- "DxDm"
names(temp_rep2)[4] <- "DxAm/DxDm"
names(temp_rep2)[5] <- "Promedio"
names(temp_rep2)[6] <- "Normalización"
names(temp_rep2)[7] <- "Replica"

#Agregar columna de tratamiento
temp_rep2$Tratamiento <- rep(c(0,0.5,1,1.5), each = 3)

#Agregar columna de DxAm. Extraer de "temp_df" todos los datos
#de longitud de onda 525 en orden

temp_vec_525 <- c() #vector vacío
temp_vec <- temp_df[seq(2, 16, by = 2),-1]
for (a in 1:nrow(temp_df[seq(2, 16, by = 2),-1])) {
  
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
temp_rep2$DxAm <- temp_vec_525
temp_rep2$DxDm <- temp_vec_475

#Agregar el cociente DxAm/DxDm
temp_rep2$`DxAm/DxDm` <- temp_vec_525/temp_vec_475


#Agregar el promedio de DxAM/DxDm del tratamiento a 
#0 NaCl para cada constructo 

#Extraer todos los datos para NaCl 0 de cada constructo
#y sumarlos
h <- temp_rep2[seq(1, nrow(temp_rep2), 12),4]
i <- temp_rep2[(seq(1, nrow(temp_rep2), 12) + 1),4]
j <- temp_rep2[(seq(1, nrow(temp_rep2), 12) + 2),4]

#Sumar todos los datos y dividirlos entre tres. Añadir
#cada valor del vector cada 12 veces para que corresponda
#con el valor de cada constructo a NaCl 0M
temp_rep2$Promedio <- rep((h + i + j)/(3), each = 12)

#Dividir cada valor del promedio con el valor de la columna
#DxAm/DxDm para obtener la normalización
temp_rep2$Normalización <- (temp_rep2$`DxAm/DxDm`)/(temp_rep2$Promedio)

#Añadir la replica con la que se está trabajando (revisar)
temp_rep2$Replica <- 2



#REPLICA 3----

#Cargar archivo FRET

#cargar el archivo solo con la hoja 1 (datos) --revisar--
fret <- readxl::read_xlsx(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/FRET/IDRFT_REP1_P1.ALTA.xlsx", 
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


#Obtención cociente DxAm y DxDm----

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/8), 
                             nrow = 0))

#Obtener los renglones con las fluorescencias del donador y
#aceptor
temp_rep3 <- fret[fret$`Wavelength [nm]` == "475" | 
                    fret$`Wavelength [nm]` == "525",]
temp_vec <- c() #vector vacío
#Ejecutar código
for (a in seq(1,length(fret[,-c(1,2)]),12)) {
  
  temp_vec <- temp_rep3[,seq(a+2,a+13)]
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

temp_rep3 <- data.frame(matrix(ncol = 7, 
                               nrow = (12*length(fret[,-c(1,2)])/12)))

#Agregar los nombres a las columnas
names(temp_rep3)[1] <- "Tratamiento"
names(temp_rep3)[2] <- "DxAm"
names(temp_rep3)[3] <- "DxDm"
names(temp_rep3)[4] <- "DxAm/DxDm"
names(temp_rep3)[5] <- "Promedio"
names(temp_rep3)[6] <- "Normalización"
names(temp_rep3)[7] <- "Replica"

#Agregar columna de tratamiento
temp_rep3$Tratamiento <- rep(c(0,0.5,1,1.5), each = 3)

#Agregar columna de DxAm. Extraer de "temp_df" todos los datos
#de longitud de onda 525 en orden

temp_vec_525 <- c() #vector vacío
temp_vec <- temp_df[seq(2, 16, by = 2),-1]
for (a in 1:nrow(temp_df[seq(2, 16, by = 2),-1])) {
  
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
temp_rep3$DxAm <- temp_vec_525
temp_rep3$DxDm <- temp_vec_475

#Agregar el cociente DxAm/DxDm
temp_rep3$`DxAm/DxDm` <- temp_vec_525/temp_vec_475


#Agregar el promedio de DxAM/DxDm del tratamiento a 
#0 NaCl para cada constructo 

#Extraer todos los datos para NaCl 0 de cada constructo
#y sumarlos
h <- temp_rep3[seq(1, nrow(temp_rep3), 12),4]
i <- temp_rep3[(seq(1, nrow(temp_rep3), 12) + 1),4]
j <- temp_rep3[(seq(1, nrow(temp_rep3), 12) + 2),4]

#Sumar todos los datos y dividirlos entre tres. Añadir
#cada valor del vector cada 12 veces para que corresponda
#con el valor de cada constructo a NaCl 0M
temp_rep3$Promedio <- rep((h + i + j)/(3), each = 12)

#Dividir cada valor del promedio con el valor de la columna
#DxAm/DxDm para obtener la normalización
temp_rep3$Normalización <- (temp_rep3$`DxAm/DxDm`)/(temp_rep3$Promedio)

#Añadir la replica con la que se está trabajando (revisar)
temp_rep3$Replica <- 3


#Juntar dataframes
fret_all <- rbind(temp_rep1, temp_rep2, temp_rep3)



#Creación de gráficos individuales----

#Para graficar, dependerá de la cantidad de 
#constructos. Cada constructo en el archivo 
#fret_all comienza en renglones distintos


fret_boxplot <- function(rep1, rep2, rep3, name_plot, 
                         name_box, save_box, dir_box) {
#Análisis estadístico
  
  #Extraer los datos estadísticos por comparar
  t1 <- c(rep1:rep1:(rep1+12)-1, rep2:rep2:(rep2+12)-1, rep3:rep3:(rep3+12)-1)
  
  #Prueba t-test para 0M y 0.5M
  p1 <- t.test(fret_all[t1,][fret_all[t1,]$Tratamiento == 0,]$Normalización, 
          fret_all[t1,][fret_all[t1,]$Tratamiento == 0.5,]$Normalización)
  #Prueba t-test para 0M y 1.0M
  p2 <- t.test(fret_all[t1,][fret_all[t1,]$Tratamiento == 0,]$Normalización, 
          fret_all[t1,][fret_all[t1,]$Tratamiento == 1.0,]$Normalización)
  #Prueba t-test para 0M y 1.5M
  p3 <- t.test(fret_all[t1,][fret_all[t1,]$Tratamiento == 0,]$Normalización, 
          fret_all[t1,][fret_all[t1,]$Tratamiento == 1.5,]$Normalización)
  
  #Nivel de significancia
  signif_level <- c("****" = 1e-04, "***" = 0.001,
                    "**" = 0.01, "*" = 0.05, ns = 1)
  
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
  
#Gráfico de cajas  
plot <- ggplot() + 
  geom_boxplot(data = fret_all[c(rep1:(rep1+12)-1, 
                                 rep2:(rep2+12)-1, 
                                 rep3:(rep3+12)-1),c(1,6)], 
               aes(x = Tratamiento, 
                   y = Normalización, group = Tratamiento),
               fill = c("#53AEF5", "#538CF5", "#535DF5", "#3E04F2")) +
  geom_jitter(data = fret_all[c(rep1:rep1:(rep1+12)-1, 
                                rep2:rep2:(rep2+12)-1, 
                                rep3:rep3:(rep3+12)-1), c(1,6)], 
              aes(x = Tratamiento, 
                  y = Normalización, group = Tratamiento)) +
  labs(x = "[NaCl] (M)", y = "DxAm/DxDm\nNormalizado") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(1,2)) +
  scale_y_continuous(breaks = seq(1,2,0.25)) +
  annotate(geom = "text", x = 0.5, y = 1.28, label = p1_box) +
  annotate(geom = "text", x = 1, y = 1.53, label = p2_box) +
  annotate(geom = "text", x = 1.5, y = 1.41, label = p3_box)  +
  annotate(geom = "text", x = 0.75, y = 2, label = name_plot,
           size = 5)
#Guardar gráfico
if (save_box == TRUE) {
  
  ggsave(plot = plot, filename = file.path(paste0(dir_box, name_box, ".png")),
         device = "png", width = 12, height = 8, units = "in", 
         dpi = 600) 
  
} else 
  
  return(plot)

}  


fret_boxplot(rep1 = 1)

fret_boxplot(rep1 = 1, 
             save_box = TRUE, name_box = "IDRFT_094_P1", dir_box = "PLOTS/")

fret_boxplot(rep1 = 1, rep2 = 97, rep3 = 193, 
             name_box = "media", save_box = FALSE)


#IDRBS-144
h <- ggplot() + 
  geom_boxplot(data = temp[73:84,c(1,6)], 
               aes(x = Tratamiento, 
                   y = Normalización, group = Tratamiento),
               fill = c("#53AEF5", "#538CF5", "#535DF5", "#3E04F2")) +
  labs(x = "[NaCl] (M)", y = "DxAm/DxDm\nNormalizado") +
  theme_classic() +
  theme(axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(1,2)) +
  scale_y_continuous(breaks = seq(1,2,0.25)) +
  annotate(geom = "text", x = 0.5, y = 1.6, label = "****") +
  annotate(geom = "text", x = 1, y = 1.81, label = "****") +
  annotate(geom = "text", x = 1.5, y = 1.65, label = "***") +
  annotate(geom = "text", x = 0.75, y = 2, label = "IDRBS-144",
           size = 5)


#Realizar anova de una vía
summary(aov(data = temp[73:84,], Normalización ~ Tratamiento))

#Comparar valores de p de cada tratamiento comparado con el
#tratamiento a NaCl 0M

p1 <-t.test(fret_all[73:84,][fret_all[73:84,]$Tratamiento == 0,]$Normalización, 
       fret_all[73:84,][fret_all[73:84,]$Tratamiento == 0.5,]$Normalización)

t.test(temp[73:84,][temp[73:84,]$Tratamiento == 0,]$Normalización, 
       temp[73:84,][temp[73:84,]$Tratamiento == 1,]$Normalización)

t.test(temp[73:84,][temp[73:84,]$Tratamiento == 0,]$Normalización, 
       temp[73:84,][temp[73:84,]$Tratamiento == 1.5,]$Normalización)



comparisons <- list(c("0.5", "0"), c("1", "0"), c("1.5", "0"))
comparisons

stat_compare_means(comparisons = comparisons)



#IDRBS-137
i <- ggplot() + 
  geom_boxplot(data = temp[37:48,c(1,6)], 
               aes(x = Tratamiento, 
                   y = Normalización, group = Tratamiento),
               fill = c("#53AEF5", "#538CF5", "#535DF5", "#3E04F2")) +
  labs(x = "[NaCl] (M)", y = "DxAm/DxDm Normalizado") +
  theme_classic() +
  theme(axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(1,2)) +
  scale_y_continuous(breaks = seq(1,2,0.25)) +
  annotate(geom = "text", x = 0.5, y = 1.13, label = "*") +
  annotate(geom = "text", x = 1, y = 1.29, label = "***") +
  annotate(geom = "text", x = 1.5, y = 1.24, label = "***")  +
  annotate(geom = "text", x = 0.75, y = 2, label = "IDRBS-137",
           size = 5)


#Realizar anova de una vía
summary(aov(data = temp[37:48,], Normalización ~ Tratamiento))

#Comparar valores de p de cada tratamiento comparado con los
#diferentes tratamientos

t.test(temp[37:48,][temp[37:48,]$Tratamiento == 0,]$Normalización, 
       temp[37:48,][temp[37:48,]$Tratamiento == 0.5,]$Normalización)

t.test(temp[37:48,][temp[37:48,]$Tratamiento == 0,]$Normalización, 
       temp[37:48,][temp[37:48,]$Tratamiento == 1,]$Normalización)

t.test(temp[37:48,][temp[37:48,]$Tratamiento == 0,]$Normalización, 
       temp[37:48,][temp[37:48,]$Tratamiento == 1.5,]$Normalización)

