#Paqueterías----

library(ggplot2)
library(ggpubr)

#REṔLICA 1----
#cargar el archivo
fret <- readxl::read_xlsx(path = "media/cesar/Backup Plus/FRET Biblioteca/DATOSFLUO/Replicas CONSTANZA/Replica1/001/376.xlsx", 
                          sheet = 1, col_names = FALSE)[-c(1:9),]

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

#Gráficos de cajas (boxplots)----
#Obtención cociente DxAm y DxDm

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Extraer la cantidad de constructos que tienen los datos
i <- length(fret[,-c(1,2)])/12 #Cantidad de constructos
seq(1,length(fret[,-c(1,2)]),12) #Columnas de cada constructo

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/i), 
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

temp_rep1 <- data.frame(matrix(ncol = 7, nrow = (12*length(fret[,-c(1,2)])/12)))

#Agregar los nombres a las columnas
names(temp_rep1)[1] <- "Tratamiento"
names(temp_rep1)[2] <- "DxAm"
names(temp_rep1)[3] <- "DxDm"
names(temp_rep1)[4] <- "DxAm/DxDm"
names(temp_rep1)[5] <- "Promedio"
names(temp_rep1)[6] <- "Normalización"
names(temp_rep1)[7] <- "Replica"

#Agregar columna de tratamiento
temp_rep1$Tratamiento <- rep(c(0,200,400,600), each = 3)

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

#Para las demás replicas, repetir lo mismo con cada uno
#de los archivos

#REPLICA 2----

#cargar el archivo
fret <- readxl::read_xlsx(path = "media/cesar/Backup Plus/FRET Biblioteca/DATOSFLUO/Replicas CONSTANZA/Replica1/001/377.xlsx", 
                          sheet = 1, col_names = FALSE)[-c(1:9),]

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


#Normalizar fluorescencia con punto isosbéstico

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



#Gráficos de cajas (boxplots)----
#Obtención cociente DxAm y DxDm

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Extraer la cantidad de constructos que tienen los datos
i <- length(fret[,-c(1,2)])/12 #Cantidad de constructos

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/i), 
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
temp_rep2$Tratamiento <- rep(c(0,800,1000,1500), each = 3)

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
temp_rep2$Replica <- 1

#REPLICA 3----

#Cargar archivo FRET (réplica 3)
fret <- readxl::read_xlsx(path = "media/cesar/Backup Plus/FRET Biblioteca/DATOSFLUO/Replicas CONSTANZA/Replica2/001/418.xlsx", 
                          sheet = 1, col_names = FALSE)[-c(1:9),]

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



#Gráficos de cajas (boxplots)----
#Obtención cociente DxAm y DxDm

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Extraer la cantidad de constructos que tienen los datos
i <- length(fret[,-c(1,2)])/12 #Cantidad de constructos

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/i), 
                             nrow = 0))

#Obtener los renglones con las fluorescencias del donador y
#aceptor
temp_rep3 <- fret[fret$`Wavelength [nm]` == "475" | 
                    fret$`Wavelength [nm]` == "525",]
temp_vec <- c() #vector vacío
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
temp_rep3$Tratamiento <- rep(c(0,200,400,600), each = 3)

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

#Añadir columnas 
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
temp_rep3$Replica <- 2


#REṔLICA 4----
#cargar el archivo
fret <- readxl::read_xlsx(path = "media/cesar/Backup Plus/FRET Biblioteca/DATOSFLUO/Replicas CONSTANZA/Replica2/001/419.xlsx", 
                          sheet = 1, col_names = FALSE)[-c(1:9),]

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

#Gráficos de cajas (boxplots)----
#Obtención cociente DxAm y DxDm

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Extraer la cantidad de constructos que tienen los datos
i <- length(fret[,-c(1,2)])/12 #Cantidad de constructos
seq(1,length(fret[,-c(1,2)]),12) #Columnas de cada constructo

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/i), 
                             nrow = 0))

#Obtener los renglones con las fluorescencias del donador y
#aceptor
temp_rep4 <- fret[fret$`Wavelength [nm]` == "475" | 
                    fret$`Wavelength [nm]` == "525",]
temp_vec <- c() #vector vacío
#Ejecutar código
for (a in seq(1,length(fret[,-c(1,2)]),12)) {
  
  temp_vec <- temp_rep4[,seq(a+2,a+13)]
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

temp_rep4 <- data.frame(matrix(ncol = 7, nrow = (12*length(fret[,-c(1,2)])/12)))

#Agregar los nombres a las columnas
names(temp_rep4)[1] <- "Tratamiento"
names(temp_rep4)[2] <- "DxAm"
names(temp_rep4)[3] <- "DxDm"
names(temp_rep4)[4] <- "DxAm/DxDm"
names(temp_rep4)[5] <- "Promedio"
names(temp_rep4)[6] <- "Normalización"
names(temp_rep4)[7] <- "Replica"

#Agregar columna de tratamiento
temp_rep4$Tratamiento <- rep(c(0,800,1000,1500), each = 3)

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
temp_rep4$DxAm <- temp_vec_525
temp_rep4$DxDm <- temp_vec_475

#Agregar el cociente DxAm/DxDm
temp_rep4$`DxAm/DxDm` <- temp_vec_525/temp_vec_475

#Agregar el promedio de DxAM/DxDm del tratamiento a 
#0 NaCl para cada constructo 

#Extraer todos los datos para NaCl 0 de cada constructo
#y sumarlos
h <- temp_rep4[seq(1, nrow(temp_rep4), 12),4]
i <- temp_rep4[(seq(1, nrow(temp_rep4), 12) + 1),4]
j <- temp_rep4[(seq(1, nrow(temp_rep4), 12) + 2),4]

#Sumar todos los datos y dividirlos entre tres. Añadir
#cada valor del vector cada 12 veces para que corresponda
#con el valor de cada constructo a NaCl 0M
temp_rep4$Promedio <- rep((h + i + j)/(3), each = 12)

#Dividir cada valor del promedio con el valor de la columna
#DxAm/DxDm para obtener la normalización
temp_rep4$Normalización <- (temp_rep4$`DxAm/DxDm`)/(temp_rep4$Promedio)

#Añadir la replica con la que se está trabajando (revisar)
temp_rep4$Replica <- 2

#Para las demás replicas, repetir lo mismo con cada uno
#de los archivos

#REPLICA 5----

#cargar el archivo
fret <- readxl::read_xlsx(path = "media/cesar/Backup Plus/FRET Biblioteca/DATOSFLUO/Replicas CONSTANZA/Replica3/001/454.xlsx", 
                          sheet = 1, col_names = FALSE)[-c(1:9),]

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


#Normalizar fluorescencia con punto isosbéstico

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



#Gráficos de cajas (boxplots)----
#Obtención cociente DxAm y DxDm

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Extraer la cantidad de constructos que tienen los datos
i <- length(fret[,-c(1,2)])/12 #Cantidad de constructos

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/i), 
                             nrow = 0))

#Obtener los renglones con las fluorescencias del donador y
#aceptor
temp_rep5 <- fret[fret$`Wavelength [nm]` == "475" | 
                    fret$`Wavelength [nm]` == "525",]
temp_vec <- c() #vector vacío
#Ejecutar código
for (a in seq(1,length(fret[,-c(1,2)]),12)) {
  
  temp_vec <- temp_rep5[,seq(a+2,a+13)]
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

temp_rep5 <- data.frame(matrix(ncol = 7, 
                               nrow = (12*length(fret[,-c(1,2)])/12)))

#Agregar los nombres a las columnas
names(temp_rep5)[1] <- "Tratamiento"
names(temp_rep5)[2] <- "DxAm"
names(temp_rep5)[3] <- "DxDm"
names(temp_rep5)[4] <- "DxAm/DxDm"
names(temp_rep5)[5] <- "Promedio"
names(temp_rep5)[6] <- "Normalización"
names(temp_rep5)[7] <- "Replica"

#Agregar columna de tratamiento
temp_rep5$Tratamiento <- rep(c(0,200,400,600), each = 3)

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
temp_rep5$DxAm <- temp_vec_525
temp_rep5$DxDm <- temp_vec_475

#Agregar el cociente DxAm/DxDm
temp_rep5$`DxAm/DxDm` <- temp_vec_525/temp_vec_475


#Agregar el promedio de DxAM/DxDm del tratamiento a 
#0 NaCl para cada constructo 

#Extraer todos los datos para NaCl 0 de cada constructo
#y sumarlos
h <- temp_rep5[seq(1, nrow(temp_rep5), 12),4]
i <- temp_rep5[(seq(1, nrow(temp_rep5), 12) + 1),4]
j <- temp_rep5[(seq(1, nrow(temp_rep5), 12) + 2),4]

#Sumar todos los datos y dividirlos entre tres. Añadir
#cada valor del vector cada 12 veces para que corresponda
#con el valor de cada constructo a NaCl 0M
temp_rep5$Promedio <- rep((h + i + j)/(3), each = 12)

#Dividir cada valor del promedio con el valor de la columna
#DxAm/DxDm para obtener la normalización
temp_rep5$Normalización <- (temp_rep5$`DxAm/DxDm`)/(temp_rep5$Promedio)

#Añadir la replica con la que se está trabajando (revisar)
temp_rep5$Replica <- 3

#REPLICA 6----

#Cargar archivo FRET (réplica 3)
fret <- readxl::read_xlsx(path = "media/cesar/Backup Plus/FRET Biblioteca/DATOSFLUO/Replicas CONSTANZA/Replica3/001/455.xlsx", 
                          sheet = 1, col_names = FALSE)[-c(1:9),]

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



#Gráficos de cajas (boxplots)----
#Obtención cociente DxAm y DxDm

#Obtener los datos de fluorescencia del donador a 475. 
#Filtrar de "fret" las fluorescencias para cada constructo
#en un dataframe

#Extraer la cantidad de constructos que tienen los datos
i <- length(fret[,-c(1,2)])/12 #Cantidad de constructos

#Generar dataframe donde guardar datos
temp_df <- data.frame(matrix(ncol = (length(fret[,-c(1,2)])/i), 
                             nrow = 0))

#Obtener los renglones con las fluorescencias del donador y
#aceptor
temp_rep6 <- fret[fret$`Wavelength [nm]` == "475" | 
                    fret$`Wavelength [nm]` == "525",]
temp_vec <- c() #vector vacío
for (a in seq(1,length(fret[,-c(1,2)]),12)) {
  
  temp_vec <- temp_rep6[,seq(a+2,a+13)]
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

#Generar dataframe para los datos normalizados
temp_rep6 <- data.frame(matrix(ncol = 7, 
                               nrow = (12*length(fret[,-c(1,2)])/12)))

#Agregar los nombres a las columnas
names(temp_rep6)[1] <- "Tratamiento"
names(temp_rep6)[2] <- "DxAm"
names(temp_rep6)[3] <- "DxDm"
names(temp_rep6)[4] <- "DxAm/DxDm"
names(temp_rep6)[5] <- "Promedio"
names(temp_rep6)[6] <- "Normalización"
names(temp_rep6)[7] <- "Replica"

#Agregar columna de tratamiento
temp_rep6$Tratamiento <- rep(c(0,800,1000,1500), each = 3)

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

#Añadir columnas 
temp_rep6$DxAm <- temp_vec_525
temp_rep6$DxDm <- temp_vec_475

#Agregar el cociente DxAm/DxDm
temp_rep6$`DxAm/DxDm` <- temp_vec_525/temp_vec_475

#Agregar el promedio de DxAM/DxDm del tratamiento a 
#0 NaCl para cada constructo 

#Extraer todos los datos para NaCl 0 de cada constructo
#y sumarlos
h <- temp_rep6[seq(1, nrow(temp_rep6), 12),4]
i <- temp_rep6[(seq(1, nrow(temp_rep6), 12) + 1),4]
j <- temp_rep6[(seq(1, nrow(temp_rep6), 12) + 2),4]

#Sumar todos los datos y dividirlos entre tres. Añadir
#cada valor del vector cada 12 veces para que corresponda
#con el valor de cada constructo a NaCl 0M
temp_rep6$Promedio <- rep((h + i + j)/(3), each = 12)

#Dividir cada valor del promedio con el valor de la columna
#DxAm/DxDm para obtener la normalización
temp_rep6$Normalización <- (temp_rep6$`DxAm/DxDm`)/(temp_rep6$Promedio)

#Añadir la replica con la que se está trabajando (revisar)
temp_rep6$Replica <- 3

#Juntar dataframes
fret_all <- rbind(temp_rep1, temp_rep2, temp_rep3, 
                  temp_rep4, temp_rep5, temp_rep6)

















