#Paqueterías----

library(ggplot2)
library(ggpubr)
library(grid)

#Construcción de datos de correlación----

#Cargar archivo FRET

#cargar el archivo solo con la hoja 1 (datos) --revisar--
fret <- readxl::read_xlsx(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/FRET/IDRFT-baja. Rep1.xlsx", 
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


#Juntar cada uno de los dataframes de los constructos
#fret_constructs <- data.frame() #EJECUTAR SOLO UNA VEZ 
fret_constructs <- rbind(temp_rep1, fret_constructs)


#DETENERSE AQUÍ HASTA QUE ESTÉN TODOS LOS DATOS
temp_constructs <- fret_constructs

fret_constructs <- temp_constructs
#Añadir el nombre de los constructos de manualmente
fret_constructs$Constructo <- rep(c("SED1", "ara.F", "016", "045", "139", "142", "147", "148", "149", 
                                    "000","053", "054", "064", "077", "136", "140", "141",
                                    "146", "165", "044", "081", "082","137", "138", "143", "144", "145"), each = 12)
#Añadir el identificador del factor de transcripción
#Eliminar aquellos factores que no se encuentran dentro
#del filtrado de los 21 factores
#Los datos a eliminar son: 044, 053, 016, SED1 y ara.F
fret_constructs <- fret_constructs[-c(1:36, 109:132, 229:240),]

fret_constructs$factor_ID <- rep(c("64", "11", "42", "47", "39",
                                   "46", "1", "14", "36", "26",
                                   "33", "6", "28", "51", "25",
                                   "4", "55", "7", "27", "15",
                                   "3"), each = 12)

#Emplear el dataframe sublist_1B y temp_coverage para
#obtener la longitud y el porcentaje desordenado
temp_corr <- (sublist_1B[(sublist_1B$IDR %in% temp_coverage$IDR),])
temp_corr <- temp_corr[order(temp_corr$IDR),]
temp_corr_1 <- temp_coverage[order(temp_coverage$IDR),]
temp_corr_1$longitud_IDR <- temp_corr$longitud_IDR
temp_corr_1$Porcentaje_desordenado <- temp_corr$Porcentaje_desordenado

#Una vez obtenido un dataframe con los valores a utilizar, ordenar 
#temp_corr_1 por el constructo o Entry, al igual que fret_constructs

temp_corr_1 <- temp_corr_1[order(temp_corr_1$Entry),]
fret_constructs <- fret_constructs[order(fret_constructs$Constructo),]

#Añadir al fret_constructs la columna de longitud y porcentaje de
#desorden

fret_constructs$Longitud_IDR <- rep(temp_corr_1$longitud_IDR, each = 12)
fret_constructs$Desorden_prom <- rep(temp_corr_1$Porcentaje_desordenado, each = 12)

#Añadir al dataframe largo los datos de longitud de la IDR, 
#numeor de resiudos duales, desorden promedio (PONDR) y el
#pLDDT promedio

temp_plddt <- pLDDT_bios_P[order(pLDDT_bios_P$Entry),]

#Añadir las columnas del pLDDT
fret_constructs$Very_high_pLDDT <- rep(temp_plddt$Very_high, each = 12)
fret_constructs$Confident_pLDDT <- rep(temp_plddt$Confident, each = 12)
fret_constructs$Low_pLDDT <- rep(temp_plddt$Low, each = 12)
fret_constructs$Very_Low_pLDDT <- rep(temp_plddt$Very_Low, each = 12)

#Añadir la cantidad de residuos duales de cada IDR
fret_constructs$Residuos_dual <- rep(((temp_corr_1$coverage)/(100))*(temp_corr_1$longitud_IDR), 
                                     each = 12)

#Añadir el porcentaje de dualidad de toda la IDR 
fret_constructs$Porcentaje_dual <- rep(temp_corr_1$coverage, 
                                       each = 12)


#Guardar archivo 

write.table(x = fret_constructs, file = "DATA/DATA FOR R/FRET/Data_FRET/FRET_correlación.txt", 
            quote = FALSE, sep = ",", row.names = FALSE)


#Modificar la base de dato "fret_construct" para obtener
#cuatro gráficos distintos por cada tratamiento. Emplear
#la media de los datos de FRET normalizado para los
#gráficos

construc_names <- c("081", "082", "137", "138", "143", "144",
                    "145", "146", "165", "054", "064", "077",
                    "136", "140", "141", "142", "147", "148", 
                    "149", "045", "139")

construc_names <- construc_names[order(as.numeric(construc_names))]
construc_names

temp_mean <- c() #vector vacío
#Para obtener las condiciones a 0M
for (a in construc_names) {
  
  temp <- fret_constructs[fret_constructs$Constructo == a & 
                    fret_constructs$Tratamiento == 0,]
  
  #Agregar la media a un vector vacío
  temp_mean[length(temp_mean) + 1] <- mean(temp$`DxAm/DxDm`)
}

temp_mean
#Agregar columna de medias y nuevo dataframe
fret_constructs_0M <- fret_constructs[c(1,13,25,37,49,61,73,85,98,109,
                                        121,133,145,157,169,181,193,205,
                                        217,229,241),]
fret_constructs_0M$media_FRET <- temp_mean



temp_mean <- c() #vector vacío
#Para obtener las condiciones a 0M
for (a in construc_names) {
  
  temp <- fret_constructs[fret_constructs$Constructo == a & 
                    fret_constructs$Tratamiento == 0.5,]
  
  #Agregar la media a un vector vacío
  temp_mean[length(temp_mean) + 1] <- mean(temp$`DxAm/DxDm`)
}

temp_mean
#Agregar columna de medias y nuevo dataframe
fret_constructs_0.5M <- fret_constructs[c(1,13,25,37,49,61,73,85,98,109,
                                        121,133,145,157,169,181,193,205,
                                        217,229,241) + 3,]
fret_constructs_0.5M$media_FRET <- temp_mean




temp_mean <- c() #vector vacío
#Para obtener las condiciones a 1M
for (a in construc_names) {
  
  temp <- fret_constructs[fret_constructs$Constructo == a & 
                    fret_constructs$Tratamiento == 1,]
  
  #Agregar la media a un vector vacío
  temp_mean[length(temp_mean) + 1] <- mean(temp$`DxAm/DxDm`)
}


#Agregar columna de medias y nuevo dataframe
fret_constructs_1M <- fret_constructs[c(1,13,25,37,49,61,73,85,98,109,
                                        121,133,145,157,169,181,193,205,
                                        217,229,241) + 6,]
fret_constructs_1M$media_FRET <- temp_mean



temp_mean <- c() #vector vacío
#Para obtener las condiciones a 1.5M
for (a in construc_names) {
  
  temp <- fret_constructs[fret_constructs$Constructo == a & 
                    fret_constructs$Tratamiento == 1.5,]
  
  #Agregar la media a un vector vacío
  temp_mean[length(temp_mean) + 1] <- mean(temp$`DxAm/DxDm`)
}


#Agregar columna de medias y nuevo dataframe
fret_constructs_1.5M <- fret_constructs[c(1,13,25,37,49,61,73,85,98,109,
                                        121,133,145,157,169,181,193,205,
                                        217,229,241) + 9,]
fret_constructs_1.5M$media_FRET <- temp_mean





#Gráficos de correlación----
#Graficar cociente FRET (DxAm/DxDm) vs longitud IDR----

#Gráfico de media DxAm/DxDm vs longitudde IDR para 0M
temp <- round(cor(fret_constructs_0M$media_FRET,
              fret_constructs_0M$Longitud_IDR, 
              method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Longitud_IDR), 
              size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Longitud_IDR), 
              method = "lm", fill = "gray", 
              color = "black", alpha = 0.4) +
  theme_classic2() +
  labs(x = "Longitud de la IDR (aa)", y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(45,150), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Longitud_IDR) +
  annotate(geom = "text", x = 125, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_len_0M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs longitudde IDR para 0.5M
temp <- round(cor(fret_constructs_0.5M$media_FRET,
                  fret_constructs_0.5M$Longitud_IDR, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0.5M, 
             aes(y = media_FRET, x = Longitud_IDR), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0.5M, 
              aes(y = media_FRET, x = Longitud_IDR), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Longitud de la IDR (aa)", y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(45,150), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Longitud_IDR) +
  annotate(geom = "text", x = 125, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_len_0.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs longitudde IDR para 1M

temp <- round(cor(fret_constructs_1M$media_FRET,
                  fret_constructs_1M$Longitud_IDR, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1M, 
             aes(y = media_FRET, x = Longitud_IDR), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1M, 
              aes(y = media_FRET, x = Longitud_IDR), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Longitud de la IDR", y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(45,150), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Longitud_IDR) +
  annotate(geom = "text", x = 125, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)


#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_len_1M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs longitudde IDR para 1.5M
temp <- round(cor(fret_constructs_1.5M$media_FRET,
                  fret_constructs_1.5M$Longitud_IDR, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1.5M, 
             aes(y = media_FRET, x = Longitud_IDR), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1.5M, 
              aes(y = media_FRET, x = Longitud_IDR), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Longitud de la IDR", y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(45,150), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Longitud_IDR) +
  annotate(geom = "text", x = 125, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)


#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_len_1.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Graficar media delcociente FRET (DxAm/DxDm) vs residuos
#duales----

#Gráfico de media DxAm/DxDm vs residuos duales para 0M
temp <- round(cor(fret_constructs_0M$media_FRET,
                  fret_constructs_0M$Residuos_dual, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(y = media_FRET, x = Residuos_dual), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Residuos_dual), 
              method = "lm", fill = "gray", 
              color = "black", alpha = 0.4) +
  theme_classic2() +
  labs(x = "Número de residuos duales", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(15,90), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Residuos_dual) +
  annotate(geom = "text", x = 75, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_dual_0M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media DxAm/DxDm vs longitudde IDR para 0.5M
temp <- round(cor(fret_constructs_0.5M$media_FRET,
                  fret_constructs_0.5M$Residuos_dual, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0.5M, 
             aes(y = media_FRET, x = Residuos_dual), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0.5M, 
              aes(y = media_FRET, x = Residuos_dual), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Número de residuos duales", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(15,90), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Residuos_dual) +
  annotate(geom = "text", x = 75, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_dual_0.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs rsiudos duales para 1M

temp <- round(cor(fret_constructs_1M$media_FRET,
                  fret_constructs_1M$Residuos_dual, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1M, 
             aes(y = media_FRET, x = Residuos_dual), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1M, 
              aes(y = media_FRET, x = Residuos_dual), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Número de residuos duales", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(15,90), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Residuos_dual) +
  annotate(geom = "text", x = 75, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_dual_1M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs resiudos duales para 1.5M
temp <- round(cor(fret_constructs_1.5M$media_FRET,
                  fret_constructs_1.5M$Residuos_dual, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1.5M, 
             aes(y = media_FRET, x = Residuos_dual), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1.5M, 
              aes(y = media_FRET, x = Residuos_dual), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Número de residuos duales", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(15,90), ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Residuos_dual) +
  annotate(geom = "text", x = 75, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_dual_1.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Graficar media delcociente FRET (DxAm/DxDm) vs desorden----


#Gráfico de media DxAm/DxDm vs desorden para 0M
temp <- round(cor(fret_constructs_0MM$media_FRET,
                  fret_constructs_0M$Desorden_prom, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(y = media_FRET, x = Desorden_prom), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Desorden_prom), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Porcentaje de desorden", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0M$Desorden_prom),
                           max(fret_constructs_0M$Desorden_prom)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Desorden_prom) +
  annotate(geom = "text", x = 65, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_disor_0M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs desorden para 0.5M
temp <- round(cor(fret_constructs_0.5M$media_FRET,
                  fret_constructs_0.5M$Desorden_prom, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0.5M, 
             aes(y = media_FRET, x = Desorden_prom), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0.5M, 
              aes(y = media_FRET, x = Desorden_prom), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Porcentaje de desorden", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0.5M$Desorden_prom),
                           max(fret_constructs_0.5M$Desorden_prom)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Desorden_prom) +
  annotate(geom = "text", x = 65, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_disor_0.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs desorden para 1M
temp <- round(cor(fret_constructs_1M$media_FRET,
                  fret_constructs_1M$Desorden_prom, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1M, 
             aes(y = media_FRET, x = Desorden_prom), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1M, 
              aes(y = media_FRET, x = Desorden_prom), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Porcentaje de desorden", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1M$Desorden_prom),
                           max(fret_constructs_1M$Desorden_prom)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Desorden_prom) +
  annotate(geom = "text", x = 65, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_disor_1M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs desorden para 1.5M
temp <- round(cor(fret_constructs_1.5M$media_FRET,
                  fret_constructs_1.5M$Desorden_prom, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1.5M, 
             aes(y = media_FRET, x = Desorden_prom), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1.5M, 
              aes(y = media_FRET, x = Desorden_prom), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "Porcentaje de desorden", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1.5M$Desorden_prom),
                           max(fret_constructs_1.5M$Desorden_prom)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Desorden_prom) +
  annotate(geom = "text", x = 65, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_disor_1.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Graficar media delcociente FRET (DxAm/DxDm) vs pLDDT > 90----


#Gráfico de media DxAm/DxDm vs pLDDT > 90 para 0M
temp <- round(cor(fret_constructs_0M$media_FRET,
                  fret_constructs_0M$Very_high_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(y = media_FRET, x = Very_high_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Very_high_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT muy confiable (> 90)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0M$Very_high_pLDDT),
                           max(fret_constructs_0M$Very_high_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Very_high_pLDDT) +
  annotate(geom = "text", x = 69, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt_0M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media DxAm/DxDm vs pLDDT > 90 para 0.5M
temp <- round(cor(fret_constructs_0.5M$media_FRET,
                  fret_constructs_0.5M$Very_high_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0.5M, 
             aes(y = media_FRET, x = Very_high_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0.5M, 
              aes(y = media_FRET, x = Very_high_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT muy confiable (> 90)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0.5M$Very_high_pLDDT),
                           max(fret_constructs_0.5M$Very_high_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Very_high_pLDDT) +
  annotate(geom = "text", x = 69, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt_0.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs pLDDT > 90 para 1M
temp <- round(cor(fret_constructs_1M$media_FRET,
                  fret_constructs_1M$Very_high_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1M, 
             aes(y = media_FRET, x = Very_high_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1M, 
              aes(y = media_FRET, x = Very_high_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT muy confiable (> 90)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1M$Very_high_pLDDT),
                           max(fret_constructs_1M$Very_high_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Very_high_pLDDT) +
  annotate(geom = "text", x = 69, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt_1M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs pLDDT > 90 para 1.5M
temp <- round(cor(fret_constructs_1.5M$media_FRET,
                  fret_constructs_1.5M$Very_high_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1.5M, 
             aes(y = media_FRET, x = Very_high_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1.5M, 
              aes(y = media_FRET, x = Very_high_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT muy confiable (> 90)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1.5M$Very_high_pLDDT),
                           max(fret_constructs_1.5M$Very_high_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Longitud_IDR) +
  annotate(geom = "text", x = 69, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt_1.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Graficar media cociente FRET (DxAm/DxDm) vs 90 > pLDDT > 70----


#Gráfico de media DxAm/DxDm vs 90 > pLDDT > 70 para 0M
temp <- round(cor(fret_constructs_0M$media_FRET,
                  fret_constructs_0M$Confident_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(y = media_FRET, x = Confident_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Confident_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT confiable (90 > pLDDT > 70)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0M$Confident_pLDDT),
                           max(fret_constructs_0M$Confident_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Confident_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt70_0M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media DxAm/DxDm vs 90 > pLDDT > 70 para 0.5M
temp <- round(cor(fret_constructs_0.5M$media_FRET,
                  fret_constructs_0.5M$Confident_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0.5M, 
             aes(y = media_FRET, x = Confident_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0.5M, 
              aes(y = media_FRET, x = Confident_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT confiable (90 > pLDDT > 70)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0.5M$Confident_pLDDT),
                           max(fret_constructs_0.5M$Confident_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Confident_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt70_0.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media DxAm/DxDm vs 90 > pLDDT > 70 para 1M
temp <- round(cor(fret_constructs_1M$media_FRET,
                  fret_constructs_1M$Confident_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1M, 
             aes(y = media_FRET, x = Confident_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1M, 
              aes(y = media_FRET, x = Confident_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT confiable (90 > pLDDT > 70)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1M$Confident_pLDDT),
                           max(fret_constructs_1M$Confident_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Confident_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt70_1M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs 90 > pLDDT > 70 para 1.5M
temp <- round(cor(fret_constructs_1.5M$media_FRET,
                  fret_constructs_1.5M$Confident_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1.5M, 
             aes(y = media_FRET, x = Confident_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1.5M, 
              aes(y = media_FRET, x = Confident_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT confiable (90 > pLDDT > 70)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1.5M$Confident_pLDDT),
                           max(fret_constructs_1.5M$Confident_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Confident_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_90plddt70_1.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Graficar media delcociente FRET (DxAm/DxDm) vs 70 > pLDDT > 50----


#Gráfico de media DxAm/DxDm vs 70 > pLDDT > 50 para 0M
temp <- round(cor(fret_constructs_0M$media_FRET,
                  fret_constructs_0M$Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(y = media_FRET, x = Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0M$Low_pLDDT),
                           max(fret_constructs_0M$Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Low_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_70plddt50_0M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media DxAm/DxDm vs 70 > pLDDT > 50 para 0.5M
temp <- round(cor(fret_constructs_0.5M$media_FRET,
                  fret_constructs_0.5M$Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0.5M, 
             aes(y = media_FRET, x = Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0.5M, 
              aes(y = media_FRET, x = Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0.5M$Low_pLDDT),
                           max(fret_constructs_0.5M$Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Longitud_IDR) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_70plddt50_0.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media DxAm/DxDm vs 70 > pLDDT > 50 para 1M
temp <- round(cor(fret_constructs_1M$media_FRET,
                  fret_constructs_1M$Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1M, 
             aes(y = media_FRET, x = Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1M, 
              aes(y = media_FRET, x = Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1M$Low_pLDDT),
                           max(fret_constructs_1M$Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Low_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_70plddt50_1M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs 70 > pLDDT > 50 para 1.5M
temp <- round(cor(fret_constructs_1.5M$media_FRET,
                  fret_constructs_1.5M$Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1.5M, 
             aes(y = media_FRET, x = Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1.5M, 
              aes(y = media_FRET, x = Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1.5M$Low_pLDDT),
                           max(fret_constructs_1.5M$Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Low_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_70plddt50_1.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Graficar media delcociente FRET (DxAm/DxDm) vs pLDDT < 50----


#Gráfico de media DxAm/DxDm vs pLDDT < 50 para 0M
temp <- round(cor(fret_constructs_0M$media_FRET,
                  fret_constructs_0M$Very_Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(y = media_FRET, x = Very_Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0M, 
              aes(y = media_FRET, x = Very_Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0M$Very_Low_pLDDT),
                           max(fret_constructs_0M$Very_Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Very_Low_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_plddt50_0M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs pLDDT < 50 para 0.5M
temp <- round(cor(fret_constructs_0.5M$media_FRET,
                  fret_constructs_0.5M$Very_Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_0.5M, 
             aes(y = media_FRET, x = Very_Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_0.5M, 
              aes(y = media_FRET, x = Very_Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_0.5M$Very_Low_pLDDT),
                           max(fret_constructs_0.5M$Very_Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Very_Low_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_plddt50_0.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media DxAm/DxDm vs pLDDT < 50 para 1M
temp <- round(cor(fret_constructs_1M$media_FRET,
                  fret_constructs_1M$Very_Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1M, 
             aes(y = media_FRET, x = Very_Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1M, 
              aes(y = media_FRET, x = Very_Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1M$Very_Low_pLDDT),
                           max(fret_constructs_1M$Very_Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Very_Low_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_plddt50_1M.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media DxAm/DxDm vs pLDDT < 50 para 0.5M
temp <- round(cor(fret_constructs_1.5M$media_FRET,
                  fret_constructs_1.5M$Very_Low_pLDDT, 
                  method = "pearson"), 2)

ggplot() +
  geom_point(data = fret_constructs_1.5M, 
             aes(y = media_FRET, x = Very_Low_pLDDT), 
             size = 1.5, color = "#006bbf") +
  geom_smooth(data = fret_constructs_1.5M, 
              aes(y = media_FRET, x = Very_Low_pLDDT), 
              method = "lm", fill = "gray", color = "black") +
  theme_classic2() +
  labs(x = "pLDDT bajo (70 > pLDDT > 50)", 
       y = "Mean\nDxAm/DxDm") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(fret_constructs_1.5M$Very_Low_pLDDT),
                           max(fret_constructs_1.5M$Very_Low_pLDDT)), 
                  ylim = c(0,2.5)) +
  geom_smooth(formula = media_FRET ~ Very_Low_pLDDT) +
  annotate(geom = "text", x = 38, y = 2.25, 
           label = paste0("r = ", temp), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Correlaciones/IDRFTs_corr_plddt50_1.5M.png", 
       device = "png", width = 6, height = 4, dpi = 600)






#Generar una matriz de correlación----

#Matriz de corelación para 0M
temp_matrix <- fret_constructs_0M[fret_constructs_0M$Tratamiento == 0,][,c(4,10:17)]
temp_matrix <- round(cor(temp_matrix),2)

#Gráfico
ggcorrplot(temp_matrix, hc.order = FALSE, type = "lower", 
           ggtheme = theme_bw(), outline.color = "black",
           lab = TRUE, legend.title = "Correlación", 
           title = "Tratamiento a 0M")

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_matrix_0M.png", 
       device = "png", width = 8, height = 5, units = "in", dpi = 600)


#Matriz de corelación para 0.5M
temp_matrix <- fret_constructs[fret_constructs$Tratamiento == 0.5,][,c(4,10:17)]
temp_matrix <- round(cor(temp_matrix),2)

#Gráfico
ggcorrplot(temp_matrix, hc.order = TRUE, type = "lower", 
           ggtheme = theme_bw(), outline.color = "black",
           lab = TRUE, legend.title = "Correlación", 
           title = "Tratamiento a 0.5M")

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_matrix_0.5M.png", 
       device = "png", width = 8, height = 5, units = "in", dpi = 600)


#Matriz de corelación para 1M
temp_matrix <- fret_constructs[fret_constructs$Tratamiento == 1,][,c(4,10:17)]
temp_matrix <- round(cor(temp_matrix),2)

#Gráfico
ggcorrplot(temp_matrix, hc.order = TRUE, type = "lower", 
           ggtheme = theme_bw(), outline.color = "black",
           lab = TRUE, legend.title = "Correlación", 
           title = "Tratamiento a 1M")

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_matrix_1M.png", 
       device = "png", width = 8, height = 5, units = "in", dpi = 600)


#Matriz de corelación para 1.5M
temp_matrix <- fret_constructs[fret_constructs$Tratamiento == 1.5,][,c(4,10:17)]
temp_matrix <- round(cor(temp_matrix),2)

#Gráfico
ggcorrplot(temp_matrix, hc.order = TRUE, type = "lower", 
           ggtheme = theme_bw(), outline.color = "black",
           lab = TRUE, legend.title = "Correlación", 
           title = "Tratamiento a 1.5M")

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_matrix_1.5M.png", 
       device = "png", width = 8, height = 5, units = "in", dpi = 600)




#Gráfico de delta FRET----

i <- fret_constructs[fret_constructs$Tratamiento == 0,]$`DxAm/DxDm`
h <- fret_constructs[fret_constructs$Tratamiento == 1,]$`DxAm/DxDm`

#Generar dos dataframes para cada condición
delta_fret_0M <- fret_constructs[fret_constructs$Tratamiento == 0,]
delta_fret_1M <- fret_constructs[fret_constructs$Tratamiento == 1,]

#Agregar columnas de delta fret a cada dataframe
delta_fret_0M$delta_fret <- (h-i)
delta_fret_1M$delta_fret <- (h-i)


#Gráfico de delta fret vs, fret inicial

#Función para buscar el constructo
#Añadir columna de mena delta fret
fret_constructs_0M$delta_FRET <- (fret_constructs_1M$media_FRET - fret_constructs_0M$media_FRET)


#Guardar archivo
write.csv(x = fret_constructs_0M, 
          file = "DATA/DATA FOR R/FRET/Data_FRET/Delta_FRET.csv",
          quote = FALSE, row.names = FALSE)



delta_corr <- function(construct) {
  
  construct <- construct
  plot <- ggplot() +
    geom_point(data = fret_constructs_0M, 
                aes(x = DxAm.DxDm, y = delta_FRET)) +
    geom_point(data = (fret_constructs_0M[fret_constructs_0M$Constructo == construct,]), 
                aes(x = DxAm.DxDm, y = delta_FRET), 
                color = "red", size = 2) +
    theme_bw() +
    labs(x = "DxAm/DxDm normalizado", 
         y = "Mean Delta FRET") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    coord_cartesian(xlim = c(0.5,2), ylim = c(-0.1,0.8))
  
  return(plot)
  
}


delta_corr(construct = 45)
delta_corr(construct = 54)
delta_corr(construct = 64)
delta_corr(construct = 77)
delta_corr(construct = 81)
delta_corr(construct = 82)
delta_corr(construct = 136)
delta_corr(construct = 137)
delta_corr(construct = 138)
delta_corr(construct = 139)
delta_corr(construct = 140)
delta_corr(construct = 141)
delta_corr(construct = 142)
delta_corr(construct = 144)
delta_corr(construct = 145)
delta_corr(construct = 146)
delta_corr(construct = 147)
delta_corr(construct = 148)
delta_corr(construct = 149)
delta_corr(construct = 165)



#Función para buscar el constructo
delta_corr <- function(construct) {
  
  construct <- construct
  plot <- ggplot() +
    geom_point(data = fret_constructs_0M, 
               aes(x = DxAm.DxDm, y = delta_FRET)) +
    geom_point(data = (delta_fret_0M[delta_fret_0M$Constructo == construct,]), 
               aes(x = DxAm.DxDm, y = delta_fret), 
               color = "red", size = 2) +
    theme_bw() +
    labs(x = "DxAm/DxDm normalizado", 
         y = "Delta FRET") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    coord_cartesian(xlim = c(0.5,2), ylim = c(-0.1,0.8))
  
  return(plot)
  
}

#----
#Gráfico de media delta DxAm/DxDm vs longitud de la IDR
temp <- round(cor(fret_constructs_0M$media_FRET,
                  fret_constructs_0M$delta_FRET, 
                  method = "pearson"), 2)

temp_p <- round(cor.test(fret_constructs_0M$media_FRET,
                   fret_constructs_0M$delta_FRET, 
                   method = "pearson")[3][[1]], 2)

a1 <- ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(x = Longitud_IDR, y = delta_FRET), 
             size = 1.5, color = "#ff0000") +
  geom_smooth(data = fret_constructs_0M,
              aes(y = delta_FRET, x = Longitud_IDR),
              method = "lm", fill = "gray", color = "#0031c8",
              alpha = 0.6) +
  theme_classic2() +
  labs(x = "Longitud de la IDR (aa)", 
       y = expression(Delta ~ "FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-0.5,1.5)) + 
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Longitud_IDR) + 5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.7, 
           label = paste0("r = ", temp), size = 5) +
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Longitud_IDR) + 5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.55, 
         label = paste0("p = ", temp_p), size = 5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_delta_len.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Gráfico de media delta DxAm/DxDm vs desorde
temp <- round(cor(fret_constructs_0M$delta_FRET,
                  fret_constructs_0M$Desorden_prom, 
                  method = "pearson"), 2)

temp_p <- round(cor.test(fret_constructs_0M$delta_FRET,
                         fret_constructs_0M$Desorden_prom, 
                         method = "pearson")[3][[1]], 2)

a2 <- ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(x = Desorden_prom, y = delta_FRET), 
             size = 1.5, color = "#ff0000") +
  geom_smooth(data = fret_constructs_0M,
              aes(y = delta_FRET, x = Desorden_prom),
              method = "lm", fill = "gray", color = "#0031c8",
              alpha = 0.6) +
  theme_classic2() +
  labs(x = "Porcentaje desordenado", 
       y = expression(Delta ~ "FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-0.5,1.5)) + 
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Desorden_prom) + 2, 
           y = max(fret_constructs_0M$delta_FRET) + 0.7, 
           label = paste0("r = ", temp), size = 5) +
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Desorden_prom) + 2, 
           y = max(fret_constructs_0M$delta_FRET) + 0.55, 
           label = paste0("p = ", temp_p), size = 5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_delta_disor.png", 
       device = "png", width = 6, height = 4, dpi = 600)




#Gráfico de media delta DxAm/DxDm vs dualidad
temp <- round(cor(fret_constructs_0M$delta_FRET,
                  fret_constructs_0M$Residuos_dual, 
                  method = "pearson"), 2)

temp_p <- round(cor.test(fret_constructs_0M$delta_FRET,
                         fret_constructs_0M$Residuos_dual, 
                         method = "pearson")[3][[1]], 2)

a3 <- ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(x = Residuos_dual, y = delta_FRET), 
             size = 1.5, color = "#ff0000") +
  geom_smooth(data = fret_constructs_0M,
              aes(y = delta_FRET, x = Residuos_dual),
              method = "lm", fill = "gray", color = "#0031c8",
              alpha = 0.6) +
  theme_classic2() +
  labs(x = "Cantidad de residuos duales", 
       y = expression(Delta ~ "FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-0.5,1.5)) + 
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Residuos_dual) + 4, 
           y = max(fret_constructs_0M$delta_FRET) + 0.7, 
           label = paste0("r = ", temp), size = 5) +
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Residuos_dual) + 4, 
           y = max(fret_constructs_0M$delta_FRET) + 0.55, 
           label = paste0("p = ", temp_p), size = 5)


#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_delta_dual.png", 
       device = "png", width = 6, height = 4, dpi = 600)




#Gráfico de media delta DxAm/DxDm vs Veri high plDDT
temp <- round(cor(fret_constructs_0M$delta_FRET,
                  fret_constructs_0M$Very_high_pLDDT, 
                  method = "pearson"), 2)

temp_p <- round(cor.test(fret_constructs_0M$delta_FRET,
                         fret_constructs_0M$Very_high_pLDDT, 
                         method = "pearson")[3][[1]], 2)

a4 <- ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(x = Very_high_pLDDT, y = delta_FRET), 
             size = 1.5, color = "#ff0000") +
  geom_smooth(data = fret_constructs_0M,
              aes(y = delta_FRET, x = Very_high_pLDDT),
              method = "lm", fill = "gray", color = "#0031c8",
              alpha = 0.6) +
  theme_classic2() +
  labs(x = "Very high pLDDT", 
       y = expression(Delta ~ "FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-0.5,1.5)) + 
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Very_high_pLDDT) + 4.5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.7, 
           label = paste0("r = ", temp), size = 5) +
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Very_high_pLDDT) + 4.5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.55, 
           label = paste0("p = ", temp_p), size = 5)


#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_delta_90plddt.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media delta DxAm/DxDm vs Confident pLDDT
temp <- round(cor(fret_constructs_0M$delta_FRET,
                  fret_constructs_0M$Confident_pLDDT, 
                  method = "pearson"), 2)

temp_p <- round(cor.test(fret_constructs_0M$delta_FRET,
                         fret_constructs_0M$Confident_pLDDT, 
                         method = "pearson")[3][[1]], 2)

a5 <- ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(x = Confident_pLDDT, y = delta_FRET), 
             size = 1.5, color = "#ff0000") +
  geom_smooth(data = fret_constructs_0M,
              aes(y = delta_FRET, x = Confident_pLDDT),
              method = "lm", fill = "gray", color = "#0031c8",
              alpha = 0.6) +
  theme_classic2() +
  labs(x = "Confident pLDDT", 
       y = expression(Delta ~ "FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-0.5,1.5)) + 
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Confident_pLDDT) + 1, 
           y = max(fret_constructs_0M$delta_FRET) + 0.7, 
           label = paste0("r = ", temp), size = 5) +
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Confident_pLDDT) + 1.5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.55, 
           label = paste0("p = ", temp_p), size = 5)

#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_delta_90plddt70.png", 
       device = "png", width = 6, height = 4, dpi = 600)



#Gráfico de media delta DxAm/DxDm vs Low pLDDT
temp <- round(cor(fret_constructs_0M$delta_FRET,
                  fret_constructs_0M$Low_pLDDT, 
                  method = "pearson"), 2)

temp_p <- round(cor.test(fret_constructs_0M$delta_FRET,
                         fret_constructs_0M$Low_pLDDT, 
                         method = "pearson")[3][[1]], 2)

a6 <- ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(x = Low_pLDDT, y = delta_FRET), 
             size = 1.5, color = "#ff0000") +
  geom_smooth(data = fret_constructs_0M,
              aes(y = delta_FRET, x = Low_pLDDT),
              method = "lm", fill = "gray", color = "#0031c8",
              alpha = 0.6) +
  theme_classic2() +
  labs(x = "Low pLDDT", 
       y = expression(Delta ~ "FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-0.5,1.5)) + 
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Low_pLDDT) + 2.5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.7, 
           label = paste0("r = ", temp), size = 5) +
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Low_pLDDT) + 2.5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.55, 
           label = paste0("p = ", temp_p), size = 5)


#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_delta_70plddt50.png", 
       device = "png", width = 6, height = 4, dpi = 600)




#Gráfico de media delta DxAm/DxDm vsmuy bajo pLDDT
temp <- round(cor(fret_constructs_0M$delta_FRET,
                  fret_constructs_0M$Very_Low_pLDDT, 
                  method = "pearson"), 2)

temp_p <- round(cor.test(fret_constructs_0M$delta_FRET,
                         fret_constructs_0M$Very_Low_pLDDT, 
                         method = "pearson")[3][[1]], 2)

a7 <- ggplot() +
  geom_point(data = fret_constructs_0M, 
             aes(x = Very_Low_pLDDT, y = delta_FRET), 
             size = 1.5, color = "#ff0000") +
  geom_smooth(data = fret_constructs_0M,
              aes(y = delta_FRET, x = Very_Low_pLDDT),
              method = "lm", fill = "gray", color = "#0031c8",
              alpha = 0.6) +
  theme_classic2() +
  labs(x = "Very low pLDDT", 
       y = expression(Delta ~ "FRET")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(-0.5,1.5)) + 
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Very_Low_pLDDT) + 1.5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.7, 
           label = paste0("r = ", temp), size = 5) +
  annotate(geom = "text", 
           x = min(fret_constructs_0M$Very_Low_pLDDT) + 0.5, 
           y = max(fret_constructs_0M$delta_FRET) + 0.55, 
           label = paste0("p = ", temp_p), size = 5)


#Guardar gráfico
ggsave(filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_delta_plddt50.png", 
       device = "png", width = 6, height = 4, dpi = 600)


#Juntar gráficos para mejorar su visualización----

fig_corr <- ggarrange(a1 + rremove("ylab"),
                      a2 + rremove("ylab"),
                      a3 + rremove("ylab"),
                      a4 + rremove("ylab"),
                      a5 + rremove("ylab"),
                      a6 + rremove("ylab"),
                      a7 + rremove("ylab"),
                      nrow = 3, ncol = 3)


fig_corr <- annotate_figure(fig_corr, 
                            left = textGrob(label = expression(Delta ~ "FRET"), 
                                            vjust = 0.5, rot = 90, 
                                            gp = gpar(cex = 1.5)))

#Guardar gráfico
ggsave(plot = fig_corr, filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_all_correlations.png", 
       device = "png", width = 16, height = 10, dpi = 600)

#Guardar gráfico
ggsave(plot = fig_corr, filename = "PLOTS/IDR ESTADÍSTICOS/Corr_delta_FRET/IDRFTs_all_correlations.pdf", 
       device = "pdf", width = 16, height = 10, dpi = 600)
  