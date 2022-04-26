#Paqueterías----

library(ggplot2)
library(ggpubr)

#Análisis de dualidad----

#Realizado el análisis en AGADIR por residuos, cargar los archivos de
#AGADIR uno por uno

#Crear una variable con la longitud de renglones del archivo

len_row <- nrow(read.csv(file = "DATA/DATA FOR R/DUALIDAD/IDRFT_001A.txt", 
                         header = TRUE, skip = 12)[-1,1:3])

#Cargar el archivo omitiendo y eliminando renglones para obtener
#los datos de manera correcta y ordenados

temp_dual <- read.csv(file = "DATA/DATA FOR R/DUALIDAD/IDRFT_001A.txt", 
                      header = TRUE, skip = 12)[-c(1,len_row + 1),1:3]

#Cargar archivo de porcentaje de desorden por residuo

temp_disor <- read.table(file = "DATA/DATA FOR R/DUALIDAD/DUALIDAD_P/IDRFT_001P.txt",
                         header = T)

names(temp_disor)[1] <- "Num"


#Obtención de dualidad----

#Obtener la fracción de porcentaje de alfa hélice para la variable
#temp_dual

temp_dual$Hel_fracc <- (temp_dual$Hel)/(max(temp_dual$Hel, na.rm = TRUE))

#Agregar a temp_dual la columna de VLXT de temp_disor

temp_dual$VLXT <- temp_disor$VLXT

#Obtener la fracción hélice/desorden en otra columna

temp_dual$fr <- temp_dual$Hel_fracc/temp_dual$VLXT

#Agregar condicional para filtrar los datos de alfa hélice
#mayores a 10 porciento

temp_dual$condition_10 <- 1

for (a in 1:nrow(temp_dual)) {
  for (b in ((temp_dual[a,3])*1)) {
    if (b < 10) {
      
      temp_dual[a,7] <- 0
      
    } else {
      
      temp_dual[a,7] <- b
      
    }
  }
}


#Agregar una condición para filtrar un desorden mayor a 50%

temp_dual$condition_50 <- 1

for (a in 1:nrow(temp_dual)) {
  for (b in ((temp_dual[a,5])*100)) {
    if (b < 50) {
      
      temp_dual[a,8] <- 0
      
    } else {
      
      temp_dual[a,8] <- b
      
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


#Guardar gŕaficos de alfa hélice en carpeta única

ggsave(filename = "PLOTS/ALFA HÉLICE/IDRFT_001ALFA.png", 
       device = "png", width = 8, height = 5, dpi = 650, plot = p1)


#Determinación de residuos adyacentes----

#Para determinar la dualidad, deben existir al menos 6 residuos 
#adyacentes en las IDRs. Con cada archivo descargado y nombrado com
#IDRFT_00xD.txt, donde "x" representa el número de secuencia, crear 
#un programa que lo calcule

temp_dual <- read.table(file = "DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/IDFRT_001D.txt",
                        header = TRUE, sep = ",", dec = ".")


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

#Con la variable "coverage", realizar un gráfico 
#rápido para ver el comportamiento de los 61 factores

ggplot() +
  geom_density(data = coverage, aes(x = coverage, y = ..scaled..),
               color = "#E69F00", size = 0.8, lty = 1) +
  theme_bw() +
  labs(x = "Coverage (%)", y = "Densidad normalizada") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0.28,1), xlim = c(28.5,86.3)) +
  scale_x_continuous(breaks = seq(28.5,86.3,12))

#De acuerdo con el dataframe "coverage", se analizarán las 
#IDRs con el mayor y menor valor de coverage en AlphaFold. Para
#ello, emplar la sublista_1B para agregar las secuencias al
#dataframe "coverage"

#Eliminar de la sublista_1B las IDRs que no se analizaron

coverage[,ncol(coverage) + 1] <- sublist_1B[-c(10,13,19,38,43,53,54,62),][3]

#Emplear el dataframe "coverage" y "IDRs_bios" para obtener
#los biosensores de la base de datos

temp_bios <- c() #vector vacío
temp_coverage <- c() #vector vacío
for (a in 1:nrow(IDRs_bios)) {
  for (b in IDRs_bios[a,25]) {
    for (c in grep(pattern = b, x = coverage$IDR, fixed = FALSE)) {
      
      temp_bios[length(temp_bios) + 1] <- c
      temp_coverage <- coverage[temp_bios,]
      
    }
  }
}

#Buscar en el dataframe "coverage" las dos secuencias
#que no se añadieron y colocarlas en el dataframe
#temporal "temp_coverage". Ordenarlas con base en la
#secuencia. (seq 22 y 35 de coverage)

temp_coverage[nrow(temp_coverage) + 1,] <- coverage[22,]
temp_coverage[nrow(temp_coverage) + 1,] <- coverage[35,]

#Añadir la columan "Entry", "Uniprot" y "Protein name" del 
#dataframe "IDRs_bios" al dataframe temporal "temp_coverage"

temp_coverage[,ncol(temp_coverage) + 1] <- IDRs_bios$Entry
temp_coverage[,ncol(temp_coverage) + 1] <- IDRs_bios$UniProt
temp_coverage[,ncol(temp_coverage) + 1] <- IDRs_bios$Protein_name

#Ordenar las columnas
temp_coverage <- temp_coverage[,c(4,1,5,6,2,3)]

#Nombrar las columnas faltantes
names(temp_coverage)[1] <- "Entry"
names(temp_coverage)[3] <- "UniProt"
names(temp_coverage)[4] <- "Protein_name"








#Paqueterías----

library(ggplot2)
library(ggpubr)

#Creación de carpetas requeridas----

#Cambiar únicamente el directorio principal. En este caso, debe
#ser el directorio donde se encuentren todos los archivos que se
#desean analizar

#Creación de carpetas para guardar los archivos generados
#Se debe especificar el directorio donde se encuentren los
#archivos por analizar

#Guardar el directorio de trabajo
main_dir <- "DATA/DATA FOR R/DATA FOR ANALYSIS/" #colocar directorio propio

#Buscar las carpetas requeridas en el directorio anterior

sub_dir <- "DUALIDAD_ALL" #primer carpeta requerida
dir.create(file.path(main_dir, sub_dir)) 

#Crear dos subdirectorios dentro de la carpeta de "PLOTS"
#llamada "DUAL_PLOT" y "DISOR_PLOT" para guardar los 
#gráficos generados posteriormente
sub_dir <- "DUAL_PLOT"
main_dir <- "PLOTS/"
dir.create(file.path(main_dir, sub_dir))

sub_dir <- "DISOR_PLOT"
main_dir <- "PLOTS/"
dir.create(file.path(main_dir, sub_dir))


#Análisis de dualidad----

#Generar una variable con todos los nombres de los archivos 
#por analizar. Colocar en "path" el directorio donde se localicen
#todos los archivos

list_dual <- dir(path = "DATA/DATA FOR R/DUALIDAD/",
                 pattern = "*.txt")[-c(10,13,19,38,43,53,54,62)]

list_disor <- dir(path = "DATA/DATA FOR R/DUALIDAD/DUALIDAD_P/",
                 pattern = "*.txt")


#Analizar cada uno de los archivos de la variable "list_xlxs" y 
#Colocar el directorio donde se encuentran todos los archivos

for (a in list_dual) {
  
  #Crear una variable con la longitud de renglones del archivo
  
  len_row <- nrow(read.csv(file = paste0("DATA/DATA FOR R/DUALIDAD/", a), 
                           header = TRUE, skip = 12)[-1,1:3])
  
  #Cargar archivos de AGADIR
  temp_dual <- read.csv(file = paste0("DATA/DATA FOR R/DUALIDAD/", a), 
                        header = TRUE, skip = 12)[-c(1,len_row + 1),1:3]
  
  #Cargar archivos de PONDR
  temp_disor <- read.table(file = paste0("DATA/DATA FOR R/DUALIDAD/DUALIDAD_P/", 
                                         paste0(substr(a, 1,9)), "P.txt"), header = T)
  
  #Obtener la fracción de porcentaje de alfa hélice para la variable
  #temp_dual
  temp_dual$Hel_fracc <- (temp_dual$Hel)/(max(temp_dual$Hel, na.rm = TRUE))
  
  #Agregar a temp_dual la columna de VLXT de temp_disor
  temp_dual$VLXT <- temp_disor$VLXT
  
  #Obtener la fracción hélice/desorden en otra columna
  temp_dual$fr <- temp_dual$Hel_fracc/temp_dual$VLXT
  
  
  #Agregar condicional al porcentaje de desroden
  
  temp_dual$condition_10 <- 1 #Crear nueva columna
  
  for (c in 1:nrow(temp_dual)) {
    for (d in temp_dual[c,3]) {
      if (d < 10) {
        
        temp_dual[c,7] <- 0
        
      } else {
        
        temp_dual[c,7] <- d
        
      }
    }
  }
  
  #Agregar condicional del porcentaje de alfa hélice
  
  temp_dual$condition_50 <- 1
  
  for (e in 1:nrow(temp_dual)) {
    for (f in ((temp_dual[e,5])*100)) {
      if (f < 50) {
        
        temp_dual[e,8] <- 0
        
      } else {
        
        temp_dual[e,8] <- f
        
      }
    }
  }
  

  #Calcular el parámetro de dualidad dividiendo el valor de
  #alfa hélice de la condicional 10 entre la condicional 50 del desorden
  
  temp_dual$dualidad <- (as.numeric(temp_dual$condition_10))/(temp_dual$condition_50)
  
  #Convertir todos los renglones sin valores a cero
  
  temp_dual[temp_dual == "Inf"] <- 0
  temp_dual[temp_dual == "NaN"] <- 0
  temp_dual[temp_dual == "NA"] <- 0
  
  #Agregar un valor lógico para aquellos datos de dualidad diferentes
  #de cero
  
  temp_dual$lógico <- (temp_dual$dualidad > 0)
  
  #Código para guardar archivos
  
  temp_name <- paste0(substr(a, 1, 9), "D.txt")
  
  #Guardar el archivo en una carpeta nueva que contenga a los nuveos
  #archivos con los datos de dualidad en formato "txt" separado por
  #comas
  
  write.table(x = temp_dual, 
              file = paste0("DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/", temp_name),
              quote = FALSE, 
              sep = ",", 
              row.names = FALSE)
  
  #Graficar residuo vs fracción de alfa hélice
  
  p <- ggplot() +
    #Desorden
    geom_line(data = temp_dual, aes(x = as.integer(res), y = VLXT, 
                                    colour = "Desorden"), size = 1) +
    #Alfa hélice
    geom_line(data = temp_dual, aes(x = as.integer(res), y = Hel_fracc,
                                    colour = "α-hélice"), size = 1) +
    #Umbral de desorden
    geom_hline(yintercept = 0.5, size = 1, alpha = 0.6, lty = 2) +
    theme_classic() +
    labs(x = "Número de residuo",
         y = "Fracción α-hélice") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.key.size = unit(0.6, "cm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    coord_cartesian(xlim = c(0, max(as.numeric(temp_dual$res))), ylim = c(0,1)) +
    scale_x_continuous(breaks = seq(0, max(as.numeric(temp_dual$res)), by = 25), 
                       expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,1, by = 0.1), expand = c(0,0)) +
    scale_color_manual(name = "Fracción", values = c("blue", "red"))
  
  #Guardar gráfico en su carpeta correspondiente
  
  temp_name_plotAD <- paste0(substr(a, 1, 9), "AD.png") #generar nombres
  ggsave(plot = p, filename = (paste0("PLOTS/DISOR_PLOT/", temp_name_plotAD)), 
         device = "png", width = 8, height = 5, dpi = 650)
  
  
  #Gŕafico de dualidad
  
  if (sum(temp_dual$dualidad) != 0) {
    
    p1 <- ggplot() +
      geom_line(data = temp_dual, aes(x = as.integer(res), y = dualidad),
                color = "blue", size = 1) +
      geom_hline(yintercept = 0.1, size = 1, alpha = 0.6, lty = 2) +
      theme_classic() +
      labs(x = "Número de residuo",
           y = "Dualidad") +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) +
      coord_cartesian(xlim = c(0, max(as.numeric(temp_dual$res))), 
                      ylim = c(0,max(temp_dual$dualidad))) +
      scale_x_continuous(breaks = seq(0, max(as.numeric(temp_dual$res)), by = 15), 
                         expand = c(0,0.1)) +
      scale_y_continuous(breaks = seq(0,max(temp_dual$dualidad), by = 0.1), expand = c(0,0))
    
  } else {
    
    next
    
  }
  
  #Guardar gráfico en su carpeta correspondiente
  
  temp_name_plotD <- paste0(substr(a, 1, 9), "D.png") #generar nombres
  ggsave(plot = p1, filename = (paste0("PLOTS/DUAL_PLOT/", temp_name_plotD)), 
         device = "png", width = 8, height = 5, dpi = 650)
}




#Determinación de coverage de dualidad para IDRs----

#Generar una variable con todos los nombres de los
#archivos por analizar

list_txt <- dir(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/",
                pattern = "*.txt")

#Cargar cada archivo con los datos de dualidad calculados
#anteriormente. 

temp_dual <- c() #vector vacío
coverage <- c() #vector vacío
for (a in list_txt) {
  
  temp_dual <- read.table(file = paste0("DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/", a), 
                          header = TRUE, sep = ",")
  
  #Contar la cantidad de residuos con dualidad (mayor a 0.1) 
  #y dividirlos entre la longitud de la proteína (cantidad de
  #residuos) y multiplicar por 100 
  
  i <- table(temp_dual$dualidad >= 0.1)[2] #Obtener residuos duales
  h <- table(temp_dual$dualidad >= 0.1)[2] + table(temp_dual$dualidad >= 0.1)[1] #Total
  
  #Obtener el porcentaje dividiendo a entre b y multiplicando por
  #100
  
  coverage[length(coverage) + 1] <- (i/h)*100 #Dualidad coverage
  
  #Agregar condición para que los valores NA sean convertidos a ceros
  
  coverage[is.na(coverage)] <- 0
  
}

#Convertir en un dataframe los datos con un ID general

coverage <- data.frame("ID_biosensor" = substr(list_txt, 1, 9), 
                       "coverage" = coverage)



