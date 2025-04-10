# Paqueterías----
library(ggplot2)
library(ggpubr)

# Creación de carpetas requeridas----

# Cambiar únicamente el directorio principal. En este caso, debe
# ser el directorio donde se encuentren todos los archivos que se
# desean analizar

# Creación de carpetas para guardar los archivos generados
# Se debe especificar el directorio donde se encuentren los
# archivos por analizar

# Guardar el directorio de trabajo
main_dir <- "C:/Users/Windows/Desktop/TEST/" #colocar directorio propio

# Buscar las carpetas requeridas en el directorio anterior
sub_dir <- "DUALIDAD" #primer carpeta requerida
dir.create(file.path(main_dir, sub_dir)) 

sub_dir <- "PLOTS" #segunda carpeta requerida
dir.create(file.path(main_dir, sub_dir))

# Crear dos subdirectorios dentro de la carpeta de "PLOTS"
# llamada "DUAL_PLOT" y "DISOR_PLOT" para guardar los 
# gráficos generados posteriormente
sub_dir <- "DUAL_PLOT"
main_dir <- "C:/Users/Windows/Desktop/TEST/PLOTS/"
dir.create(file.path(main_dir, sub_dir))

sub_dir <- "DISOR_PLOT"
main_dir <- "C:/Users/Windows/Desktop/TEST/PLOTS/"
dir.create(file.path(main_dir, sub_dir))



# Análisis de dualidad----

# Generar una variable con todos los nombres de los archivos 
# por analizar. Colocar en "path" el directorio donde se localicen
# todos los archivos

list_xlsx <- dir(path = "TEST/",
                 pattern = "*.xlsx")


# Analizar cada uno de los archivos de la variable "list_xlxs" y 
# Colocar el directorio donde se encuentran todos los archivos

for (a in list_xlsx) {
  
  temp_dual <- readxl::read_xlsx(path = paste0("TEST/", a), col_names = TRUE) 
  names(temp_dual) <- fracc
  
  #Agregar candado para evaluar archivos 
  if (length(temp_dual) < 5) {
    next
  }
  #Agregar condicional al porcentaje de desroden
  
  temp_dual$condition_10 <- 1 #Crear nueva columna
  
  for (b in 1:nrow(temp_dual)) {
    for (c in ((temp_dual[b,5])*1)) {
      if (c < 10) {
        
        temp_dual[b, 6] <- 0
        
      } else {
        
        temp_dual[b, 6] <- c
        
      }
    }
  }
  
  # Agregar condicional del porcentaje de alfa hélice
  
  temp_dual$condition_50 <- 1
  
  for (d in 1:nrow(temp_dual)) {
    for (e in ((temp_dual[d,3])*1)) {
      if (e < 50) {
        
        temp_dual[d,7] <- 0
        
      } else {
        
        temp_dual[d,7] <- e
        
      }
    }
  }
  
  # Convertir la variable "temp_dual" en un dataframe
  
  as.data.frame(temp_dual)
  
  # Calcular el parámetro de dualidad dividiendo el valor de
  # alfa hélice de la condicional 10 entre la condicional 50 del desorden
  
  temp_dual$dualidad <- (as.numeric(temp_dual$condition_10))/(temp_dual$condition_50)
  
  # Convertir todos los renglones sin valores a cero
  
  temp_dual[temp_dual == "Inf"] <- 0
  temp_dual[temp_dual == "NaN"] <- 0
  temp_dual[temp_dual == "NA"] <- 0
  
  # Agregar un valor lógico para aquellos datos de dualidad diferentes
  # de cero
  
  temp_dual$lógico <- (temp_dual$dualidad > 0)
  
  
  # Código para guardar archivos
  
  temp_name <- paste0(substr(a, 1, 9), "D.txt")
  
  # Guardar el archivo en una carpeta nueva que contenga a los nuveos
  # archivos con los datos de dualidad en formato "txt" separado por
  # comas
  
  write.table(x = temp_dual, 
              file = paste0("TEST/DUALIDAD/", temp_name),
              quote = FALSE, 
              sep = ",", 
              row.names = FALSE)
  
  # Graficar residuo vs fracción de alfa hélice
  
  p <- ggplot() +
    geom_line(data = temp_dual, aes(x = as.integer(res), y = (`%des`/100), 
                                    colour = "Desorden"), size = 1) +
    geom_line(data = temp_dual, aes(x = as.integer(res), y = (`%hel`/100),
                                    colour = "α-hélice"), size = 1) +
    geom_hline(yintercept = 0.5, size = 1, alpha = 0.6) +
    theme_classic() +
    labs(x = "Número de residuo",
         y = "Fracción α-hélice") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.key.size = unit(0.6, "cm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    coord_cartesian(xlim = c(0, max(temp_dual$res)), ylim = c(0,1)) +
    scale_x_continuous(breaks = seq(0, max(temp_dual$res), by = 25), 
                       expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,1, by = 0.1), expand = c(0,0)) +
    scale_color_manual(name = "Fracción", values = c("blue", "red"))
  
  # Guardar gráfico en su carpeta correspondiente
  
  temp_name_plotAD <- paste0(substr(a, 1, 9), "AD.png") #generar nombres
  ggsave(plot = p, filename = (paste0("TEST/PLOTS/DISOR_PLOT/", temp_name_plotAD)), 
         device = "png", width = 8, height = 5, dpi = 650)
  
  
  
  # Gŕafico de dualidad
  
  if (sum(temp_dual$dualidad) != 0) {
    
    p1 <- ggplot() +
      geom_line(data = temp_dual, aes(x = as.integer(res), y = dualidad),
                color = "blue", size = 1) +
      geom_hline(yintercept = 0.1, size = 1, alpha = 0.6) +
      theme_classic2() +
      labs(x = "Número de residuo",
           y = "Dualidad") +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) +
      coord_cartesian(xlim = c(0, max(temp_dual$res)), ylim = c(0,1)) +
      scale_x_continuous(breaks = seq(0, max(temp_dual$res), by = 15), 
                         expand = c(0,0.1)) +
      scale_y_continuous(breaks = seq(0,1, by = 0.1), expand = c(0,0))
  } else {
    next
  }
  
  # Guardar gráfico en su carpeta correspondiente
  temp_name_plotD <- paste0(substr(a, 1, 9), "D.png") # generar nombres
  ggsave(plot = p1, filename = (paste0("TEST/PLOTS/DUAL_PLOT/", temp_name_plotD)), 
         device = "png", width = 8, height = 5, dpi = 650)
  
}










# Determinación de coverage de dualidad para IDRs----

# Generar una variable con todos los nombres de los
# archivos por analizar

list_txt <- dir(path = "TEST/DUALIDAD/",
                pattern = "*.txt")

# Cargar cada archivo con los datos de dualidad calculados
# anteriormente. 

temp_dual <- c() #vector vacío
coverage <- c() #vector vacío
for (a in list_txt) {
  
  temp_dual <- read.table(file = paste0("TEST/DUALIDAD/", a), header = TRUE, sep = ",")
  
  # Contar la cantidad de residuos con dualidad (mayor a 0.1) 
  # y dividirlos entre la longitud de la proteína (cantidad de
  # residuos) y multiplicar por 100 
  
  i <- table(temp_dual$dualidad >= 0.1)[2] #Obtener residuos duales
  h <- table(temp_dual$dualidad >= 0.1)[2] + table(temp_dual$dualidad >= 0.1)[1] #Total
  
  # Obtener el porcentaje dividiendo a entre b y multiplicando por
  # 100
  coverage[length(coverage) + 1] <- (i/h)*100 #Dualidad coverage

  # Agregar condición para que los valores NA sean convertidos a ceros
  coverage[is.na(coverage)] <- 0
  
}

# Convertir en un dataframe los datos con un ID general
coverage <- data.frame("ID_biosensor" = substr(list_txt, 1, 9), 
                       "coverage" = coverage)







