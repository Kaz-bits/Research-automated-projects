# En este script se encuentran los comandos necesarios 
# para juntar todos los datos del cociente FRET de los 
# 200 biosensores analizados

# Definir directorio de la carpeta FRET
dir_fret <- "D:/FRET Script Biblioteca/data/FRET/"

# Obtener el nombre de todos los biosensores
list_names <- list.files("D:/FRET Script Biblioteca/data/IDR_Biosensors/Replica 1/")

# Construir data frame para juntar todos los datos
all_bios <- data.frame()
for (a in list_names) {
  
  # Directorio del archivo
  temp_file <- file.path(dir_fret, a, "DATA", paste0(a, ".csv"))
  
  # Verificar si existe el archivo
  temp <- file.exists(temp_file)
  if (temp == TRUE) {
    
    # Cargar archivos
    temp_file <- read.csv(file = file.path(temp_file), header = TRUE)
    
    # Juntar los datos
    all_bios <- rbind(temp_file, all_bios)
    
  }
}

# Ordenar columna por constructo
all_bios <- all_bios[order(all_bios$Construct, decreasing = FALSE), ]

# Guardar archivo
write.csv(x = all_bios,
          row.names = FALSE, quote = FALSE,
          file = file.path(dir_fret, "all_FRET_biosensors.csv"))
