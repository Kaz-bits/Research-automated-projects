# Cargar archivos de dualidad
# Lista de archivos por cargar
temp_files <- list.files("/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/")

# Vector vacìo
temp <- c()

# Leer cada archivo individual
for (a in temp_files) {
  
  # Cargar archivos
  temp_dual <- read.table(
    file = file.path(paste0("/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/", a)),
    header = TRUE, 
    sep = ","
  )
  
  # Determinar la longitud de la IDR
  i <- length(temp_dual$res)
  
  # Determinar la cantidad de residuos duales
  h <- sort(table(temp_dual$lógico), decreasing = TRUE)[[1]]
  
  # Calcular porcentaje de dualidad
  j <- ((h/i) * 100)
  
  # Agregar elementos al vector
  temp[length(temp) + 1] <- j
  
}

# Crear data frame con los datos de porcentaje dual
temp_dual <- data.frame("IDR" = as.numeric(substr(temp_files, 7, 9)), 
                        "Dualidad" = temp)


