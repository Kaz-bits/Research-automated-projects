# NOTA: LOS DIRECTORIOS SE DEBEN ESCRIBIR, Y NO SE DEBEN
# COPIAR Y PEGAR, YA QUE SE GENERAN ERRORES DE LECTURA
# DEBIDO A CARACTERES "INSIVISIBLES"

# Función de análisis de datos de FRET----
FRET.data <- function(dir.input, dir.output, 
                      isosbestic = 515, 
                      em.min = 460, 
                      em.max = 550,
                      DxAm = 525,
                      DxDm = 475) {
  
  # Crear carpeta para guardar los archivos analizados
  sub_dir <- "FRET" 
  temp_dir <- file.path(dir.output, sub_dir)
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Obtener el nombre de la carpeta de las r?plicas
  temp_files <- list.files(dir.input)
  
  # Obtener los nombres de los biosensores en cada carpeta
  for (i in temp_files) {
    
    if (i == temp_files[1]) {
      
      temp_rep_1 <- list.files(file.path(dir.input, i))
      
    }
    
    if (i == temp_files[2]) {
      
      temp_rep_2 <- list.files(file.path(dir.input, i))
      
    }
    
    if (i == temp_files[3]) {
      
      temp_rep_3 <- list.files(file.path(dir.input, i))
      
    }
  }
  
  # Generar carpetas con los nombres de los biosensores analizado
  
  for (i in temp_rep_1) {
    
    # Crear subcarpetas para cada biosensor
    sub_dir <- i
    dir.create(file.path(temp_dir, sub_dir), 
               showWarnings = FALSE)
    
  }
  
  # Crear carpetas "DATA" y "PLOTS"
  for (i in temp_rep_1) {
    
    sub_dir <- "DATA"
    dir.create(file.path(dir.output, "FRET", i, sub_dir), 
               showWarnings = FALSE)
    
  }
  
  for (i in temp_rep_1) {
    
    sub_dir <- "PLOTS"
    dir.create(file.path(dir.output, "FRET", i, sub_dir), 
               showWarnings = FALSE)
    
  }
  
  
  # Leer cada uno de los archivos en cada carpeta
  
  for (bios in temp_rep_1) {
    for (reps in temp_files) {
      for (plate in c(1, 2)) {
        
        
        # Verificar si el directorio existe
        temp_path <- file.path(dir.input, reps, bios, 
                               paste0(plate, ".xlsx"))
        
        if (file.exists(temp_path) == TRUE) {
          
          # Cargar archivo de excel
          fret <- readxl::read_excel(path = file.path(dir.input, reps, bios, 
                                                      paste0(plate, ".xlsx")),
                                     sheet = 1, 
                                     col_names = FALSE)[-c(1:9), ]
          
          # Agregar los nombres a las columnas para identificarlas
          names(fret)[1:length(names(fret))] <- unlist(fret[2, ], use.names = FALSE)
          
          # Eliminar el primer rengl?n 
          fret <- fret[-c(1, 2), ]
          
          # Crear un dataframe con el numero de columnas seg?n
          # la cantidad de datos por analizar
          temp_fret <- data.frame(matrix(ncol = (length(fret[, -c(1, 2)])/(3)), 
                                         nrow = nrow(fret)))
          temp_vec <- c()
          for (a in 1:length(temp_fret)) {
            
            temp_name <- paste0("Condition_", a)
            temp_vec[length(temp_vec) + 1] <- temp_name
            
          }
          
          # Agregar nombres espec?ficos a cada una de las
          # columnas del dataframe
          colnames(temp_fret) <- temp_vec
          
          # Obtener Mean de cada condici?n para cada
          # una de las longitudes de onda
          wl <- ((em.max - em.min) + 1) # Rango de longitudes de onda
          
          temp_df <- data.frame(matrix(ncol = 4, nrow = wl))
          for (a in (seq(from = 1, to = length(fret[, -c(1, 2)]), by = 3) + 2)) {
            
            # Obtener los valores para cada longitud
            # de onda para cada condici?n
            temp <- as.numeric(unlist((fret[, a])))
            temp_df[, 1] <- temp
            
            temp <- as.numeric(unlist((fret[, (a + 1)])))
            temp_df[, 2] <- temp
            
            temp <- as.numeric(unlist((fret[, (a + 2)])))
            temp_df[, 3] <- temp
            
            # Obtener la media de las tres observaciones
            i <- (temp_df[, 1] + temp_df[, 2] + temp_df[, 3])
            h <- (length(temp_df)-1)
            
            # Media de las condiciones
            temp <- (i/h)
            temp_df[, 4] <- temp
            
            # Agregar cada columna con cada Mean al
            # dataframe llamado "temp_fret"
            
            temp_fret[, (a/3)] <- temp_df[, 4]
            
          }
          
          # Agregar la longitud de onda en la primer columna
          temp_fret[, length(temp_fret) + 1] <- fret$`Wavelength [nm]`
          temp_fret <- temp_fret[, c(length(temp_fret), 1:(length(temp_fret) - 1))]
          names(temp_fret)[1] <- names(fret)[2]
          
          # Normalizar fluorescencia con punto isosb?stico
          
          temp <- (t(temp_fret[temp_fret[, 1] == as.character(isosbestic),
                               c(2:length(temp_fret))]))
          rownames(temp) <- NULL
          
          # Emplear el vector "temp" creado para normalizar
          # cada valor de "temp_fret"
          temp_fret_N <- data.frame(matrix(ncol = (length(fret[, -c(1,2)])/(3)), 
                                           nrow = nrow(fret)))
          temp_vec <- c()
          for (a in 1:length(temp_fret_N)) {
            
            temp_name <- paste0("Condition_", a)
            temp_vec[length(temp_vec) + 1] <- temp_name
            
          }
          
          
          if (plate == 1) {
            
            # Agregar nombres espec?ficos a cada una de las
            # columnas del dataframe
            colnames(temp_fret_N) <- temp_vec
            
          }
          
          if (plate == 2) {
            
            # Cambiar los nombres de las columnas
            temp_vec[1] <- "Condition_5"
            temp_vec[2] <- "Condition_6"
            temp_vec[3] <- "Condition_7"
            temp_vec[4] <- "Condition_8"
            
            # Agregar nombres espec?ficos a cada una de las
            # columnas del dataframe
            colnames(temp_fret_N) <- temp_vec
            
          }
          
          # Normalizar los datos
          for (a in 1:nrow(temp_fret)) {
            
            i <- (temp_fret[a, -1])/(temp)
            temp_fret_N[a, ] <- i
            
          }
          
          # Agregar la longitud de onda en la primer columna
          temp_fret_N[, length(temp_fret_N) + 1] <- fret$`Wavelength [nm]`
          temp_fret_N <- temp_fret_N[, c(length(temp_fret_N), 1:(length(temp_fret_N) - 1))]
          names(temp_fret_N)[1] <- names(fret)[2]
          
          # Añadir columna con la réplica
          temp_fret_N$Replicate <- reps
          
          # Añadir columna con número de placa
          temp_fret_N$Plate <- plate
          
          # Añadir columna con el biosensor
          temp_fret_N$Construct <- bios
          
          # Guardar archivo en las carpetas "DATA" de cada biosensor
          write.csv(x = temp_fret_N,
                    file = file.path(dir.output, "FRET", bios, "DATA", 
                                     paste0("sptr_", bios, "_", reps, "_", 
                                            "plate", plate, ".csv")
                    ),
                    row.names = FALSE, 
                    quote = FALSE
          )
          
          
          
          # Obtención cociente DxAm y DxDm
          # Extraer la cantidad de constructos que tienen los datos
          i <- length(fret[, -c(1, 2)])/12 # Cantidad de constructos
          
          # Generar dataframe donde guardar datos
          temp_df <- data.frame(matrix(ncol = (length(fret[, -c(1, 2)])/i), 
                                       nrow = 0))
          
          # Obtener los renglones con las fluorescencias del donador y
          # aceptor
          temp_rep <- fret[fret$`Wavelength [nm]` == as.character(DxDm) | 
                             fret$`Wavelength [nm]` == as.character(DxAm), ]
          
          temp_vec <- c() # vector vac?o
          # Ejecutar c?digo
          for (a in seq(1, length(fret[, -c(1, 2)]), 12)) {
            
            temp_vec <- temp_rep[, seq(a + 2, a + 13)]
            temp_df[nrow(temp_df) + 1, ] <- temp_vec[1, ] 
            temp_df[nrow(temp_df) + 1, ] <- temp_vec[2, ] 
            
          }
          
          temp_df[, length(temp_df) + 1] <- c(as.character(DxDm), as.character(DxAm))
          temp_df <- temp_df[, c(length(temp_df), (1:length(temp_df) - 1))]
          names(temp_df)[1] <- names(fret)[2]
          
          # Crear nombres acorde a la placa empleada
          
          if (plate == 1) {
            
            # Crear nombres
            temp_name <- c(rep(paste0("NaCl_", "0mM"), 3), rep(paste0("NaCl_", "200mM"), 3),
                           rep(paste0("NaCl_", "400mM"), 3), rep(paste0("NaCl_", "600mM"), 3))
            
            # Agreagr nombres a las columnas por condici?n
            names(temp_df)[2:length(temp_df)] <- temp_name
            
          }
          
          if (plate == 2) {
            
            # Crear nombres
            temp_name <- c(rep(paste0("NaCl_", "0mM"), 3), rep(paste0("NaCl_", "800mM"), 3),
                           rep(paste0("NaCl_", "1000mM"), 3), rep(paste0("NaCl_", "1500mM"), 3))
            
            # Agreagr nombres a las columnas por condici?n
            names(temp_df)[2:length(temp_df)] <- temp_name
            
          }
          
          # Una vez obtenido el dataframe con los datos de
          # fluorescencia del donador y aceptor, realizar
          # la Normalized
          
          # Generar dataframe para los datos normalizados
          temp_rep <- data.frame(matrix(ncol = 9, 
                                        nrow = (12*length(fret[, -c(1,2)])/12)))
          
          # Agregar los nombres a las columnas
          names(temp_rep)[1] <- "Treatment"
          names(temp_rep)[2] <- "DxAm"
          names(temp_rep)[3] <- "DxDm"
          names(temp_rep)[4] <- "DxAm/DxDm"
          names(temp_rep)[5] <- "Mean"
          names(temp_rep)[6] <- "Normalized"
          names(temp_rep)[7] <- "Replicate"
          names(temp_rep)[8] <- "Construct"
          names(temp_rep)[9] <- "Plate"
          
          
          # Crear condicional para el Treatment
          if (plate == 1) {
            
            # Agregar columna de Treatment
            temp_rep$Treatment <- rep(c(0, 200, 400, 600), each = 3)
            temp_rep$Plate <- 1
          }
          
          if (plate == 2) {
            
            # Agregar columna de Treatment
            temp_rep$Treatment <- rep(c(0, 800, 1000, 1500), each = 3)
            temp_rep$Plate <- 2
            
          }
          
          
          # Agregar columna de DxAm. Extraer de "temp_df" todos los datos
          # de longitud de onda 525 en orden
          
          temp_vec_525 <- c() # vector vacío
          temp_vec <- temp_df[seq(2, nrow(temp_df), by = 2), -1]
          for (a in 1:nrow(temp_df[seq(2, nrow(temp_df), by = 2), -1])) {
            
            temp_vec_525 <- c(temp_vec_525, as.numeric(temp_vec[a, ]))
            
          }
          
          
          # Agregar columna de DxDm. Extraer de "temp_df" todos los datos
          # de longitud de onda 475 en orden
          
          temp_vec_475 <- c() # vector vac?o
          temp_vec <- temp_df[seq(1, nrow(temp_df), by = 2), -1]
          for (a in 1:nrow(temp_df[seq(1, nrow(temp_df), by = 2), -1])) {
            
            temp_vec_475 <- c(temp_vec_475, as.numeric(temp_vec[a, ]))
            
          }
          
          # A?adir columnas finales
          temp_rep$DxAm <- temp_vec_525
          temp_rep$DxDm <- temp_vec_475
          
          # Agregar el cociente DxAm/DxDm
          temp_rep$`DxAm/DxDm` <- temp_vec_525/temp_vec_475
          
          # Agregar el Mean de DxAM/DxDm del Treatment a 
          # 0 NaCl para cada constructo 
          
          # Extraer todos los datos para NaCl 0 de cada constructo
          # y sumarlos
          h <- temp_rep[seq(1, nrow(temp_rep), 12), 4]
          i <- temp_rep[(seq(1, nrow(temp_rep), 12) + 1), 4]
          j <- temp_rep[(seq(1, nrow(temp_rep), 12) + 2), 4]
          
          # Sumar todos los datos y dividirlos entre tres. A?adir
          # cada valor del vector cada 12 veces para que corresponda
          # con el valor de cada constructo a NaCl 0M
          temp_rep$Mean <- rep((h + i + j)/(3), each = 12)
          
          # Dividir cada valor del Mean con el valor de la columna
          # DxAm/DxDm para obtener la Normalized
          temp_rep$Normalized <- (temp_rep$`DxAm/DxDm`)/(temp_rep$Mean)
          
          # A?adir la Replicate con la que se est? trabajando (revisar)
          temp_rep$Replicate <- as.numeric(substr(start = nchar(reps), 
                                                  stop = nchar(reps), 
                                                  x = reps))
          
          # A?adir el biosensor analizado
          temp_rep$Construct <- bios
          
          # Generar seis variables para guardar los datos de cada
          # constructo de forma independiente
          
          # Evaluar r?plica 1
          if (reps == temp_files[1] && plate == 1) {
            
            temp_rep1 <- temp_rep
            
          } 
          
          if (reps == temp_files[1] && plate == 2) {
            
            temp_rep2 <- temp_rep
            
          } 
          
          if (reps == temp_files[2] && plate == 1) {
            
            temp_rep3 <- temp_rep
            
          } 
          
          if (reps == temp_files[2] && plate == 2) {
            
            temp_rep4 <- temp_rep
            
          } 
          
          if (reps == temp_files[3] && plate == 1) {
            
            temp_rep5 <- temp_rep
            
          } 
          
          if (reps == temp_files[3] && plate == 2) {
            
            temp_rep6 <- temp_rep
            
          } 
          
          
          if (exists("temp_rep6") == TRUE) {
            
            # Juntar dataframes
            fret_all <- rbind(temp_rep1, temp_rep2, temp_rep3, 
                              temp_rep4, temp_rep5, temp_rep6)
            
            # Guardar archivo con todos los datos del constructo
            write.csv(x = fret_all,
                      file = file.path(dir.output, "FRET", bios, "DATA",
                                       paste0(bios, ".csv")
                      ),
                      row.names = FALSE,
                      quote = FALSE 
            )
            
            # Eliminar variable fret_all para el siguiente biosensor
            if (exists("fret_all") == TRUE) {
              
              remove(... = fret_all)
              
            }
          }
          
        } else {
          
          next
          
        }
      }
    } 
  }
}
