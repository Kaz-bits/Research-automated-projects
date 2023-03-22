# Paqueterías
library(ggplot2)

# Cargar archivo de NCPR por residuo
temp_ncpr <- read.csv(file = "E:/ALL/Project_IDP_D2P2/DATA/DATA FOR R/LIBRARY/IDRBS_library_200_NCPR.csv", header = TRUE)[-1, ]

# Extraer los identificadores de biosensores
temp_names <- list.files(path = "E:/ALL/Project_IDP_D2P2/DATA/DATA FOR R/LIBRARY/Desorden por residuo IDRBS/")
temp_names <- substr(temp_names, 1, 9)

# Agregar los identificadores al data frame
temp_ncpr$X <- temp_names

# Cambiar nombre de la primer columna
names(temp_ncpr)[1] <- "Construct"


# Extraer datos de NCPR de cada biosensor en forma de data frame
for (a in temp_names) {
  
  # Leer cada archivo de desorden por residuo
  temp_disor <- read.table(file = file.path("E:/ALL/Project_IDP_D2P2/DATA/DATA FOR R/LIBRARY/Desorden por residuo IDRBS", paste0(a, ".txt")), 
                           header = TRUE)
  
  # Extraer datos de NCPR por cada biosensor
  temp_ncpr_p <- as.data.frame(t(temp_ncpr[temp_ncpr$Construct == a, ][-1]))
  
  # Eliminar NAs
  temp_ncpr_p <- data.frame("NCPR" = temp_ncpr_p[!is.na(temp_ncpr_p), ])
  
  # Juntar los datos de desorden y NCPR en un mismo data frame
  temp_all <- cbind(temp_disor, temp_ncpr_p)
  
  # Vector vacío
  temp <- c()
  
  # Agregar variable categórica para identificar residuos cargados
  for (b in temp_all$NCPR) {
    
    # Verificar si es negativo o positivo
    if (b == 0) {
      temp[length(temp) + 1] <- "No charge"
      
    } else if (b < 0) {
      temp[length(temp) + 1] <- "Negativo"
      
    } else {
      temp[length(temp) + 1] <- "Positivo"
      
    }
  }
  
  # Agregar variable temp a "temp_all"
  temp_all$Charge <- temp
  
  # Guardar archivos de desorden y NCPR
  write.csv(x = temp_all, 
            file = file.path(paste0("E:/ALL/Project_IDP_D2P2/DATA/DATA FOR R/LIBRARY/NCPR_Constanza/", a, ".csv")))
  
}



# Cargar cada uno de los archivos generados en ciclo for
# Obtener la lista de los archivos
temp_list <- list.files(path = "C:/Users/HP/Desktop/NCPR PLOTS/NCPR_Constanza/")

for (a in temp_list) {
  
  # Leer cada uno de los archivos
  temp <- read.csv(file = file.path("C:/Users/HP/Desktop/NCPR PLOTS/NCPR_Constanza/", paste0(substr(a, 1, 9), ".csv")))
  
  # Generar gráfico
  # Gráfico de NCPR y desorden por residuo
  p <- ggplot(data = temp) +
    geom_col(aes(x = Num, y = NCPR, fill = Charge), show.legend = FALSE) +
    geom_line(aes(x = Num, y = VLXT), size = 0.8) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 0.5, lty = 2) + 
    theme_bw() +
    labs(x = "Residuo", y = "NCPR") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank()) +
    scale_fill_manual(name = NULL, 
                      values = c("Red", "white", "blue"))
  
  # Guardar los archivos en pdf
  ggsave(plot = p, filename = file.path("C:/Users/HP/Desktop/NCPR PLOTS/PLOTS NCPR/PDF/", paste0(substr(a, 1, 9),".pdf")), 
         device = "pdf", width = 8, height = 4, units = "in", dpi = 450)
  
  # Guardar los archivos en png
  ggsave(plot = p, filename = file.path("C:/Users/HP/Desktop/NCPR PLOTS/PLOTS NCPR/PDF/", paste0(substr(a, 1, 9),".png")), 
         device = "png", width = 8, height = 4, units = "in", dpi = 450)
  
} 
