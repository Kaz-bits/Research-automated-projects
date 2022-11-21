# Cargar archivo de la biblioteca de biosensores
bios <- readxl::read_xlsx(path = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/DATA FOR ANALYSIS/200 IDRBS Library.xlsx", 
                          sheet = 1, col_names = TRUE)

# Extraer las columnas 1 y 25 correspondientes al ID y las secuencias
bios <- bios[, c(1,25)]

# Generar archivo fasta
temp <- c() 
for (a in 1:nrow(bios)) {
  
  # Extraer el ID
  temp[length(temp) + 1] <- paste0(">", bios$Entry[a])
  
  # Extraer la secuencia del ID anterior
  temp[length(temp) + 1] <- bios$`IDR sequence to order`[a]
  
}

# Guardar archivo
write.table(x = temp, file = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/DATA FOR ANALYSIS/IDRBS_Library_200.txt", 
            quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)


