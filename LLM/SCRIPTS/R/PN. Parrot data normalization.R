# Cargar datos
df_parrot <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
                        header = TRUE, sep = "\t")

# Extraer datos de delta FRET
values <- df_parrot$mean_delta

# Vector vacío
temp <- c()

# Iterar sobre cada valor de dleta FRET
for (i in values) {
  
  # Normalizar los datos
  normalized <- ((i - min(values)) / (max(values) - min(values)))
  
  # Juntar valores normalizados
  temp[length(temp) + 1] <- normalized
  
}

# Agregar datos a la columna de df_parrot
df_parrot$mean_delta <- round(temp, 2)

# Eliminar datos repetidos del delta FRET
df_parrot <- df_parrot[!duplicated(df_parrot$mean_delta), ]

# Remover el valor más alto de CAHS
df_parrot <- df_parrot[!df_parrot$construct == 163, ]

# Guardar datos
write.table(x = df_parrot, file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot_normalized.tsv", 
            quote = FALSE, row.names = FALSE)


