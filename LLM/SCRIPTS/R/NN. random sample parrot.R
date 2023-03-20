# Cargar archivo
# Cargar archivo
NN_200 <- read.table(file = "/media/kazbits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
                     header = TRUE)

# Redondear datos de Delta FRET
NN_200$mean_delta <- round(NN_200$mean_delta, digits = 2)

# Guardar archivo con los decimales redondeados
write.table(x = NN_200, file = "/media/kazbits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
            quote = FALSE, sep = "\t", row.names = FALSE)

# Remover a CAHS
NN_200 <- NN_200[!NN_200$mean_delta == max(NN_200_sample$mean_delta), ]

# Guardar archivo con los decimales redondeados y sin CAHS
write.table(x = NN_200, file = "/media/kazbits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot_WCAHS.tsv", 
            quote = FALSE, sep = "\t", row.names = FALSE)

# Generar muestra aleatoria de 160 IDRs
set.seed(1)
NN_200_sample <- sample_n(NN_200, size = 160)

# Guardar archivo con los 160 datos
write.table(x = NN_200_sample, file = "/media/kazbits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot_80p.tsv", 
            quote = FALSE, sep = "\t", row.names = FALSE)

# Generar grupos del 20% del total
set.seed(2)
NN_200_sample_1 <- sample_n(NN_200_sample, size = 32)

# Guardar archivo con el 20% de los datos
write.table(x = NN_200_sample_1, file = "/media/kazbits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot_80p_20p.tsv", 
            quote = FALSE, sep = "\t", row.names = FALSE)

# Determinar normalidad
qqnorm(NN_200_sample_1$mean_delta)
qqline(NN_200_sample_1$mean_delta)
