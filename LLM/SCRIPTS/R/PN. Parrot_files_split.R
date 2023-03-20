# Paqueterías
library(dplyr)
library(ggplot2)

# Cargar datos de delta FRET
df_delta <- read.table(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/DATABASES/delta_seq_parrot.tsv", 
                       header = TRUE, sep = "\t")

# Determinar la cantidad de biosensores de respuesta
# alta, media y baja

# Biosensores de respuesta alta, baja e intermedia 
# de acuerdo con los cuartiles
temp_25 <- unname(quantile(df_delta$mean_delta)[2]) # 25%
temp_75 <- unname(quantile(df_delta$mean_delta)[4]) # 75%

# Cuartiles de 25, 50 y 75%
temp_75_delta <- df_delta[df_delta$mean_delta > temp_75, ] # Alta
temp_25_delta <- df_delta[df_delta$mean_delta < temp_25, ] # Baja
temp_50_delta <- df_delta[df_delta$mean_delta <= temp_75 &
                            df_delta$mean_delta >= temp_25, ]

# Agregar columnas de respuesta a cada data frame
temp_25_delta$Response <- "Baja"
temp_50_delta$Response <- "Media"
temp_75_delta$Response <- "Alta"

# Juntar dataframes anteriores
df_delta <- rbind(temp_25_delta, temp_50_delta, temp_75_delta)

# Ordenar datos de df_delta
df_delta <- df_delta[order(df_delta$construct, decreasing = FALSE), ]

# Clasificar datos de la siguiente forma:
# Alta: 2
# Media: 1
# Baja: 0
temp <- ifelse(df_delta$Response == "Alta", "2", 
        ifelse(df_delta$Response == "Media", "1", "0"))

# Sustituir columna
df_delta$Response <- temp

# Remover mean_delta
df_delta$mean_delta <- NULL

# Guardar archivo
write.table(x = df_delta, file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/parrot/delta_seq_parrot_class.tsv", 
            quote = FALSE, row.names = FALSE, sep = "\t")

# Verificar la cantidad de grupos de respuesta
table(df_delta$Response)

# Obtener un número aleatorio de secuencias
set.seed(1)

# Obtener muestra aleatoria de 11 secuencias de respuesta alta
i <- sample_n(df_delta[df_delta$Response == "2", ], size = 44)

# Obtener muestra aleatoria de 11 secuencias de respuesta media
h <- sample_n(df_delta[df_delta$Response == "1", ], size = 44)

# Obtener muestra aleatoria de 11 secuencias de respuesta baja
j <- sample_n(df_delta[df_delta$Response == "0", ], size = 44)

# Juntar data frames
df_delta_1 <- rbind(i, h, j)

# Guardar archivo del 66% de muestras
write.table(x = df_delta_1, file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/parrot/delta_seq_parrot_class_80p.tsv", 
            quote = FALSE, row.names = FALSE, sep = "\t")

# Obtener el otro 44% restante para los datos de prueba
df_delta_2 <- df_delta[!df_delta$construct %in% df_delta_1$construct, ]

# Guardar archivo del 20% de muestras
write.table(x = df_delta_2, file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/parrot/delta_seq_parrot_class_test.tsv", 
            quote = FALSE, row.names = FALSE, sep = "\t")

# Obtener una muestra representativa del archivo de 80%
set.seed(3)

# Obtener muestra aleatoria de 11 secuencias de respuesta alta
i <- sample_n(df_delta_1[df_delta_1$Response == "2", ], size = 11)

# Obtener muestra aleatoria de 11 secuencias de respuesta media
h <- sample_n(df_delta_1[df_delta_1$Response == "1", ], size = 11)

# Obtener muestra aleatoria de 11 secuencias de respuesta baja
j <- sample_n(df_delta_1[df_delta_1$Response == "0", ], size = 11)

# Juntar data frames
df_delta_3 <- rbind(i, h, j)

# Guardar archivo del 20.63% del 80% de muestras
write.table(x = df_delta_3, file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/parrot/delta_seq_parrot_class_optmization.tsv", 
            quote = FALSE, row.names = FALSE, sep = "\t")

# Obtener 99 IDRs para el entrenamiento
df_delta_4 <- df_delta_1[!df_delta_1$construct %in% df_delta_3$construct, ]

# Guardar archivo de entrenamiento
write.table(x = df_delta_3, file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/parrot/delta_seq_parrot_class_training.tsv", 
            quote = FALSE, row.names = FALSE, sep = "\t")


# Verificar la distribución de los datos
ggplot(data = df_delta) +
  geom_bar(aes(x = Response, y = ..count../sum(..count..), 
               fill = Response), show.legend = FALSE, 
           color = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Response", y = "Frequency") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)


# Distribución de las 160 IDRs
ggplot(data = df_delta_1) +
  geom_bar(aes(x = Response, y = ..count../sum(..count..), 
               fill = Response), show.legend = FALSE, 
           color = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Response", y = "Frequency") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.4)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_80p.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_80p.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)



# Distribución de las 68 IDRs
ggplot(data = df_delta_2) +
  geom_bar(aes(x = Response, y = ..count../sum(..count..), 
               fill = Response), show.legend = FALSE, 
           color = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Response", y = "Frequency") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.85)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_test.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_test.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)



# Distribución de las 33 IDRs
ggplot(data = df_delta_3) +
  geom_bar(aes(x = Response, y = ..count../sum(..count..), 
               fill = Response), show.legend = FALSE, 
           color = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Response", y = "Frequency") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.4)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_opt.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_opt.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)



# Distribución de las 33 IDRs
ggplot(data = df_delta_4) +
  geom_bar(aes(x = Response, y = ..count../sum(..count..), 
               fill = Response), show.legend = FALSE, 
           color = "black", size = 0.6) +
  theme_classic() +
  labs(x = "Response", y = "Frequency") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0, 0.4)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_predict.pdf",
       device = "pdf", width = 4, height = 5, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_IDP_D2P2/PLOTS/LOCALCIDER/NCPR/parrot_response_distribution_predict.png",
       device = "png", width = 4, height = 5, units = "in", dpi = 400)


