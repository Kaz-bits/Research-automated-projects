# Paqueterías
library(ggplot2)
library(Rtsne)

# Cargar datos de cider
cider <- read.csv(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/condensados/df_fret_hml_delta.csv",
                  header = TRUE, sep = ",")

# Remover datos duplicados del data frame
cider <- unique(cider)

# Generar una matriz con las variables a considerar
cider_matrix <- as.matrix(cider[, c(2, 15, 17)])

# Reproducibilidad
set.seed(1)

# Calcular el tSNE con Rtsne
cider_tsne <- Rtsne(cider_matrix)

# Convertir datos de tSNE a data frame
cider_tsne <- data.frame(x = cider_tsne$Y[, 1],
                         y = cider_tsne$Y[, 2])

# Agregar data frame al data frame de "cider"
cider <- cbind(cider, cider_tsne)

# Realizar gráfico
ggplot() +
  geom_point(data = cider, aes(x = x, y = y, 
                               color = Response)) +
  theme_bw() +
  labs(title = expression(Delta*"FRET vs kappa vs NCPR"), 
         x = "tSNE1", y = "tSNE2") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_color_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/tsne_fret_kappa_ncpr.pdf",
       device = "pdf", width = 4, height = 3, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/tsne_fret_kappa_ncpr.png",
       device = "png", width = 4, height = 3, units = "in", dpi = 400)




# Cargar datos de cider
cider <- read.csv(file = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/DATA/CLEAN/condensados/df_fret_hml_delta.csv",
                  header = TRUE, sep = ",")

# Remover datos duplicados del data frame
cider <- unique(cider)

# Generar una matriz con las variables a considerar
cider_matrix <- as.matrix(cider[, c(2, 9, 15)])

# Calcular el tSNE con Rtsne
cider_tsne <- Rtsne(cider_matrix)

# Convertir datos de tSNE a data frame
cider_tsne <- data.frame(x = cider_tsne$Y[, 1],
                         y = cider_tsne$Y[, 2])

# Agregar data frame al data frame de "cider"
cider <- cbind(cider, cider_tsne)

# Realizar gráfico
ggplot() +
  geom_point(data = cider, aes(x = x, y = y, 
                               color = Response)) +
  theme_bw() +
  labs(title = expression(Delta*"FRET vs kappa vs NCPR"), 
       x = "tSNE1", y = "tSNE2") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_color_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PDF/tsne_fret_kappa_ncpr.pdf",
       device = "pdf", width = 4, height = 3, units = "in", dpi = 400)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/ALL/Project_NN_proteins/PLOTS/obs/PNG/tsne_fret_kappa_ncpr.png",
       device = "png", width = 4, height = 3, units = "in", dpi = 400)


