#Paqueterías----

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)

#Gráfico de cajas con archivo completo----
#Generar un vector con los nombres de los archivos
temp <- list.files(path = "DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/FACTORS/")[-22]
temp <- substr(x = temp, start = 7, stop = 9)

for (b in temp) {
  
#Nombrar una variable con los datos del constructo deseado
fret_factor_box <- fret_factor[fret_factor$Constructo == b,]

#Extraer los valores de FRET normalizado para cada 
#concentración
i <- fret_factor_box[fret_factor_box$Tratamiento == 0.0,]$Normalización
h <- fret_factor_box[fret_factor_box$Tratamiento == 0.5,]$Normalización
j <- fret_factor_box[fret_factor_box$Tratamiento == 1.0,]$Normalización
k <- fret_factor_box[fret_factor_box$Tratamiento == 1.5,]$Normalización

#Prueba t-test para 0M y 0.5M
p1 <- t.test(i, h)[[3]]
#Prueba t-test para 0M y 1.0M
p2 <- t.test(i, j)[[3]]
#Prueba t-test para 0M y 1.5M
p3 <- t.test(i, k)[[3]]

#Nivel de significancia
signif_level <- c(ns = 1, "*" = 0.05, "**" = 0.01, 
                  "***" = 0.001, "****" = 0.0001)

#Obtener los valores de p de cada concentración
a <- c(p1, p2, p3)

#Evaluar el valor de p de las concentraciones
#Para 0.5 vs 0.0
i <- c(a[1] > 0.05, a[1] <= 0.05, a[1] <= 0.01, a[1] <= 0.001, a[1] <= 0.0001)
#Para 1.0 vs 0.0
h <- c(a[2] > 0.05, a[2] <= 0.05, a[2] <= 0.01, a[2] <= 0.001, a[2] <= 0.0001)
#Para 1.5 vs 0.0
j <- c(a[3] > 0.05, a[3] <= 0.05, a[3] <= 0.01, a[3] <= 0.001, a[3] <= 0.0001)

#Extraer los simbolos de significancia del valor de probabilidad
p1_box <- (names(signif_level[i]))[length((names(signif_level[i])))] #p1
p2_box <- (names(signif_level[h]))[length((names(signif_level[h])))] #p2
p3_box <- (names(signif_level[j]))[length((names(signif_level[j])))] #p3

#Gráfico de cajas  
plot <- ggplot() + 
  geom_boxplot(data = fret_factor_box, 
               aes(x = Tratamiento, 
                   y = Normalización, group = Tratamiento),
               fill = c("#53AEF5", "#538CF5", 
                        "#535DF5", "#3E04F2"),
               outlier.shape = NA) +
  geom_jitter(data = fret_factor_box, 
              aes(x = Tratamiento, 
                  y = Normalización, group = Tratamiento),
              color = "black") +
  labs(x = "[NaCl] (M)", y = "Normalized\nDxAm/DxDm") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0.85,3)) +
  scale_y_continuous(breaks = seq(0.85,3,0.5)) +
  annotate(geom = "text", x = 0.5, y = 2.5, label = p1_box) +
  annotate(geom = "text", x = 1, y = 2.5, label = p2_box) +
  annotate(geom = "text", x = 1.5, y = 2.5, label = p3_box)  +
  annotate(geom = "text", x = 0.75, y = 2.85, label = paste0("IDRBS-",b),
           size = 5)

#Guardar gráfico
ggsave(plot = plot, filename = file.path(paste0("PLOTS/FRET/Boxplots/new_box/All/", 
                                                "IDRFT-", b, ".png")),
       device = "png", width = 6, height = 4, units = "in", 
       dpi = 600) 


}


for (b in temp) {
fret_factor_box <- fret_factor[fret_factor$Constructo == b,]

#Extraer los valores de FRET normalizado para cada 
#concentración
i <- fret_factor_box[fret_factor_box$Tratamiento == 0.0,]$Normalización
h <- fret_factor_box[fret_factor_box$Tratamiento == 0.5,]$Normalización
j <- fret_factor_box[fret_factor_box$Tratamiento == 1.0,]$Normalización
k <- fret_factor_box[fret_factor_box$Tratamiento == 1.5,]$Normalización

#Prueba t-test para 0M y 0.5M
p1 <- t.test(i, h)[[3]]
#Prueba t-test para 0M y 1.0M
p2 <- t.test(i, j)[[3]]
#Prueba t-test para 0M y 1.5M
p3 <- t.test(i, k)[[3]]

#Nivel de significancia
signif_level <- c(ns = 1, "*" = 0.05, "**" = 0.01, 
                  "***" = 0.001, "****" = 0.0001)

#Obtener los valores de p de cada concentración
a <- c(p1, p2, p3)

#Evaluar el valor de p de las concentraciones
#Para 0.5 vs 0.0
i <- c(a[1] > 0.05, a[1] <= 0.05, a[1] <= 0.01, a[1] <= 0.001, a[1] <= 0.0001)
#Para 1.0 vs 0.0
h <- c(a[2] > 0.05, a[2] <= 0.05, a[2] <= 0.01, a[2] <= 0.001, a[2] <= 0.0001)
#Para 1.5 vs 0.0
j <- c(a[3] > 0.05, a[3] <= 0.05, a[3] <= 0.01, a[3] <= 0.001, a[3] <= 0.0001)

#Extraer los simbolos de significancia del valor de probabilidad
p1_box <- (names(signif_level[i]))[length((names(signif_level[i])))] #p1
p2_box <- (names(signif_level[h]))[length((names(signif_level[h])))] #p2
p3_box <- (names(signif_level[j]))[length((names(signif_level[j])))] #p3

#Gráfico de cajas  
plot <- ggplot() + 
  geom_boxplot(data = fret_factor_box, 
               aes(x = Tratamiento, 
                   y = Normalización, group = Tratamiento),
               fill = c("#53AEF5", "#538CF5", 
                        "#535DF5", "#3E04F2"),
               outlier.shape = NA) +
  geom_jitter(data = fret_factor_box, 
              aes(x = Tratamiento, 
                  y = Normalización, group = Tratamiento),
              color = "black") +
  labs(x = "[NaCl] (M)", y = "Normalized\nDxAm/DxDm") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0.85,3)) +
  scale_y_continuous(breaks = seq(0.85,3,0.5)) +
  annotate(geom = "text", x = 0.5, y = 2.5, label = p1_box) +
  annotate(geom = "text", x = 1, y = 2.5, label = p2_box) +
  annotate(geom = "text", x = 1.5, y = 2.5, label = p3_box)  +
  annotate(geom = "text", x = 0.75, y = 2.85, label = paste0("IDRBS-",b),
           size = 5) +
  xlab(NULL) + ylab(NULL)

#Asignar una variable a cada gráfico
assign(paste0("FT-", b), plot)

}

#Juntar todos los factores en un gráfico
fig_boxplot <- ggarrange(`FT-045`+ rremove("x.text"), 
                         `FT-054`+ rremove("xy.text"), 
                         `FT-064`+ rremove("xy.text"), 
                         `FT-077`+ rremove("xy.text"),
                         `FT-081`+ rremove("xy.text"), 
                         `FT-082`+ rremove("xy.text"), 
                         `FT-136`+ rremove("xy.text"), 
                         `FT-137`+ rremove("x.text"),
                         `FT-138`+ rremove("xy.text"), 
                         `FT-139`+ rremove("xy.text"), 
                         `FT-140`+ rremove("xy.text"), 
                         `FT-141`+ rremove("xy.text"),
                         `FT-142`+ rremove("xy.text"), 
                         `FT-143`+ rremove("xy.text"), 
                         `FT-144`, 
                         `FT-145`+ rremove("y.text"),
                         `FT-146`+ rremove("y.text"), 
                         `FT-147`+ rremove("y.text"), 
                         `FT-148`+ rremove("y.text"), 
                         `FT-149`+ rremove("y.text"),
                         `FT-165`+ rremove("y.text"),
                         nrow = 3, ncol = 7)

fig_boxplot <- ggarrange(`FT-045`,`FT-054`,`FT-064`,`FT-077`,
                         `FT-081`,`FT-082`,`FT-136`,`FT-137`,
                         `FT-138`,`FT-139`,`FT-140`,`FT-141`,
                         `FT-142`,`FT-143`,`FT-144`,`FT-145`,
                         `FT-146`,`FT-147`,`FT-148`,`FT-149`,
                         `FT-165`,
                         nrow = 3, ncol = 7)


fig_boxplot <- annotate_figure(fig_boxplot,
                               left = textGrob("normalized DxAm/DxDm", 
                                               vjust = 0.5, rot = 90, 
                                               gp = gpar(cex = 1.5)),
                               bottom = textGrob("[NaCl] (M)",
                                                 gp = gpar(cex = 1.5), 
                                                 hjust = 0.5))

#Guardar gráfico
ggsave(plot = fig_boxplot, 
       filename = "PLOTS/FRET/Boxplots/new_box/All/all_boxplots.png", 
       device = "png", width = 18, height = 9, units = "in", dpi = 600)

#Guardar gráfico
ggsave(plot = fig_boxplot, 
       filename = "PLOTS/FRET/Boxplots/new_box/All/all_boxplots.pdf", 
       device = "pdf", width = 18, height = 9, units = "in", dpi = 600)

