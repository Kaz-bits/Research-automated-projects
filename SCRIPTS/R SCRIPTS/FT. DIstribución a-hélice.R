#Paqueterías----
library(ggplot2)
library(ggpubr)

#Distribución de alfa hélico en los factores----

#Obtener media de a-hélice
mean(df1_TFs$alfa_score) #3.223

#Obtener desviación estándar
sd(df1_TFs$alfa_score) #6.664

qnorm(p = 0.90, mean = mean(df1_TFs$alfa_score), sd = sd(df1_TFs$alfa_score))
qnorm(p = 0.95, mean = mean(df1_TFs$alfa_score), sd = sd(df1_TFs$alfa_score))
qnorm(p = 0.99, mean = mean(df1_TFs$alfa_score), sd = sd(df1_TFs$alfa_score))

#Gráfico de distribución de alfa score----

#Fórmula para el ancho de banda
bins <- function(x) {
  if ((2 * IQR(x, na.rm = TRUE) / length(x)^(1/3)) > 0.05) {
    ceiling(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3))
  } else {
    round(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3), digits = 2)
  }
}

#Valor de ancho de banda 
bin_alfa <- bins(df1_TFs$alfa_score)
bin_sub1 <- bins(sublist_1$alfa_score)
bin_sub1A <- bins(sublist_1A$alfa_score)
bin_sub1B <- bins(sublist_1B$alfa_score)

#Gráfico de distribución de alfa score
ggplot() + 
  geom_histogram(data = df1_TFs, aes(y = ..ncount..,x = alfa_score),
                 binwidth = bin_alfa, color = "black", fill = "steelblue") +
  labs(x = "Porcentaje de alfa-hélice", 
       y = "Frecuencia relativa") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,100, by = 5), expand = c(0.01,0)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), expand = c(0,0)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

#Gráfico de predicción de alfa score vs % desordenado----

ggplot() +
  geom_jitter(data = sublist_1, aes(x = Porcentaje_desordenado, 
                                    y = alfa_score, color = "Mayor a 14.18"), 
              size = 1.5) +
  geom_jitter(data = sublist_2, aes(x = Porcentaje_desordenado,
                                    y = alfa_score, color = "Menor a 14.18"),
              alpha = 0.8) +
  geom_hline(mapping = aes(yintercept = 11.763, color = "124 datos (90%)"), lty = 2,
             size = 1) +
  geom_hline(mapping = aes(yintercept = 14.185, color = "95 datos (95%)"), lty = 2,
             size = 1) +
  geom_hline(mapping = aes(yintercept = 18.726, color = "69 datos (99%)"), lty = 2,
             size = 1) +
  theme_light() +
  labs(x = "Porcentaje de desorden",
       y = "Predicción de alfa-hélice",
       fill = "Valor") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0,100, by = 15), expand = c(0.01,0)) +
  scale_y_continuous(breaks = seq(0,100, by = 15), expand = c(0.01,0)) +
  scale_color_manual(values = c("darkgreen", "darkorange", "brown", "steelblue", "gray"),
                     name = "DIstribución de\ndatos")

#Gráficos de distribución para el 90% de datos

ggplot() +
  geom_density(data = sublist_1, aes(x = alfa_score, fill = "95%"),
               alpha = 0.3) +
  geom_density(data = sublist_1A, aes(x = alfa_score, fill = "90%"),
               alpha = 0.3) +
  geom_density(data = sublist_1B, aes(x = alfa_score, fill = "99%"),
               alpha = 0.3) +
  scale_x_continuous(breaks = seq(0,100, by = 15), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_light() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(name = "Distribución de\ndatos", 
                    values = c("steelblue", "darkorange", "darkgreen"),
                    labels = c("90% (124 datos)", "95% (95 datos)", "99% (69 datos)"))

ggsave(filename = "Corte_IDRs.png", device = "png", width = 8, height = 5, 
       units = "in", dpi = 650)


#Corte delas IDRs respecto a % de desorden----

qnorm(p = 0.90, mean = mean(df1_TFs$Porcentaje_desordenado), sd = sd(df1_TFs$alfa_score))
qnorm(p = 0.95, mean = mean(df1_TFs$Porcentaje_desordenado), sd = sd(df1_TFs$alfa_score))
qnorm(p = 0.99, mean = mean(df1_TFs$Porcentaje_desordenado), sd = sd(df1_TFs$alfa_score))


#Empleando un valor de probabilidad del 95% para el % de desorden, se obtiene
#un valor de 54.632, por lo que las sublistas se acotan a dichos parámetros:
#alfa score de 14.185 y desorden 54.632


#Generación de sub-listas de los IDRs para factores----

#A partir del valor de qqplot obtenido para una probabilidad del
#95%, realizar un corte de la base de datos obteniendo alfa-score
#AGI y porcentaje de desorden (14.18463)

sublist_1 <- df1_TFs[c(df1_TFs$alfa_score >= 14.185 & 
                         df1_TFs$Porcentaje_desordenado >= 52.215),][,c(1,4:6,12)]

sublist_1A <- df1_TFs[c(df1_TFs$alfa_score >= 11.763 & 
                         df1_TFs$Porcentaje_desordenado >= 54.632),][,c(1,4:6,12)]

sublist_1B <- df1_TFs[c(df1_TFs$alfa_score >= 18.726 & 
                         df1_TFs$Porcentaje_desordenado >= 59.178),][,c(1,4:6,12)]


#Sublista 2 con valores contrarios a los descritos anteriormente


sublist_2 <- df1_TFs[c(df1_TFs$alfa_score < 14.185 & 
                         df1_TFs$Porcentaje_desordenado < 52.215),][,c(1,4:6,12)]

sublist_2A <- df1_TFs[c(df1_TFs$alfa_score < 11.763 & 
                          df1_TFs$Porcentaje_desordenado < 54.632),][,c(1,4:6,12)]

sublist_2B <- df1_TFs[c(df1_TFs$alfa_score < 18.726 & 
                          df1_TFs$Porcentaje_desordenado < 59.178),][,c(1,4:6,12)]

#Sublistas aplicando el alfa-score unicamente----

sublist_1 <- df1_TFs[df1_TFs$alfa_score >= 14.185,][,c(1,4:6,12)]
sublist_1A <- df1_TFs[df1_TFs$alfa_score >= 11.763,][,c(1,4:6,12)]
sublist_1B <- df1_TFs[df1_TFs$alfa_score >= 18.726,][,c(1,4:6,12)]

#Sublistas 2 de lo contrario a lo anterior

sublist_2 <- df1_TFs[df1_TFs$alfa_score < 14.185,][,c(1,4:6,12)]
sublist_2A <- df1_TFs[df1_TFs$alfa_score < 11.763,][,c(1,4:6,12)]
sublist_2B <- df1_TFs[df1_TFs$alfa_score < 18.726,][,c(1,4:6,12)]

#Guardar archivo de IDRs al 99% (sublist_1B)

write.table(x = sublist_1B[,3], 
            file = "DATA/DATA FOR R/DATA FOR ANALYSIS/IDRs_99p", 
            row.names = FALSE, quote = FALSE)
