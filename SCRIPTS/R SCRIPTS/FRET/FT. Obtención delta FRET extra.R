#Paqueterías----

library(ggplot2)
library(ggpubr)
library(ggrepel)

#Cargar archivo a analizar----
df_extra_bios <- readxl::read_xlsx(path = "home/cesar/Desktop/ALL FILES/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/df_extra_bios.xlsx", 
                                   sheet = 1, col_names = TRUE)

#Obtener el cociente FRET para cada constructo
#Generar un dataframe vacio para  colocar los datos
#del delta fret

df_fret_extra <- data.frame(matrix(nrow = 25,
                             ncol = 4))

names(df_fret_extra)[1] <- "Constructo"
names(df_fret_extra)[2] <- "Delta_FRET_promedio"
names(df_fret_extra)[3] <- "Longitud"
names(df_fret_extra)[4] <- "sd"

#Añadir constructos al dataframe creado
a <- paste0("00",as.character(names(table(df_extra_bios$IDRBS)))[1:4])
b <- paste0("0",as.character(names(table(df_extra_bios$IDRBS)))[5:21])
c <- as.character(names(table(df_extra_bios$IDRBS)))[22:25]

df_fret_extra$Constructo <- c(a,b,c)


temp <- c() #vector vacío
for (a in as.numeric(names(table(df_extra_bios$IDRBS)))) {
 
  i <- df_extra_bios[df_extra_bios$IDRBS == a & 
                       df_extra_bios$NaCl == 0,]$`DxAm/DxDm`
  h <- df_extra_bios[df_extra_bios$IDRBS == a & 
                       df_extra_bios$NaCl == 1.5,]$`DxAm/DxDm`
  #Realizar delta FRET promedio
  temp[length(temp) + 1] <- mean(h-i)

}

#Añadir columna con delta FRET promedio al dataframe
df_fret_extra$Delta_FRET_promedio <- temp

#Añadir columna con longitudes de cada constructo
temp <- c(200,166,71,100,200,153,148,100,200,105,
          100,150,150,100,185,50,77,50,50,68,59,130,
          121,164,204)

df_fret_extra$Longitud <- temp

#Añadir columna de desviación estándar
temp <- c() #vector vacío
for (a in (names(table(df_extra_bios$IDRBS)))) {
  
  i <- df_extra_bios[df_extra_bios$IDRBS == a & 
                       df_extra_bios$NaCl == 0,]$`DxAm/DxDm`
  h <- df_extra_bios[df_extra_bios$IDRBS == a & 
                       df_extra_bios$NaCl == 1.5,]$`DxAm/DxDm`
  #Realizar delta FRET promedio
  temp[length(temp) + 1] <- sd(h-i)
  
}

#Añadir columna con delta FRET promedio al dataframe
df_fret_extra$sd <- temp

#Añadir una columna extra que contenga los valores
#promedio de FRET inicial (0M)

temp <- c() #vector vacío
for (a in (names(table(df_extra_bios$IDRBS)))) {
  
  i <- df_extra_bios[df_extra_bios$IDRBS == a & 
                       df_extra_bios$NaCl == 0,]$`DxAm/DxDm`
  #Realizar delta FRET promedio
  temp[length(temp) + 1] <- mean(i)
  
}

#Añadir columna de FRET inicial (0M)
df_fret_extra$FRET_inicial_0M <- temp



#Unir gráficos con los 32 constructos anteriores
#incluyendo los 25 constructos nuevos
df_all_fret_cons <- rbind(df_cons,df_fret_extra)

#Realizar un tratamiento final al dataframe
df_all_fret_cons <- df_all_fret_cons[-c(1,54,55,56,57,39,42),]

#Ordenar el dataframe
df_all_fret_cons <- df_all_fret_cons[order(as.numeric(df_all_fret_cons$Constructo)),]

#Agregar el constructo faltante 112 al dataframe

cons_112 <- read.table(file = "DATA/DATA FOR R/FRET/Data_FRET/112.txt", 
                       header = T, sep = ",", fileEncoding = "latin1")


#Eliminar los datos que no se requieren 
cons_112 <- cons_112[-c(1:12,25:36,49:60),]

#Realizar el delta FRET

i <- cons_112[cons_112$Tratamiento == 0,]$Normalización
h <- cons_112[cons_112$Tratamiento == 1500,]$Normalización

#Obtener los parámetros estadísticos y añadirlos al 
#dataframe

df_all_fret_cons[51,1] <- "112"
df_all_fret_cons[51,2] <- mean(h-i)
df_all_fret_cons[51,3] <- 50
df_all_fret_cons[51,4] <- sd(h-i)
df_all_fret_cons[51,5] <- mean(i)


#Añadir a SED1 proveniente de los datos de Daniela

df_dani <- readxl::read_xlsx(path = "DATA/DATA FOR R/FRET/Data_FRET/Gruposelectoproteinas LEA_CIDER_Script.xlsx", 
                             sheet = 1)

df_all_fret_cons[52,1] <- "SED1"
df_all_fret_cons[52,2] <- df_dani[3,]$Delta_FRET_promedio
df_all_fret_cons[52,3] <- df_dani[3,]$Longitud
df_all_fret_cons[52,4] <- df_dani[3,]$sd


#Gráficos de delta FRET----


p <- ggplot() +
  geom_vline(xintercept = unname(quantile(df_all_fret_cons$Delta_FRET_promedio)[2]), 
             lty = 2) +
  geom_vline(xintercept = median(df_all_fret_cons$Delta_FRET_promedio), 
             lty = 2, color = "blue") +
  geom_vline(xintercept = unname(quantile(df_all_fret_cons$Delta_FRET_promedio)[4]), 
             lty = 2) +
  geom_point(data = df_all_fret_cons, aes(x = Delta_FRET_promedio, 
                                       y = as.factor(Constructo)), 
             size = 4, shape = 21, 
             fill = "white", stroke = 1.5) +
  geom_errorbar(data = df_all_fret_cons, aes(x = Delta_FRET_promedio, 
                                          y = as.factor(Constructo), 
                                          xmin = Delta_FRET_promedio-sd, 
                                          xmax = Delta_FRET_promedio+sd)) +
  labs(x = expression(Delta * "FRET"), 
       y = "IDR") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(-1,2.5)) +
  scale_x_continuous(breaks = seq(-1,2.5,0.5),
                     expand = c(0,-0.4))

#Marcar de un color distintos a algunas IDRs

#190
p <- p + geom_point(data = df_all_fret_cons[df_all_fret_cons$Constructo == "190",], 
               aes(x = Delta_FRET_promedio, 
                   y = as.factor(Constructo)), 
               size = 4, shape = 21, 
               color = "red", stroke = 1.5) +
  geom_errorbar(data = df_all_fret_cons[df_all_fret_cons$Constructo == "190",], aes(x = Delta_FRET_promedio, 
                                             y = as.factor(Constructo), 
                                             xmin = Delta_FRET_promedio-sd, 
                                             xmax = Delta_FRET_promedio+sd), color = "red")
  
#163
p <- p + geom_point(data = df_all_fret_cons[df_all_fret_cons$Constructo == "163",], 
                    aes(x = Delta_FRET_promedio, 
                        y = as.factor(Constructo)), 
                    size = 4, shape = 21, 
                    color = "red", stroke = 1.5) +
  geom_errorbar(data = df_all_fret_cons[df_all_fret_cons$Constructo == "163",], aes(x = Delta_FRET_promedio, 
                                                                                    y = as.factor(Constructo), 
                                                                                    xmin = Delta_FRET_promedio-sd, 
                                                                                    xmax = Delta_FRET_promedio+sd), color = "red")

#026
p <- p + geom_point(data = df_all_fret_cons[df_all_fret_cons$Constructo == "026",], 
                    aes(x = Delta_FRET_promedio, 
                        y = as.factor(Constructo)), 
                    size = 4, shape = 21, 
                    color = "red", stroke = 1.5) +
  geom_errorbar(data = df_all_fret_cons[df_all_fret_cons$Constructo == "026",], aes(x = Delta_FRET_promedio, 
                                                                                    y = as.factor(Constructo), 
                                                                                    xmin = Delta_FRET_promedio-sd, 
                                                                                    xmax = Delta_FRET_promedio+sd), color = "red")

#127
p <- p + geom_point(data = df_all_fret_cons[df_all_fret_cons$Constructo == "127",], 
                    aes(x = Delta_FRET_promedio, 
                        y = as.factor(Constructo)), 
                    size = 4, shape = 21, 
                    color = "red", stroke = 1.5) +
  geom_errorbar(data = df_all_fret_cons[df_all_fret_cons$Constructo == "127",], aes(x = Delta_FRET_promedio, 
                                                                                    y = as.factor(Constructo), 
                                                                                    xmin = Delta_FRET_promedio-sd, 
                                                                                    xmax = Delta_FRET_promedio+sd), color = "red")


#043
p <- p + geom_point(data = df_all_fret_cons[df_all_fret_cons$Constructo == "043",], 
                    aes(x = Delta_FRET_promedio, 
                        y = as.factor(Constructo)), 
                    size = 4, shape = 21, 
                    color = "red", stroke = 1.5) +
  geom_errorbar(data = df_all_fret_cons[df_all_fret_cons$Constructo == "043",], aes(x = Delta_FRET_promedio, 
                                                                                    y = as.factor(Constructo), 
                                                                                    xmin = Delta_FRET_promedio-sd, 
                                                                                    xmax = Delta_FRET_promedio+sd), color = "red")

#SED1
p <- p + geom_point(data = df_all_fret_cons[df_all_fret_cons$Constructo == "SED1",], 
                    aes(x = Delta_FRET_promedio, 
                        y = as.factor(Constructo)), 
                    size = 4, shape = 21, 
                    color = "red", stroke = 1.5) +
  geom_errorbar(data = df_all_fret_cons[df_all_fret_cons$Constructo == "SED1",], aes(x = Delta_FRET_promedio, 
                                                                                    y = as.factor(Constructo), 
                                                                                    xmin = Delta_FRET_promedio-sd, 
                                                                                    xmax = Delta_FRET_promedio+sd), color = "red")

#Guardar gráfico
ggsave(plot = p, filename = "PLOTS/delta_FRET_all_quartil_red.pdf", 
       device = "pdf", height = 10, width = 5, units = "in", dpi = 600)

#Guardar gráfico
ggsave(plot = p, filename = "PLOTS/delta_FRET_all_quartil_black.pdf", 
       device = "pdf", height = 10, width = 5, units = "in", dpi = 600)

#Guardar archivo 
write.csv(x = df_all_fret_cons, 
          file = "DATA/DATA FOR R/DATA FOR ANALYSIS/delta_FRET_data_all_biosensors.csv", 
          quote = F, row.names = F)


#Gráficos de correlaciones----

#Gráfico de delta FRET vs Longitud
temp <- signif(cor(df_all_fret_cons$Delta_FRET_promedio,
                  df_all_fret_cons$Longitud, 
                  method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_all_fret_cons$Delta_FRET_promedio, 
                df_all_fret_cons$Longitud, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_all_fret_cons, aes(x = as.numeric(Longitud), 
                                           y = Delta_FRET_promedio)) +
  geom_smooth(data = df_all_fret_cons, aes(x = as.numeric(Longitud), 
                                           y = Delta_FRET_promedio),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Longitud", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_all_fret_cons$Longitud),
                           max(df_all_fret_cons$Longitud)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 62, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 62, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)
  
#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_length.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)


#Cargar archivo con las demás correlaciones

df_corr_FRET <- read.csv(file = "DATA/DATA FOR R/FRET/Data_FRET/Correlaciones_CIDER.csv", 
                    header = T, sep = ",")

#Modificar valores del constructo 33

df_corr_FRET[30,5] <- 0.201
df_corr_FRET[30,6] <- 0.307
df_corr_FRET[30,7] <- -0.027
df_corr_FRET[30,8] <- 3.724
df_corr_FRET[30,9] <- 0.767
df_corr_FRET[30,10] <- 2
df_corr_FRET[30,11] <- 0.027

#Gráfico de delta FRET vs kappa
temp <- signif(cor(df_corr_FRET$Delta_FRET_promedio,
                   df_corr_FRET$X_,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_corr_FRET$Delta_FRET_promedio,
                       df_corr_FRET$X_, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_corr_FRET, aes(x = X_, 
                                           y = Delta_FRET_promedio)) +
  geom_smooth(data = df_corr_FRET, aes(x = X_, 
                                           y = Delta_FRET_promedio),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = expression(kappa), 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_corr_FRET$X_),
                           max(df_corr_FRET$X_)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 0.090, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.094, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_kappa.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)



#Gráfico de delta FRET vs FCR
temp <- signif(cor(df_corr_FRET$Delta_FRET_promedio,
                   df_corr_FRET$FCR,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_corr_FRET$Delta_FRET_promedio,
                       df_corr_FRET$FCR, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_corr_FRET, aes(x = FCR, 
                                       y = Delta_FRET_promedio)) +
  geom_smooth(data = df_corr_FRET, aes(x = FCR, 
                                       y = Delta_FRET_promedio),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "FCR", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_corr_FRET$FCR),
                           max(df_corr_FRET$FCR)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 0.075, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.070, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_FCR.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)



#Gráfico de delta FRET vs NCPR
temp <- signif(cor(df_corr_FRET$Delta_FRET_promedio,
                   df_corr_FRET$NCPR,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_corr_FRET$Delta_FRET_promedio,
                       df_corr_FRET$NCPR, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_jitter(data = df_corr_FRET, aes(x = NCPR, 
                                       y = Delta_FRET_promedio)) +
  labs(x = "NCPR", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(-0.4,0.4), 
                  ylim = c(0,2))

#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_NCPR.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)



#Gráfico de delta FRET vs hidropaía
temp <- signif(cor(df_corr_FRET$Delta_FRET_promedio,
                   df_corr_FRET$Hydropathy,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_corr_FRET$Delta_FRET_promedio,
                       df_corr_FRET$Hydropathy, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_corr_FRET, aes(x = Hydropathy, 
                                       y = Delta_FRET_promedio)) +
  geom_smooth(data = df_corr_FRET, aes(x = Hydropathy, 
                                       y = Delta_FRET_promedio),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Hidropatía", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_corr_FRET$Hydropathy),
                           max(df_corr_FRET$Hydropathy)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 2.78, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 2.74, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_Hydropathy.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)



#Gráfico de delta FRET vs desorden promovido
temp <- signif(cor(df_corr_FRET$Delta_FRET_promedio,
                   df_corr_FRET$Disorder.promoting,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_corr_FRET$Delta_FRET_promedio,
                       df_corr_FRET$Disorder.promoting, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_corr_FRET, aes(x = Disorder.promoting, 
                                       y = Delta_FRET_promedio)) +
  geom_smooth(data = df_corr_FRET, aes(x = Disorder.promoting, 
                                       y = Delta_FRET_promedio),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Fracción de desorden promovido", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_corr_FRET$Disorder.promoting),
                           max(df_corr_FRET$Disorder.promoting)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 0.68, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.668, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_disorder_promoting.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)


#Gráfico de puntos con barras de error (DANI)----

#Ordenar los datos
df_dani <- df_dani[c(1,3,2,4),]
ggplot() +
  geom_vline(xintercept = unname(quantile(df_dani$Delta_FRET_promedio)[2]), 
             lty = 2) +
  geom_vline(xintercept = median(df_dani$Delta_FRET_promedio), 
             lty = 2, color = "blue") +
  geom_vline(xintercept = unname(quantile(df_dani$Delta_FRET_promedio)[4]), 
             lty = 2) +
  geom_point(data = df_dani, aes(x = Delta_FRET_promedio, 
                                          y = as.factor(Constructo)), 
             size = 4, shape = 21, 
             fill = "white", stroke = 1.5) +
  geom_errorbar(data = df_dani, aes(x = Delta_FRET_promedio, 
                                             y = as.factor(Constructo), 
                                             xmin = Delta_FRET_promedio-sd, 
                                             xmax = Delta_FRET_promedio+sd)) +
  labs(x = expression(Delta * "FRET"), 
       y = "IDR") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(0,1.5)) +
  scale_x_continuous(breaks = seq(0,1.5,0.5),
                     expand = c(0,-0.4)) + 
  scale_y_discrete(limits = c("PvLEA18", "LEA_4 At2g", 
                              "LEA_1", "LEA_5"), 
                   labels = c("PVLEA18", "LEA_4", 
                              "LEA_1", "LEA_5"))


#Guardar gráfico
ggsave(filename = "PLOTS/delta_FRET_LEAs.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)




#Gráfico de delta FRET vs MNC
temp <- signif(cor(df_corr_FRET$Delta_FRET_promedio,
                   df_corr_FRET$MNC,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_corr_FRET$Delta_FRET_promedio,
                         df_corr_FRET$MNC, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_corr_FRET, aes(x = MNC, 
                                       y = Delta_FRET_promedio)) +
  geom_smooth(data = df_corr_FRET, aes(x = MNC, 
                                       y = Delta_FRET_promedio),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Mean Net Charge", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_corr_FRET$MNC),
                           max(df_corr_FRET$MNC)), 
                  ylim = c(-0.5,2.5)) +
  annotate(geom = "text", x = 0.035, y = 2.4, 
       
           
               label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.04, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_MNC.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)


#Gráficos de correlación entre parámetros

#Gráfico de delta FRET vs Longitud
temp <- signif(cor(df_corr_FRET$X_,
                   df_corr_FRET$MNC, 
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_corr_FRET$X_,
                         df_corr_FRET$MNC, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_corr_FRET, aes(x = as.numeric(), 
                                           y = Delta_FRET_promedio)) +
  geom_smooth(data = df_all_fret_cons, aes(x = as.numeric(Longitud), 
                                           y = Delta_FRET_promedio),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Longitud", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_all_fret_cons$Longitud),
                           max(df_all_fret_cons$Longitud)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 62, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 62, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

#Guardar gráfico
ggsave(filename = "PLOTS/FRET_vs_length.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)


#Cargar archivo con las demás correlaciones

df_corr_FRET <- read.csv(file = "DATA/DATA FOR R/FRET/Data_FRET/Correlaciones_CIDER.csv", 
                         header = T, sep = ",")








#Gráfico de puntos con barras de error (scrambles)----

#Cargar archivo
SED1_scrambles <- readxl::read_xlsx(path = "DATA/DATA FOR R/FRET/Data_FRET/SED1-Scrambles.xlsx")


p <- ggplot() +
  geom_vline(xintercept = unname(quantile(df_all_fret_cons$Delta_FRET_promedio)[2]), 
             lty = 2) +
  geom_vline(xintercept = median(df_all_fret_cons$Delta_FRET_promedio), 
             lty = 2, color = "blue") +
  geom_vline(xintercept = unname(quantile(df_all_fret_cons$Delta_FRET_promedio)[4]), 
             lty = 2) +
  geom_point(data = SED1_scrambles, aes(x = Delta_FRET, 
                                 y = as.factor(Construct)), 
             size = 4, shape = 21, 
             fill = "white", stroke = 1.5) +
  geom_errorbar(data = SED1_scrambles, aes(x = Delta_FRET, 
                                    y = as.factor(Construct), 
                                    xmin = Delta_FRET-SD, 
                                    xmax = Delta_FRET+SD)) +
  labs(x = expression(Delta * "FRET"), 
       y = "IDR") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank())  + 
  coord_cartesian(xlim = c(-0.5,2)) +
  scale_y_discrete(limits = c("Scramble5", "Scramble4","Scramble3", 
                              "Scramble2", "Scramble1", "SED1")) 
  

p <- p + geom_point(data = SED1_scrambles[SED1_scrambles$Construct == "SED1",], 
               aes(x = Delta_FRET, 
                   y = as.factor(Construct)), 
               size = 4, shape = 21, 
               color = "red", stroke = 1.5) +
  geom_errorbar(data = SED1_scrambles[SED1_scrambles$Construct == "SED1",], aes(x = Delta_FRET, 
                                                                                    y = as.factor(Construct), 
                                                                                    xmin = Delta_FRET-SD, 
                                                                                    xmax = Delta_FRET+SD),color = "red")

#Guardar gráfico
ggsave(plot = p, filename = "PLOTS/delta_FRET_scrambles_black.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)


#Guardar gráfico
ggsave(plot = p, filename = "PLOTS/delta_FRET_scrambles_red.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)



#Delta FRET vs Kappa SED1 y scrambles----

SED1_scrambles <- readxl::read_xlsx(path = "DATA/DATA FOR R/FRET/Data_FRET/SED1-Scrambles.xlsx")

#Realizar gráfico
p <- ggplot() +
  geom_point(data = SED1_scrambles, aes(y = Delta_FRET, 
                                        x = (Kappa)), 
             size = 4, shape = 21, 
             fill = "white", stroke = 1.5) +
  geom_errorbar(data = SED1_scrambles, aes(y = Delta_FRET, 
                                           x = (Kappa), 
                                           ymin = Delta_FRET-SD, 
                                           ymax = Delta_FRET+SD),
                width = 0.008) +
  labs(y = expression(Delta * "FRET"), 
       x = expression(kappa)) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(0.05,0.25))

p <- p + geom_point(data = SED1_scrambles[SED1_scrambles$Construct == "SED1",], aes(y = Delta_FRET, 
                                          x = Kappa), 
               size = 4, shape = 21, 
               fill = "white", stroke = 1.5, color = "red") +
  geom_errorbar(data = SED1_scrambles[SED1_scrambles$Construct == "SED1",], 
                aes(y = Delta_FRET,
                    x = Kappa,
                    ymin = Delta_FRET-SD,
                    ymax = Delta_FRET+SD), 
                width = 0.008, color = "red")


#Guardar gráfico
ggsave(plot = p, filename = "PLOTS/delta_FRET_scrambles_Kappa_black.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)

#Guardar gráfico
ggsave(plot = p, filename = "PLOTS/delta_FRET_scrambles_Kappa_red.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)


#Gráfico de delta FRET vs FRET inicial----
#Gráfico de delta FRET vs MNC
temp <- signif(cor(df_all_fret_cons$Delta_FRET_promedio,
                   df_all_fret_cons$FRET_inicial_0M,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_all_fret_cons$Delta_FRET_promedio,
                         df_all_fret_cons$FRET_inicial_0M, 
                         method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_all_fret_cons, aes(x = FRET_inicial_0M, 
                                       y = Delta_FRET_promedio)) +
  #geom_smooth(data = df_all_fret_cons, aes(x = FRET_inicial_0M, 
                     #                  y = Delta_FRET_promedio),
              #method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "FRET (0 M)", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(0.5,2.2), 
                  ylim = c(-0.5,2.5)) +
  #annotate(geom = "text", x = 0.6, y = 2.4, 
    #       label = paste0("r = ", temp), size = 4.5) +
  #annotate(geom = "text", x = 0.6, y = 2.15,
     #      label = paste0("p = ", temp1), size = 4.5) +
  geom_label_repel(data = df_all_fret_cons, 
                   aes(x = FRET_inicial_0M, 
                       y = Delta_FRET_promedio,
                       label = Constructo), 
                   max.overlaps = 50)

#Guardar gráfico
ggsave(filename = "PLOTS/delta_FRET_vs_FRET_inicial.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)

#Guardar gráfico
ggsave(filename = "PLOTS/delta_FRET_vs_FRET_inicial_names.pdf", device = "pdf", 
       width = 10, height = 6, units = "in", dpi = 600)




