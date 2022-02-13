
###----Requisitos----
library(ggplot2)

###----Funciones requeridas----
bins <- function(x) {
  if ((2 * IQR(x, na.rm = TRUE) / length(x)^(1/3)) > 0.05) {
    ceiling(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3))
  } else {
    round(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3), digits = 2)
  }
}


###----Gráficos de proteoma vs. FTs----


###----Frecuencia vs. Porcentaje de desorden----

###Determinación del ancho de banda----
binp <- bins(as.numeric(df1_TFs$Porcentaje_desordenado))


#Gráfico del proteoma
ggplot() + 
  geom_histogram(data = subset(df_proteome, !ATG %in% c(df1_TFs$ATG_TF)),
                 aes(y = ..ncount.., x = as.numeric(Porcentaje_desordenado), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = binp, boundary = 0) +
#Agregar gráfico de los Factores de transcripción
geom_histogram(data = subset(df_proteome, ATG %in% c(df1_TFs$ATG_TF)), 
                        aes(y = ..ncount..,x = as.numeric(Porcentaje_desordenado), 
                            fill = "2"), alpha = 0.5, color = "black", 
                        binwidth = binp, boundary = 0) +
#Agregar modificaciones finales
labs(x = "Porcentaje de desorden", 
               y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(color = "black"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,100, binp)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs"))

##Determinación de la mediana de los grupos

#Mediana del proteoma
median(as.numeric(df1$Porcentaje_desordenado), na.rm = TRUE) #27.09

#Mediana de factores de transcripción
median(as.numeric(df1_TFs$Porcentaje_desordenado), na.rm = TRUE) #43.4


ggsave(filename = "desordenado.png", device = "png", width = 10, 
       height = 6, units = "in", dpi = 450)

###----Frecuencia vs. longitud (aa)----

##Determinación del ancho de banda
binl <- bins(df1_TFs$longitud)


#Gráfico del proteoma
a2 <- ggplot() + 
  geom_histogram(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)),  
                 aes(y = ..ncount..,x = as.numeric(longitud), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = binl, boundary = 0) +
#Agregar gráfico de los Factores de transcripción
geom_histogram(data = df1_TFs, 
               aes(y = ..ncount.., x = as.numeric(longitud), 
                   fill = "2"), alpha = 0.3, color = "black", 
               binwidth = binl, boundary = 0) +
#Agregar modificaciones finales
labs(x = "Tamaño del FT (aa)", 
               y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,2000, binl*3)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs")) +
  coord_cartesian(xlim = c(0,2000))
 

##Determinación de la mediana de los grupos

#Mediana del proteoma
median(as.numeric(df1$longitud), na.rm = TRUE) #348

#Mediana de factores de transcripción
median(as.numeric(df1_TFs$longitud), na.rm = TRUE) #313




###----Frecuencia vs. Fracción desordenada más larga----

#Determinación del ancho de banda
binf <- bins((df1_TFs$Mayor_segmento_desordenado)/(as.numeric(df1_TFs$longitud)))

#Gráfico del proteoma
ggplot() + 
  geom_histogram(data = subset(df_proteome, !ATG %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount..,x = (Mayor_segmento_desordenado)/(as.numeric(longitud)), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = binf, boundary = 0) +
  #Agregar gráfico de los Factores de transcripción
  geom_histogram(data = subset(df_proteome, ATG %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount.., x = (Mayor_segmento_desordenado)/(as.numeric(longitud)), 
                     fill = "2"), alpha = 0.3, color = "black",
                 binwidth = binf, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Fracción de la región desordenada más larga", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(color = "black"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,1, binf*3)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs"))


##Determinación de la mediana de los grupos

#Mediana del proteoma
median((df1$Mayor_segmento_desordenado)/(as.numeric(df1$longitud)), na.rm = TRUE) #0.09

#Mediana de factores de transcripción
median((df1_TFs$Mayor_segmento_desordenado)/(as.numeric(df1_TFs$longitud)), na.rm = TRUE) #0.15


ggsave(filename = "fracción.png", device = "png", width = 10, 
       height = 6, units = "in", dpi = 450)



###----Frecuencia vs. Número de IDRs----

#Determinación del ancho de banda
bint <- bins(as.numeric(df1_TFs$total_disordered_segments))

#Gráfico del proteoma
a4 <- ggplot() + 
  geom_histogram(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount..,x = as.numeric(total_disordered_segments), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = bint, boundary = 0) +
  #Agregar gráfico de los Factores de transcripción
  geom_histogram(data = subset(df_proteome, ATG %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount.., x = as.numeric(total_disordered_segments), 
                     fill = "2"), alpha = 0.3, color = "black",
                 binwidth = bint, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Número de regiones desordenadas", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,30,bint)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs")) + 
  coord_cartesian(xlim = c(0,30))

##Determinación de la mediana de los grupos

#Mediana del proteoma
median(as.numeric(df1$total_disordered_segments), na.rm = TRUE) #6

#Mediana de factores de transcripción
median(as.numeric(df1_TFs$total_disordered_segments), na.rm = TRUE) #6



###----Frecuencia vs. Número de residuos de IDRs----

#Determinación del ancho de banda
binm <- bins(as.numeric(df1_TFs$Mayor_segmento_desordenado))

#Gráfico del proteoma
ggplot() + 
  geom_histogram(data = subset(df_proteome, !ATG %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount..,x = as.numeric(Mayor_segmento_desordenado), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = binm, boundary = 0) +
  #Agregar gráfico de los Factores de transcripción
  geom_histogram(data = subset(df_proteome, ATG %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount.., x = as.numeric(Mayor_segmento_desordenado), 
                     fill = "2"), alpha = 0.3, color = "black", 
                 binwidth = binm, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Número de residuos de la región desordenada más larga (aa)", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(color = "black"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,350,binm*3)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs")) +
  coord_cartesian(xlim = c(0,350))


##Determinación de la mediana de los grupos

#Mediana del proteoma
median(as.numeric(df1$Mayor_segmento_desordenado), na.rm = TRUE) #32

#Mediana de factores de transcripción
median(as.numeric(df1_TFs$Mayor_segmento_desordenado), na.rm = TRUE) #49


ggsave(filename = "segmento_mayor.png", device = "png", width = 10, 
       height = 6, units = "in", dpi = 450)




###----Propuestas de gráficas de Rocío----


###----Frecuencia vs. Porcentaje de desorden----

ggplot() +
geom_histogram(data = df1_TFs, 
               aes(y = ..ncount..,x = as.numeric(Porcentaje_desordenado)), 
               alpha = 0.4, color = "black", fill = "steelblue4",
               binwidth = binp, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Porcentaje de desorden", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,100, binp))

###----Frecuencia vs. longitud (aa)----

ggplot() +
geom_histogram(data = df1_TFs, 
               aes(y = ..ncount.., x = as.numeric(longitud)), 
               alpha = 0.5, color = "black", fill = "steelblue4",
               binwidth = binl, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Tamaño del FT (aa)", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0,0), breaks = seq(87,2000, binl*3)) + 
  coord_cartesian(xlim = c(0,1600))



###----Frecuencia vs. Fracción desordenada más larga----

ggplot() +
geom_histogram(data = df1_TFs, 
               aes(y = ..ncount.., x = (Mayor_segmento_desordenado)/(as.numeric(longitud))), 
                   alpha = 0.5, color = "black", fill = "steelblue4",
               binwidth = binf, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Fracción de la región desordenada más larga", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,1, binf*3)) +
  coord_cartesian(xlim = c(0,1))



###----Frecuencia vs. Número de IDRs----

ggplot() + 
geom_histogram(data = df1_TFs, 
               aes(y = ..ncount.., x = as.numeric(total_disordered_segments)), 
               alpha = 0.5, color = "black", fill = "steelblue4",
               binwidth = bint, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Número de regiones desordenadas", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,30,bint)) + 
  coord_cartesian(xlim = c(0,30))



###----Frecuencia vs. Número de residuos de IDRs----

ggplot() +
geom_histogram(data = df1_TFs, 
               aes(y = ..ncount.., x = as.numeric(Mayor_segmento_desordenado)), 
               alpha = 0.5, color = "black", fill = "steelblue4",
               binwidth = binm, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Número de residuos de la región desordenada más larga (aa)", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,350,binm*3)) +
  coord_cartesian(xlim = c(0,350))




#guardar gráficos
ggsave(filename = "IDRs.png", device = "png", units = "in",
      width = 9, height = 5, dpi = 350)





### Obtención de densidades ----------

###----Frecuencia vs. Porcentaje de desorden----

#Gráfico del proteoma
a1 <- ggplot() +  
  geom_density(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
               aes(y = ..scaled..,x = as.numeric(Porcentaje_desordenado),
                   color = "Densidad: Proteoma"), size = 1, alpha = 0.6) +
  geom_density(data = df1_TFs, 
               aes(y = ..scaled..,x = as.numeric(Porcentaje_desordenado),
                   color = "Densidad: FTs"), size = 1, alpha = 0.6) + 
  geom_vline(xintercept = median(as.numeric((subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)))$Porcentaje_desordenado), 
                                 na.rm = TRUE),
             color = "gray", lty = "dashed", size = 1.3) + 
  geom_vline(xintercept = median(as.numeric(df1_TFs$Porcentaje_desordenado), na.rm = TRUE),
             color = "steelblue4", lty = "dashed", size = 1.3) +
  #Agregar modificaciones finales
  labs(x = "Porcentaje de desorden", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,100, binp*3)) +
  scale_color_manual(name = "Líneas de\ndensidad", 
                     values = c("steelblue4", "gray"))


###----Frecuencia vs. longitud (aa)----

#Gráfico del proteoma
a2 <- ggplot() +  
  geom_density(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
               aes(y = ..scaled..,x = as.numeric(longitud),
                   color = "Densidad: Proteoma"), size = 1, alpha = 0.6) +
  geom_density(data = df1_TFs, 
               aes(y = ..scaled..,x = as.numeric(longitud),
                   color = "Densidad: FTs"), size = 1, alpha = 0.6) + 
  geom_vline(xintercept = median(as.numeric((subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)))$longitud), 
                                 na.rm = TRUE),
             color = "gray", lty = "dashed", size = 1.3) + 
  geom_vline(xintercept = median(as.numeric(df1_TFs$longitud), na.rm = TRUE),
             color = "steelblue4", lty = "dashed", size = 1.3) +
  #Agregar modificaciones finales
  labs(x = "Tamaño del FT (aa)", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,2000, binl*6)) +
  coord_cartesian(xlim = c(0,2000)) +
  scale_color_manual(name = "Líneas de\ndensidad", 
                     values = c("steelblue4", "gray"))



###----Frecuencia vs. Fracción desordenada más larga----

#Gráfico del proteoma
a3 <- ggplot() + 
  geom_density(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..scaled..,x = (Mayor_segmento_desordenado)/(as.numeric(longitud)),
                     color = "Densidad: Proteoma"), size = 1, alpha = 0.6) +
  #Agregar gráfico de los Factores de transcripción
  geom_density(data = df1_TFs, 
                 aes(y = ..scaled.., x = (Mayor_segmento_desordenado)/(as.numeric(longitud)), 
                     color = "Densidad: FTs"), size = 1, alpha = 0.6) +
  geom_vline(xintercept = median((df1$Mayor_segmento_desordenado)/(as.numeric(df1$longitud)), na.rm = TRUE),
             color = "gray", lty = "dashed", size = 1.3) + 
  geom_vline(xintercept = median((df1_TFs$Mayor_segmento_desordenado)/(as.numeric(df1_TFs$longitud)), na.rm = TRUE),
             color = "steelblue4", lty = "dashed", size = 1.3) +
  #Agregar modificaciones finales
  labs(x = "Fracción de la región desordenada más larga", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,1, binf*3)) + 
  scale_color_manual(name = "Líneas de\ndensidad", 
                     values = c("steelblue4", "gray"))



###----Frecuencia vs. Número de IDRs----

#Gráfico del proteoma
a4 <- ggplot() + 
  geom_density(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..scaled..,x = as.numeric(total_disordered_segments), 
                     color = "Densidad: Proteoma"), size = 1, alpha = 0.6) +
  #Agregar gráfico de los Factores de transcripción
  geom_density(data = df1_TFs, 
                 aes(y = ..scaled.., x = as.numeric(total_disordered_segments), 
                     color = "Densidad: FTs"), size = 1, alpha = 0.6) +
  geom_vline(xintercept = median(as.numeric((subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)))$total_disordered_segments), 
                                 na.rm = TRUE),
             color = "gray", lty = "dashed", size = 1.3) + 
  geom_vline(xintercept = median(as.numeric(df1_TFs$total_disordered_segments), na.rm = TRUE),
             color = "steelblue4", lty = "dashed", size = 1.3) +
  #Agregar modificaciones finales
  labs(x = "Número de regiones desordenadas", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,30,bint)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs")) + 
  coord_cartesian(xlim = c(0,30)) +
  scale_color_manual(name = "Líneas de\ndensidad", 
                     values = c("steelblue4", "gray"))




###----Frecuencia vs. Número de residuos de IDRs----

#Gráfico del proteoma
a5 <- ggplot() + 
  geom_density(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..scaled..,x = as.numeric(Mayor_segmento_desordenado), 
                     color = "Densidad: Proteoma"), size = 1, alpha = 0.6) +
  #Agregar gráfico de los Factores de transcripción
  geom_density(data = df1_TFs, 
                 aes(y = ..scaled.., x = as.numeric(Mayor_segmento_desordenado), 
                     color = "Densidad: FTs"), size = 1, alpha = 0.6) +
  geom_vline(xintercept = median(as.numeric((subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)))$Mayor_segmento_desordenado), 
                                 na.rm = TRUE),
             color = "gray", lty = "dashed", size = 1.3) + 
  geom_vline(xintercept = median(as.numeric(df1_TFs$Mayor_segmento_desordenado), na.rm = TRUE),
             color = "steelblue4", lty = "dashed", size = 1.3) +
  #Agregar modificaciones finales
  labs(x = "Número de residuos de la región desordenada más larga (aa)", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.85,0.85),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.05)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,350,binm*3)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs")) +
  coord_cartesian(xlim = c(0,350)) +
  scale_color_manual(name = "Líneas de\ndensidad", 
                     values = c("steelblue4", "gray"))






#----Figuras ordenadas

#Gráfico del proteoma
a1 <- ggplot() + 
  geom_histogram(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)),
                 aes(y = ..ncount.., x = as.numeric(Porcentaje_desordenado), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = binp, boundary = 0) +
  #Agregar gráfico de los Factores de transcripción
  geom_histogram(data = df1_TFs, 
                 aes(y = ..ncount..,x = as.numeric(Porcentaje_desordenado), 
                     fill = "2"), alpha = 0.4, color = "black", 
                 binwidth = binp, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Porcentaje de desorden", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(color = "black"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),) + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.5)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,100, binp*3)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs"))

##Determinación de la mediana de los grupos

#Mediana del proteoma
median(as.numeric(df1$Porcentaje_desordenado), na.rm = TRUE) #27.09

#Mediana de factores de transcripción
median(as.numeric(df1_TFs$Porcentaje_desordenado), na.rm = TRUE) #43.4




###----Frecuencia vs. longitud (aa)----

##Determinación del ancho de banda
binl <- bins(df1_TFs$longitud)


#Gráfico del proteoma
a2 <- ggplot() + 
  geom_histogram(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)),  
                 aes(y = ..ncount..,x = as.numeric(longitud), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = binl, boundary = 0) +
  #Agregar gráfico de los Factores de transcripción
  geom_histogram(data = df1_TFs, 
                 aes(y = ..ncount.., x = as.numeric(longitud), 
                     fill = "2"), alpha = 0.3, color = "black", 
                 binwidth = binl, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Tamaño del FT (aa)", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = "none") + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.5)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,2000, binl*8)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs")) +
  coord_cartesian(xlim = c(0,2000))


##Determinación de la mediana de los grupos

#Mediana del proteoma
median(as.numeric(df1$longitud), na.rm = TRUE) #348

#Mediana de factores de transcripción
median(as.numeric(df1_TFs$longitud), na.rm = TRUE) #313




###----Frecuencia vs. Fracción desordenada más larga----

#Determinación del ancho de banda
binf <- bins((df1_TFs$Mayor_segmento_desordenado)/(as.numeric(df1_TFs$longitud)))

#Gráfico del proteoma
a3 <- ggplot() + 
  geom_histogram(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount..,x = (Mayor_segmento_desordenado)/(as.numeric(longitud)), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = binf, boundary = 0) +
  #Agregar gráfico de los Factores de transcripción
  geom_histogram(data = df1_TFs, 
                 aes(y = ..ncount.., x = (Mayor_segmento_desordenado)/(as.numeric(longitud)), 
                     fill = "2"), alpha = 0.3, color = "black",
                 binwidth = binf, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Fracción de la región desordenada más larga", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = "none") + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.5)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,1, binf*8)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs"))


##Determinación de la mediana de los grupos

#Mediana del proteoma
median((df1$Mayor_segmento_desordenado)/(as.numeric(df1$longitud)), na.rm = TRUE) #0.09

#Mediana de factores de transcripción
median((df1_TFs$Mayor_segmento_desordenado)/(as.numeric(df1_TFs$longitud)), na.rm = TRUE) #0.15



###----Frecuencia vs. Número de IDRs----

#Determinación del ancho de banda
bint <- bins(as.numeric(df1_TFs$total_disordered_segments))

#Gráfico del proteoma
a4 <- ggplot() + 
  geom_histogram(data = subset(df1, !Identificador %in% c(df1_TFs$ATG_TF)), 
                 aes(y = ..ncount..,x = as.numeric(total_disordered_segments), 
                     fill = "1"), alpha = 0.6, color = "black", 
                 binwidth = bint, boundary = 0) +
  #Agregar gráfico de los Factores de transcripción
  geom_histogram(data = df1_TFs, 
                 aes(y = ..ncount.., x = as.numeric(total_disordered_segments), 
                     fill = "2"), alpha = 0.3, color = "black",
                 binwidth = bint, boundary = 0) +
  #Agregar modificaciones finales
  labs(x = "Número de regiones desordenadas", 
       y = "Frecuencia") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = "none") + 
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.5)) + 
  scale_x_continuous(expand = c(0.01,0), breaks = seq(0,30,bint*3)) + 
  scale_fill_manual(name = "Base de\ndatos", values = c("gray", "steelblue4"),
                    labels = c("Proteoma", "FTs")) + 
  coord_cartesian(xlim = c(0,30))

##Determinación de la mediana de los grupos

#Mediana del proteoma
median(as.numeric(df1$total_disordered_segments), na.rm = TRUE) #6

#Mediana de factores de transcripción
median(as.numeric(df1_TFs$total_disordered_segments), na.rm = TRUE) #6




library(gridExtra)

f1 <- grid.arrange("A" = a1)
f2 <- grid.arrange("B" = a4, "C" = a2, "D" = a3, 
                   nrow = 2, 
                   ncol = 2)

f3 <- grid.arrange(f1,f2, nrow = 1)


grid.arrange(
  grobs = 
)


library(ggpubr)

f1 <- ggarrange(a1, labels = c("A"))
f2 <- ggarrange(a4,a2,a3, labels = c("B", "C", "D"))
f3 <- ggarrange(f1, f2, widths = c(1.5,2))


ggsave(filename = "arrange.png", device = "png", units = "in",
       width = 10, height = 6, dpi = 350)




ggplot() + 
  geom_jitter(data = df1_TFs, aes(x = as.numeric(longitud), 
                                  y = as.numeric(Porcentaje_desordenado)), 
             pch = 1, color = "red")
