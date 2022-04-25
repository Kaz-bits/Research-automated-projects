#Paqueterías----

library(ggplot2)
library(ggpubr)

#Cargar archivos----

pLDDT_99p <- read.csv(file = "DATA/DATA FOR PYTHON/FASTA_META/METAPREDICT_RESULTS/IDRs_99p_pLDDT.txt", 
                         header = FALSE, sep = ",")

disorder_99p_M <- read.csv(file = "DATA/DATA FOR PYTHON/FASTA_META/METAPREDICT_RESULTS/IDRs_99p_disorder.txt",
                           header = FALSE, sep = ",")



#Creación de carpetas y directorios----

#Creación de carpetas para guardar los archivos generados
#Se debe especificar el directorio donde se encuentren los
#archivos por analizar

#Guardar el directorio de trabajo
main_dir <- "PLOTS/ALPHAFOLD/" #colocar directorio propio

#Buscar las carpetas requeridas en el directorio anterior
sub_dir <- "ALPHA_pLDDT" #primer carpeta requerida
dir.create(file.path(main_dir, sub_dir)) 

sub_dir <- "DESORDEN_(MvsP)" #segunda carpeta requerida
dir.create(file.path(main_dir, sub_dir))

#Generar los nombres de todas las IDRs analizadas
list_txt <- dir(path = "DATA/DATA FOR R/DATA FOR ANALYSIS/DUALIDAD_ALL/",
                pattern = "*.txt")

#Generación de gráficos de AlphaFold (pLDDT)----

for (a in 1:nrow(pLDDT_99p)) {
  
  #Variable con valores de pLDDT de cada IDR
  temp_df_cov <- c() #vector vacío
  temp_df_cov <- t(pLDDT_99p[a,c(3:144)])[!is.na(t(pLDDT_99p[a,c(3:144)])),] #147
  temp_df_cov <- data.frame("pLDDT" = temp_df_cov, "aa" = 1:length(names(temp_df_cov)))
  row.names(temp_df_cov) <- NULL
  
  #Obtención de gráfico
  p <- ggplot() +
    geom_line(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 0.4) +
    geom_point(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 1) +
    #pLDDT > 90
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = Inf, 
                  fill = "#000fff"), alpha = 1/8) +
    #90 > pLDDT > 70
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 90,
                  fill = "#00d4ff"), alpha = 1/8) +
    #50 > pLDDT < 70
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 70,
                  fill = "#dff102"), alpha = 1/6) +
    #pLDDT < 50
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
                  fill = "#f18102"), alpha = 1/6) +
    theme_bw() +
    labs(x = "Número de residuo", y = "pLDDT") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid = element_blank(),
          legend.position = "top",
          legend.key.size = unit(0.6, "cm")) +
    scale_fill_manual(name = NULL, 
                      values = c("#000fff", "#00d4ff", "#f7ff00", "#ff8700"),
                      labels = c("Very high (pLDDT > 90)", "Confident (90 > pLDDT > 70)",
                                 "Low (70 > pLDDT > 50)", "Very low (pLDDT < 50)")) +
    coord_cartesian(xlim = c(1,max(temp_df_cov$aa)), 
                    ylim = c(0,100)) +
    scale_y_continuous(breaks = seq(0,100,20))
  
  for (b in list_txt[a]) {
    
    #Guardar gráfico en carpeta creada
    temp_name <- paste0(substr(b, 1, 10), ".png")
    ggsave(plot = p, filename = paste0("PLOTS/ALPHAFOLD/ALPHA_pLDDT/", temp_name), 
           device = "png", width = 8, height = 5, units = "in", 
           dpi = 650)
  }
}


#Generación de gráficos de desorden (PONDR vs META)----
list_txt_P <- dir(path = "DATA/DATA FOR R/DUALIDAD/DUALIDAD_P/",
                  pattern = "*.txt")


for (a in 1:nrow(disorder_99p_M)) {
  
  #Cargar archivos de desonder en PONDR
  temp_file <- list_txt_P[a]
  temp_disor_P <- read.table(file = paste0("DATA/DATA FOR R/DUALIDAD/DUALIDAD_P/", temp_file), 
                             header = TRUE)
  names(temp_disor_P)[1] = "Num"
  names(temp_disor_P)[2] = "Res"
  names(temp_disor_P)[3] = "VLXT"
  
  #Modificar dataframe
  temp_df_disor_M <- disorder_99p_M[a,2:143][(!is.na(disorder_99p_M[a,2:143]))]
  temp_df_disor_M <- data.frame("disorder" = temp_df_disor_M, "aa" = 1:length(temp_df_disor_M))
  
  #Construcción del gráfico
  
  p <- ggplot() +
    #Metapredict
    geom_line(data = temp_df_disor_M, aes(x = aa, y = disorder,
                                          color = "metapredict"), size = 0.8) +
    geom_point(data = temp_df_disor_M, aes(x = aa, y = disorder,
                                           color = "metapredict"), size = 1.0) +
    #PONDR
    geom_line(data = temp_disor_P, aes(x = Num, y = VLXT,
                                       color = "PONDR"), size = 0.8) +
    geom_point(data = temp_disor_P, aes(x = Num, y = VLXT, 
                                        color = "PONDR"), size = 1.0) +
    #Umbral de desorder
    geom_hline(yintercept = 0.5, lty = 2) +
    #Modificaciones adicionales
    labs(x = "Número de residuo",
         y = "Desorden predicho") +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "top",
          panel.grid = element_blank()) +
    coord_cartesian(ylim = c(0,1), 
                    xlim = c(0,max(temp_df_disor_M$aa))) +
    scale_y_continuous(breaks = seq(0,1,0.25)) +
    scale_x_continuous(breaks = seq(0,max(temp_df_disor_M$aa), 10)) +
    scale_color_manual(name = NULL, 
                       values = c("red", "blue"),
                       labels = c("metapredict", "PONDR"))
  
  
  for (b in list_txt[a]) {
    
    #Guardar gráfico en carpeta creada
    temp_name <- paste0(substr(b, 1, 10), ".png")
    ggsave(plot = p, filename = paste0("PLOTS/ALPHAFOLD/DESORDEN_(MvsP)/", temp_name), 
           device = "png", width = 8, height = 5, units = "in", 
           dpi = 650)
    
  }
}



#Análisis de pLDDT (AlphaFold)----

#Al archivo cargado, agregarle la columna "factor_ID" del dataframe
#llamado "coverage" para identificar los biosensores que
#son requeridos

pLDDT_99p[,ncol(pLDDT_99p) + 1] <- coverage$factor_ID
pLDDT_99p <- pLDDT_99p[,c(144, 1:143)]
names(pLDDT_99p)[1] <- "factor_ID"

#Una vez limpiado el dataframe con los valore de pLDDT, filtrar
#con "temp_coverage" los 21 biosensores en función de la columna
#"factor_ID"

pLDDT_bios <- pLDDT_99p[pLDDT_99p$factor_ID %in% temp_coverage$factor_ID,]

#Agregar la columna de "Entry" al dataframe creado 

pLDDT_bios$Entry <- temp_coverage$Entry
pLDDT_bios <- pLDDT_bios[,c(145, 1:144)]


#Gŕaficos de pLDDT para biosensores----

#Graficar los valores de pLDDT de los biosensores de interés, los
#cuales son IDRBS-140, 144 y 081. Extraerlos del dataframe


#Gráfico para #081

temp_df_cov <- t(pLDDT_bios[5,c(4:145)])[!is.na(t(pLDDT_bios[5,c(4:145)])),] #081
temp_df_cov <- data.frame("pLDDT" = temp_df_cov, "aa" = 1:length(names(temp_df_cov)))
row.names(temp_df_cov) <- NULL

ggplot() +
  geom_line(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 0.4) +
  geom_point(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 1) +
  #pLDDT > 90
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = Inf, 
                fill = "#000fff"), alpha = 1/8) +
  #90 > pLDDT > 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 90,
                fill = "#00d4ff"), alpha = 1/8) +
  #50 > pLDDT < 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 70,
                fill = "#dff102"), alpha = 1/6) +
  #pLDDT < 50
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
                fill = "#f18102"), alpha = 1/6) +
  theme_bw() +
  labs(x = "Residuo de la IDR (aa)", y = "pLDDT") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_fill_manual(name = "Confiabilidad", 
                    values = c("#000fff", "#00d4ff", "#f7ff00", "#ff8700"),
                    labels = c("Very high (pLDDT > 90)", "Confident (90 > pLDDT > 70)",
                               "Low (70 > pLDDT > 50)", "Very low (pLDDT < 50)")) +
  coord_cartesian(xlim = c(1,max(temp_df_cov$aa)), 
                  ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,20))

#Guardar gráfico

ggsave(filename = "PLOTS/ALPHAFOLD/IDRBS-081_pLDDT.png", device = "png", 
       width = 8, height = 5, units = "in", dpi = 650)



#Gráfico para #140

temp_df_cov <- t(pLDDT_bios[11,c(4:145)])[!is.na(t(pLDDT_bios[11,c(4:145)])),] #140
temp_df_cov <- data.frame("pLDDT" = temp_df_cov, "aa" = 1:length(names(temp_df_cov)))
row.names(temp_df_cov) <- NULL

ggplot() +
  geom_line(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 0.4) +
  geom_point(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 1) +
  #pLDDT > 90
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = Inf, 
                fill = "#000fff"), alpha = 1/8) +
  #90 > pLDDT > 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 90,
                fill = "#00d4ff"), alpha = 1/8) +
  #50 > pLDDT < 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 70,
                fill = "#dff102"), alpha = 1/6) +
  #pLDDT < 50
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
                fill = "#f18102"), alpha = 1/6) +
  theme_bw() +
  labs(x = "Residuo de la IDR (aa)", y = "pLDDT") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_fill_manual(name = "Confiabilidad", 
                    values = c("#000fff", "#00d4ff", "#f7ff00", "#ff8700"),
                    labels = c("Very high (pLDDT > 90)", "Confident (90 > pLDDT > 70)",
                               "Low (70 > pLDDT > 50)", "Very low (pLDDT < 50)")) +
  coord_cartesian(xlim = c(1,max(temp_df_cov$aa)), 
                  ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,20))

#Guardar gráfico

ggsave(filename = "PLOTS/ALPHAFOLD/IDRBS-140_pLDDT.png", device = "png", 
       width = 8, height = 5, units = "in", dpi = 650)



#Gráfico para #144

temp_df_cov <- t(pLDDT_bios[13,c(4:145)])[!is.na(t(pLDDT_bios[13,c(4:145)])),] #144
temp_df_cov <- data.frame("pLDDT" = temp_df_cov, "aa" = 1:length(names(temp_df_cov)))
row.names(temp_df_cov) <- NULL

ggplot() +
  geom_line(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 0.4) +
  geom_point(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 1) +
  #pLDDT > 90
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = Inf, 
                fill = "#000fff"), alpha = 1/8) +
  #90 > pLDDT > 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 90,
                fill = "#00d4ff"), alpha = 1/8) +
  #50 > pLDDT < 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 70,
                fill = "#dff102"), alpha = 1/6) +
  #pLDDT < 50
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
                fill = "#f18102"), alpha = 1/6) +
  theme_bw() +
  labs(x = "Residuo de la IDR (aa)", y = "pLDDT") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_fill_manual(name = "Confiabilidad", 
                    values = c("#000fff", "#00d4ff", "#f7ff00", "#ff8700"),
                    labels = c("Very high (pLDDT > 90)", "Confident (90 > pLDDT > 70)",
                               "Low (70 > pLDDT > 50)", "Very low (pLDDT < 50)")) +
  coord_cartesian(xlim = c(1,max(temp_df_cov$aa)), 
                  ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,20))

#Guardar gráfico

ggsave(filename = "PLOTS/ALPHAFOLD/IDRBS-144_pLDDT.png", device = "png", 
       width = 8, height = 5, units = "in", dpi = 650)


#Porcentajes de confiabilidad por biosensor----

#Emplear el dataframe llamado "pLDDT_bios" y generar otro dataframe con los
#los porcentajes de residuos con valores de pLDDT de acuerdo a la escala
#de AlphaFold


temp_pLDDT <- c() #vector vacío
pLDDT_bios_P <- data.frame("Entry" = pLDDT_bios$Entry, 
                           "Very_high" = 1, "Confident" = 2, 
                           "Low" = 3, "Very_Low" = 4) #dataframe

for (a in 1:nrow(pLDDT_bios)) {
  
  temp_pLDDT <- (pLDDT_bios[a,-c(1,2,3)])[!is.na(pLDDT_bios[a,-c(1,2,3)])]
  
  #Generar condiciones de confiabilidad
  
  i <- (length(temp_pLDDT[temp_pLDDT > 90])/(length(temp_pLDDT)))*100 #Very high
  j <- (length(temp_pLDDT[temp_pLDDT > 70 & temp_pLDDT < 90])/(length(temp_pLDDT)))*100 #Confident
  k <- (length(temp_pLDDT[temp_pLDDT > 50 & temp_pLDDT < 70])/(length(temp_pLDDT)))*100 #Low
  l <- (length(temp_pLDDT[temp_pLDDT < 50])/(length(temp_pLDDT)))*100 #Very low
  
  #Agregar los valores en el dataframe según corresponda
  
  pLDDT_bios_P[a,2] <- i
  pLDDT_bios_P[a,3] <- j
  pLDDT_bios_P[a,4] <- k
  pLDDT_bios_P[a,5] <- l
  
}

#Con dicho análisis, realizar los gráficos para las IDRs-biosensores
#con el porcentaje más alto de pLDDT > 90, el más bajo y la mediana 
#de los valores

#Gráfico para #054

temp_df_cov <- t(pLDDT_bios[2,c(4:145)])[!is.na(t(pLDDT_bios[2,c(4:145)])),] #054
temp_df_cov <- data.frame("pLDDT" = temp_df_cov, "aa" = 1:length(names(temp_df_cov)))
row.names(temp_df_cov) <- NULL

ggplot() +
  geom_line(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 0.4) +
  geom_point(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 1) +
  #pLDDT > 90
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = Inf, 
                fill = "#000fff"), alpha = 1/8) +
  #90 > pLDDT > 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 90,
                fill = "#00d4ff"), alpha = 1/8) +
  #50 > pLDDT < 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 70,
                fill = "#dff102"), alpha = 1/6) +
  #pLDDT < 50
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
                fill = "#f18102"), alpha = 1/6) +
  theme_bw() +
  labs(x = "Número de residuo", y = "pLDDT") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_fill_manual(name = "Confiabilidad", 
                    values = c("#000fff", "#00d4ff", "#f7ff00", "#ff8700"),
                    labels = c("Very high (pLDDT > 90)", "Confident (90 > pLDDT > 70)",
                               "Low (70 > pLDDT > 50)", "Very low (pLDDT < 50)")) +
  coord_cartesian(xlim = c(1,max(temp_df_cov$aa)), 
                  ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,20))

#Guardar gráfico

ggsave(filename = "PLOTS/ALPHAFOLD/IDRBS-054_pLDDT.png", device = "png", 
       width = 8, height = 5, units = "in", dpi = 650)



#Gráfico para #064

temp_df_cov <- t(pLDDT_bios[3,c(4:145)])[!is.na(t(pLDDT_bios[3,c(4:145)])),] #064
temp_df_cov <- data.frame("pLDDT" = temp_df_cov, "aa" = 1:length(names(temp_df_cov)))
row.names(temp_df_cov) <- NULL

ggplot() +
  geom_line(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 0.4) +
  geom_point(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 1) +
  #pLDDT > 90
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = Inf, 
                fill = "#000fff"), alpha = 1/8) +
  #90 > pLDDT > 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 90,
                fill = "#00d4ff"), alpha = 1/8) +
  #50 > pLDDT < 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 70,
                fill = "#dff102"), alpha = 1/6) +
  #pLDDT < 50
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
                fill = "#f18102"), alpha = 1/6) +
  theme_bw() +
  labs(x = "Número de residuo", y = "pLDDT") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_fill_manual(name = "Confiabilidad", 
                    values = c("#000fff", "#00d4ff", "#f7ff00", "#ff8700"),
                    labels = c("Very high (pLDDT > 90)", "Confident (90 > pLDDT > 70)",
                               "Low (70 > pLDDT > 50)", "Very low (pLDDT < 50)")) +
  coord_cartesian(xlim = c(1,max(temp_df_cov$aa)), 
                  ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,20))

#Guardar gráfico

ggsave(filename = "PLOTS/ALPHAFOLD/IDRBS-064_pLDDT.png", device = "png", 
       width = 8, height = 5, units = "in", dpi = 650)


#Gráfico para #147

median(pLDDT_bios_P$Very_high)
temp_df_cov <- t(pLDDT_bios[16,c(4:145)])[!is.na(t(pLDDT_bios[16,c(4:145)])),] #147
temp_df_cov <- data.frame("pLDDT" = temp_df_cov, "aa" = 1:length(names(temp_df_cov)))
row.names(temp_df_cov) <- NULL

ggplot() +
  geom_line(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 0.4) +
  geom_point(data = temp_df_cov, aes(x = aa, y = pLDDT), size = 1) +
  #pLDDT > 90
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = Inf, 
                fill = "#000fff"), alpha = 1/8) +
  #90 > pLDDT > 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 70, ymax = 90,
                fill = "#00d4ff"), alpha = 1/8) +
  #50 > pLDDT < 70
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 70,
                fill = "#dff102"), alpha = 1/6) +
  #pLDDT < 50
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
                fill = "#f18102"), alpha = 1/6) +
  theme_bw() +
  labs(x = "Residuo de la IDR (aa)", y = "pLDDT") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  scale_fill_manual(name = "Confiabilidad", 
                    values = c("#000fff", "#00d4ff", "#f7ff00", "#ff8700"),
                    labels = c("Very high (pLDDT > 90)", "Confident (90 > pLDDT > 70)",
                               "Low (70 > pLDDT > 50)", "Very low (pLDDT < 50)")) +
  coord_cartesian(xlim = c(1,max(temp_df_cov$aa)), 
                  ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,20)) +
scale_x_continuous(breaks = seq(0,max(temp_df_cov$aa), 15))

#Guardar gráfico

ggsave(filename = "PLOTS/ALPHAFOLD/IDRBS-147_pLDDT.png", device = "png", 
       width = 8, height = 5, units = "in", dpi = 650)

