# Paqueterías----
library(ggplot2)
library(ggpubr)
library(ggrepel)

# Cargar archivo de DELTA FRET
df_fret_200 <- read.csv(file = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/FRET_delta_200.csv",
                        header = TRUE)

# Cargar archivo de datos de CIDER
df_fret_200_cider <- readxl::read_excel(path = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/CIDER _Biblioteca.xlsx", 
                                        sheet = 1, col_names = TRUE)

# Extraer los biosensores que se han analizado hasta el momento
temp1 <- df_fret_200$Construct
temp2 <- as.numeric(substr(df_fret_200_cider$Entry, 7, 9))

# Obtener datos de CIDER de biosensores analizados
df_fret_200_cider <- df_fret_200_cider[(temp2 %in% temp1), ]

# Eliminar el biosensor 201 (SED1) del data frame
df_fret_200 <- df_fret_200[!(df_fret_200$Construct == 201), ]

# Juantar los dos data frame anteriores
df_fret_200 <- cbind(df_fret_200, df_fret_200_cider)


# Gráficos de correlaciones----

# Gráfico de delta FRET vs Longitud
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$Length, 
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$Length, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = as.numeric(Length), 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = as.numeric(Length), 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Longitud", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_fret_200$Length),
                           max(df_fret_200$Length)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 62, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 61, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/FRET_vs_length.pdf", 
       device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)




# Gráfico de delta FRET vs kappa
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$κ,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$κ, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = κ, 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = κ, 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = expression(kappa), 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_fret_200$κ),
                           max(df_fret_200$κ)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 0.08, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.091, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/FRET_vs_kappa.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)



# Gráfico de delta FRET vs FCR
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$FCR,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$FCR, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = FCR, 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = FCR, 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "FCR", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_fret_200$FCR),
                           max(df_fret_200$FCR)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 0.05, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.05, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/FRET_vs_FCR.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)





# Gráfico de delta FRET vs NCPR
ggplot() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_jitter(data = df_fret_200, aes(x = NCPR, 
                                      y = Mean_Delta)) +
  labs(x = "NCPR", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(-0.4,0.4), 
                  ylim = c(0,2))

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/FRET_vs_NCPR_no_correlation.pdf", 
       device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)



# Gráfico de delta FRET vs NCPR (correlación)
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$NCPR,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$NCPR, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = NCPR, 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = NCPR, 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "NCPR", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_fret_200$NCPR),
                           max(df_fret_200$NCPR)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = -0.35, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = -0.374, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)


# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/FRET_vs_NCPR_correlation.pdf", 
       device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)




# Gráfico de delta FRET vs hidropaía
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$Hydropathy,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$Hydropathy, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = Hydropathy, 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = Hydropathy, 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Hidropatía", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_fret_200$Hydropathy),
                           max(df_fret_200$Hydropathy)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 2.2, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 2.16, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/FRET_vs_Hydropathy.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)





# Gráfico de delta FRET vs desorden promovido
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$`Disorder promoting`,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$`Disorder promoting`, method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = `Disorder promoting`, 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = `Disorder promoting`, 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Fracción de desorden promovido", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_fret_200$`Disorder promoting`),
                           max(df_fret_200$`Disorder promoting`)), 
                  ylim = c(0,2.5)) +
  annotate(geom = "text", x = 0.625, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.616, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/FRET_vs_disorder_promoting.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)




# Gráfico de delta FRET vs FRET inicial----
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$FRET_0M,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$FRET_0M, 
                         method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = FRET_0M, 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = FRET_0M, 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "FRET (0 M)", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(min(df_fret_200$FRET_0M),
                           max(df_fret_200$FRET_0M)),
                  ylim = c(-0.5,2.5)) +
  annotate(geom = "text", x = 0.48, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.52, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/PLOTS_200/delta_FRET_vs_FRET_inicial.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)
