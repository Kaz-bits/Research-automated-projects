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
eee(eetemp2 %in% temp1)
essseweqweweqwe #no se que es esto jaja
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
ggsave(filename = "PLOTS/CONS_PLOTS/FRET_vs_length.pdf", device = "pdf", 
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
  annotate(geom = "text", x = 0.075, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.073, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "PLOTS/CONS_PLOTS/FRET_vs_kappa.pdf", device = "pdf", 
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
  annotate(geom = "text", x = 0.19, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.19, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "PLOTS/CONS_PLOTS/FRET_vs_FCR.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)





# Gráfico de delta FRET vs NCPR
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$NCPR,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$NCPR, method = "pearson")[[3]], digits = 2)


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
ggsave(filename = "PLOTS/CONS_PLOTS/FRET_vs_NCPR.pdf", device = "pdf", 
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
  annotate(geom = "text", x = 2.92, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 2.93, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "PLOTS/CONS_PLOTS/FRET_vs_Hydropathy.pdf", device = "pdf", 
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
  annotate(geom = "text", x = 0.75, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.75, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "PLOTS/CONS_PLOTS/FRET_vs_disorder_promoting.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)




# Gráfico de delta FRET vs FRET inicial----
temp <- signif(cor(df_fret_200$Mean_Delta,
                   df_fret_200$FRET_inicial_0M,
                   method = "pearson"), digits = 2)
temp1 <- signif(cor.test(df_fret_200$Mean_Delta,
                         df_fret_200$FRET_inicial_0M, 
                         method = "pearson")[[3]], digits = 2)


ggplot() +
  geom_jitter(data = df_fret_200, aes(x = FRET_inicial_0M, 
                                      y = Mean_Delta)) +
  geom_smooth(data = df_fret_200, aes(x = FRET_inicial_0M, 
                                      y = Mean_Delta),
              method = "lm", fill = "gray", color = "blue", alpha = 0.4) +
  labs(x = "Basal FRET", 
       y = expression(Delta * "FRET")) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(xlim = c(0.7,1.4), 
                  ylim = c(-0.5,2.5)) +
  annotate(geom = "text", x = 0.75, y = 2.4, 
           label = paste0("r = ", temp), size = 4.5) +
  annotate(geom = "text", x = 0.77, y = 2.15,
           label = paste0("p = ", temp1), size = 4.5)

# Guardar gráfico
ggsave(filename = "PLOTS/CONS_PLOTS/delta_FRET_vs_FRET_inicial.pdf", device = "pdf", 
       width = 5, height = 3, units = "in", dpi = 600)
