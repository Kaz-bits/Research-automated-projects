# Paqueterías
library(ggplot2)

# Cargar datos de condensados
cond_high <- readxl::read_xlsx(path = "E:/ALL/Project_NN_proteins/DATA/CLEAN/condensados/df_fret_high_delta CONDENSADOS.xlsx", 
                               sheet = 4)

cond_med <- readxl::read_xlsx(path = "E:/ALL/Project_NN_proteins/DATA/CLEAN/condensados/df_fret_medium_delta CONDENSADOS.xlsx", 
                               sheet = 4)

cond_low <- readxl::read_xlsx(path = "E:/ALL/Project_NN_proteins/DATA/CLEAN/condensados/df_fret_low_delta CONDENSADOS.xlsx", 
                               sheet = 4)

# Juntar data frames
condensates <- rbind(cond_high, cond_med, cond_low)

# Ordenar por factores
condensates$Response <- factor(condensates$Response, levels = c("High", "Medium", "Low"))


# Gráfico de barras
ggplot() +
  geom_bar(data = condensates, 
           aes(x = Response, y = ..count../sum(..count..), fill = Response), show.legend = FALSE) +
  theme_classic() +
  labs(y = "Fraction of IDR that\n form condensates",
       x = expression(Delta*"FRET response")) +
  theme(axis.title = element_text(size = 14, color = "white"),
        axis.text = element_text(size = 12, color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.line = element_line(color = "white"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  coord_cartesian(ylim = c(0, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3288FF", "#65BFFF", "#99E5FF"))

# Guardar gráfico
ggsave(filename = "E:/ALL/Project_NN_proteins/PLOTS/obs/PDF/condensates_delta_fret.pdf", 
       device = "pdf", width = 3, height = 3.5, units = "in", dpi = 400, bg = "transparent")

ggsave(filename = "E:/ALL/Project_NN_proteins/PLOTS/obs/PNG/condensates_delta_fret.png", 
       device = "png", width = 3, height = 3.5, units = "in", dpi = 400, bg = "transparent")


