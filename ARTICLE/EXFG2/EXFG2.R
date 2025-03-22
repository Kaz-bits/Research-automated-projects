# Libraries
library(ggplot2)

# Load data 
df_cider <- read.csv(file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/all_localcider_values.csv", header = TRUE)

# Create a data frame with the values of the diagram of states (Holehouse)
r1 <- data.frame("f1" = c(0, 0.25, 0), "f2" = c(0.25, 0, 0))
r2 <- data.frame("f1" = c(0.25, 0.35, 0, 0), "f2" = c(0, 0, 0.35, 0.25))
r3 <- data.frame("f1" = c(0.65, 0.35, 0, 0.35), "f2" = c(0.35, 0.65, 0.35, 0))
r4 <- data.frame("f1" = c(0.35, 0, 0), "f2" = c(0.65, 1, 0.35))
r5 <- data.frame("f1" = c(0.65, 0.35, 1), "f2" = c(0.35, 0, 0))

# Create numerical axes
label_vec <- c("0" = NULL, "0.2" = 0.2, "0.4" = 0.4, "0.6" = 0.6, "0.8" = 0.8, "1.0" = 1.0)

# Plot diagram of states
p <- ggplot() +
  geom_polygon(data = r1, aes(x = f1, y = f2), fill = "#9FFF40") +
  geom_polygon(data = r2, aes(x = f1, y = f2), fill = "#6DC694") +
  geom_polygon(data = r3, aes(x = f1, y = f2), fill = "#408B40") +
  geom_polygon(data = r4, aes(x = f1, y = f2), fill = "#FF4040") +
  geom_polygon(data = r5, aes(x = f1, y = f2), fill = "#4040FF") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_classic() +
  labs(x = bquote(f["+"]), y = bquote(f["-"])) +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        panel.grid = element_blank()) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  scale_x_continuous(breaks = label_vec, expand = c(0, 0))

# Add IDRs to the diagram of states
p1 <- p + geom_jitter(data = df_cider, aes(x = f_pos, y = f_neg), size = 1)
p1

# Guardar gráfico en PDF
ggsave(plot = p1, filename = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/Extended_Data_Fig2.pdf", 
       device = "pdf", width = 6, height = 5, units = "in", dpi = 450)

# Guardar gráfico en PNG
ggsave(plot = p1, filename = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/Extended_Data_Fig2.png", 
       device = "png", width = 6, height = 5, units = "in", dpi = 450)


