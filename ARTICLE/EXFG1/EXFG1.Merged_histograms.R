# Packages
library(ggplot2)
library(ggpubr)

# Fuctions
bins <- function(x) {
  if ((2 * IQR(x, na.rm = TRUE) / length(x)^(1/3)) > 0.05) {
    round(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3), digits = 2)
  } else {
    round(2 * IQR(x, na.rm = TRUE) / length(x)^(1/3), digits = 2)
  }
}



# Load IDRome data set
df_idrome <- read.csv(file = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/IDRome_all.csv", 
                      header = TRUE)

# Load library data set
df_library <- read.csv(file = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/Extended_fig1_data.csv", 
                       header = TRUE)

# Load library data set with organisms
df_library_org <- read.csv(file = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/IDR_Properties.csv", 
                           header = TRUE)


# Define the steps of the ticks for y-axis
step_tick <- 0.5

# Library length ----
# Determine number of bins
bin2 <- bins(df_library_org$idr_length)

# Example dataset
range2 <- range(df_library_org$idr_length)

# Compute the differences between the ranges of the data
range_diff2 <- diff(range2)

# Determine number of bins
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram
df_hist_library <- hist(df_library_org$idr_length, plot = FALSE, breaks = bin2 + 24)



# Create a data frame from the library data
df_hist_library <- data.frame(
  length = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
l1 <- ggplot() +
  geom_step(data = df_hist_library, aes(x = length, y = count), 
            linetype = "solid", size = 1, color = "orange") +
  theme_bw() +
  labs(x = "Length (aa)", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(50, 210)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(expand = c(0, 0))

# Save plot
ggsave(filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/length_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/length_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)





# Library pI ----
# Determine number of bins
bin2 <- bins(df_library_org$pI)

# Example dataset
range2 <- range(df_library_org$pI)

# Compute the differences between the ranges of the data
range_diff2 <- diff(range2)

# Determine number of bins
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram
df_hist_library <- hist(df_library_org$pI, plot = FALSE, breaks = bin2 + 24)

# Create a data frame from the library data
df_hist_library <- data.frame(
  pI = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
l2 <- ggplot() +
  geom_step(data = df_hist_library, aes(x = pI, y = count), 
            linetype = "solid", size = 1, color = "orange") +
  theme_bw() +
  labs(x = "pI", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(2, 14)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 14, 2)) +
  scale_y_continuous(expand = c(0, 0), , breaks = seq(0, 1.05, step_tick)) 

# Save plot
ggsave(plot = l2, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/pI_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = l2, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/pI_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)




# Colors
color_hist <- c("gray", "orange")

# Library FCR ----
# Determine number of bins
bin1 <- bins(df_idrome$FCR)
bin2 <- bins(df_library$FCR)

# Example dataset
range1 <- range(df_idrome$FCR)
range2 <- range(df_library$FCR)

# Compute the differences between the ranges of the data
range_diff1 <- diff(range1)
range_diff2 <- diff(range2)

# Determine number of bins
bin1 <- ceiling(range_diff1 / bin1)
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram 
df_hist_idrome <- hist(df_idrome$FCR, plot = FALSE, breaks = bin1) 
df_hist_library <- hist(df_library$FCR, plot = FALSE, breaks = bin2)

# Create a data frame from the IDRome data
df_hist_idrome <- data.frame(
  FCR = df_hist_idrome$mids,  # Midpoints of bins
  count = df_hist_idrome$counts / max(df_hist_idrome$counts)  # Normalized counts
)

# Create a data frame from the library data
df_hist_library <- data.frame(
  FCR = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
l3 <- ggplot() +
  geom_step(data = df_hist_idrome, aes(x = FCR, y = count, color = "IDRome"), 
            linetype = "solid", size = 1) +
  geom_step(data = df_hist_library, aes(x = FCR, y = count, color = "Library"), 
            linetype = "solid", size = 1) +
  theme_bw() +
  labs(x = "FCR", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_color_manual(name = NULL, values = color_hist)

# Save plot
ggsave(plot = l3, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/FCR_dist_idrome_vs_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = l3, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/FCR_dist_idrome_vs_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)








# Kappa ----
# Remove data with kappa equal to -1
df_idrome <- df_idrome[!df_idrome$kappa < 0, ]

# Determine number of bins
bin1 <- bins(df_idrome$kappa)
bin2 <- bins(df_library$kappa)

# Example dataset
range1 <- range(df_idrome$kappa)
range2 <- range(df_library$kappa)

# Compute the differences between the ranges of the data
range_diff1 <- diff(range1)
range_diff2 <- diff(range2)

# Determine number of bins
bin1 <- ceiling(range_diff1 / bin1)
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram 
df_hist_idrome <- hist(df_idrome$kappa, plot = FALSE, breaks = bin1) 
df_hist_library <- hist(df_library$kappa, plot = FALSE, breaks = bin2)

# Create a data frame from the IDRome data
df_hist_idrome <- data.frame(
  kappa = df_hist_idrome$mids,  # Midpoints of bins
  count = df_hist_idrome$counts / max(df_hist_idrome$counts)  # Normalized counts
)

# Create a data frame from the library data
df_hist_library <- data.frame(
  kappa = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
p1 <- ggplot() +
  geom_step(data = df_hist_idrome, aes(x = kappa, y = count, color = "IDRome"), 
            linetype = "solid", size = 1) +
  geom_step(data = df_hist_library, aes(x = kappa, y = count, color = "Library"), 
            linetype = "solid", size = 1) +
  theme_bw() +
  labs(x = expression(kappa), y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_color_manual(name = NULL, values = color_hist)

# Save plot
ggsave(plot = p1, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/kappa_dist_idrome_vs_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = p1, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/kappa_dist_idrome_vs_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)








# NCPR ----
# Determine number of bins
bin1 <- bins(df_idrome$NCPR)
bin2 <- bins(df_library$NCPR)

# Example dataset
range1 <- range(df_idrome$NCPR)
range2 <- range(df_library$NCPR)

# Compute the differences between the ranges of the data
range_diff1 <- diff(range1)
range_diff2 <- diff(range2)

# Determine number of bins
bin1 <- ceiling(range_diff1 / bin1)
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram 
df_hist_idrome <- hist(df_idrome$NCPR, plot = FALSE, breaks = bin1) 
df_hist_library <- hist(df_library$NCPR, plot = FALSE, breaks = bin2)

# Create a data frame from the IDRome data
df_hist_idrome <- data.frame(
  NCPR = df_hist_idrome$mids,  # Midpoints of bins
  count = df_hist_idrome$counts / max(df_hist_idrome$counts)  # Normalized counts
)

# Create a data frame from the library data
df_hist_library <- data.frame(
  NCPR = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
p2 <- ggplot() +
  geom_step(data = df_hist_idrome, aes(x = NCPR, y = count, color = "IDRome"), 
            linetype = "solid", size = 1) +
  geom_step(data = df_hist_library, aes(x = NCPR, y = count, color = "Library"), 
            linetype = "solid", size = 1) +
  theme_bw() +
  labs(x = "NCPR", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(-0.5, 0.5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(breaks = seq(-0.4, 0.4, 0.2)) +
  scale_color_manual(name = NULL, values = color_hist)

# Save plot
ggsave(plot = p2, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/NCPR_dist_idrome_vs_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = p2, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/NCPR_dist_idrome_vs_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)





# Hydropathy ----
# Determine number of bins
bin1 <- bins(df_idrome$hydropathy_kyte)
bin2 <- bins(df_library$hydropathy)

# Example dataset
range1 <- range(df_idrome$hydropathy_kyte)
range2 <- range(df_library$hydropathy)

# Compute the differences between the ranges of the data
range_diff1 <- diff(range1)
range_diff2 <- diff(range2)

# Determine number of bins
bin1 <- ceiling(range_diff1 / bin1)
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram 
df_hist_idrome <- hist(df_idrome$hydropathy_kyte, plot = FALSE, breaks = bin1) 
df_hist_library <- hist(df_library$hydropathy, plot = FALSE, breaks = bin2)

# Create a data frame from the IDRome data
df_hist_idrome <- data.frame(
  Hydropathy = df_hist_idrome$mids,  # Midpoints of bins
  count = df_hist_idrome$counts / max(df_hist_idrome$counts)  # Normalized counts
)

# Create a data frame from the library data
df_hist_library <- data.frame(
  Hydropathy = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
p3 <- ggplot() +
  geom_step(data = df_hist_idrome, aes(x = Hydropathy, y = count, color = "IDRome"), 
            linetype = "solid", size = 1) +
  geom_step(data = df_hist_library, aes(x = Hydropathy, y = count, color = "Library"), 
            linetype = "solid", size = 1) +
  theme_bw() +
  labs(x = "Hydropathy", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(0.8, 7)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(breaks = seq(0.8, 7, 1.5)) +
  scale_color_manual(name = NULL, values = color_hist)

# Save plot
ggsave(plot = p3, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/hydropathy_dist_idrome_vs_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = p3, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/hydropathy_dist_idrome_vs_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)











# Aromatics ----
# Determine number of bins
bin1 <- bins(df_idrome$fract_aro)
bin2 <- bins(df_library$aromatic)

# Example dataset
range1 <- range(df_idrome$fract_aro)
range2 <- range(df_library$aromatic)

# Compute the differences between the ranges of the data
range_diff1 <- diff(range1)
range_diff2 <- diff(range2)

# Determine number of bins
bin1 <- ceiling(range_diff1 / bin1)
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram 
df_hist_idrome <- hist(df_idrome$fract_aro, plot = FALSE, breaks = 45) 
df_hist_library <- hist(df_library$aromatic, plot = FALSE, breaks = bin2)

# Create a data frame from the IDRome data
df_hist_idrome <- data.frame(
  aromatic = df_hist_idrome$mids,  # Midpoints of bins
  count = df_hist_idrome$counts / max(df_hist_idrome$counts)  # Normalized counts
)

# Create a data frame from the library data
df_hist_library <- data.frame(
  aromatic = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
p4 <- ggplot() +
  geom_step(data = df_hist_idrome, aes(x = aromatic, y = count, color = "IDRome"), 
            linetype = "solid", size = 1) +
  geom_step(data = df_hist_library, aes(x = aromatic, y = count, color = "Library"), 
            linetype = "solid", size = 1) +
  theme_bw() +
  labs(x = "Aromatic", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(0, 0.4)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(breaks = seq(0, 0.4, 0.1)) +
  scale_color_manual(name = NULL, values = color_hist)

# Save plot
ggsave(plot = p4, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/aromatic_dist_idrome_vs_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = p4, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/aromatic_dist_idrome_vs_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)








# Aliphatic ----
# Determine number of bins
bin1 <- bins(df_idrome$fract_ali)
bin2 <- bins(df_library$aliphatic)

# Example dataset
range1 <- range(df_idrome$fract_ali)
range2 <- range(df_library$aliphatic)

# Compute the differences between the ranges of the data
range_diff1 <- diff(range1)
range_diff2 <- diff(range2)

# Determine number of bins
bin1 <- ceiling(range_diff1 / bin1)
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram 
df_hist_idrome <- hist(df_idrome$fract_ali, plot = FALSE, breaks = 45) 
df_hist_library <- hist(df_library$aliphatic, plot = FALSE, breaks = bin2)

# Create a data frame from the IDRome data
df_hist_idrome <- data.frame(
  aliphatic = df_hist_idrome$mids,  # Midpoints of bins
  count = df_hist_idrome$counts / max(df_hist_idrome$counts)  # Normalized counts
)

# Create a data frame from the library data
df_hist_library <- data.frame(
  aliphatic = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
p5 <- ggplot() +
  geom_step(data = df_hist_idrome, aes(x = aliphatic, y = count, color = "IDRome"), 
            linetype = "solid", size = 1) +
  geom_step(data = df_hist_library, aes(x = aliphatic, y = count, color = "Library"), 
            linetype = "solid", size = 1) +
  theme_bw() +
  labs(x = "Aliphatic", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(0, 0.82)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(breaks = seq(0, 0.82, 0.2)) +
  scale_color_manual(name = NULL, values = color_hist)

# Save plot
ggsave(plot = p5, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/aliphatic_dist_idrome_vs_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = p5, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/aliphatic_dist_idrome_vs_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)







# Polar ----
# Determine number of bins
bin1 <- bins(df_idrome$fract_polar)
bin2 <- bins(df_library$polar)

# Example dataset
range1 <- range(df_idrome$fract_polar)
range2 <- range(df_library$polar)

# Compute the differences between the ranges of the data
range_diff1 <- diff(range1)
range_diff2 <- diff(range2)

# Determine number of bins
bin1 <- ceiling(range_diff1 / bin1)
bin2 <- ceiling(range_diff2 / bin2)

# Summarize counts in each bin for the histogram 
df_hist_idrome <- hist(df_idrome$fract_polar, plot = FALSE, breaks = bin1) 
df_hist_library <- hist(df_library$polar, plot = FALSE, breaks = bin2)

# Create a data frame from the IDRome data
df_hist_idrome <- data.frame(
  polar = df_hist_idrome$mids,  # Midpoints of bins
  count = df_hist_idrome$counts / max(df_hist_idrome$counts)  # Normalized counts
)

# Create a data frame from the library data
df_hist_library <- data.frame(
  polar = df_hist_library$mids,  # Midpoints of bins
  count = df_hist_library$counts / max(df_hist_library$counts)  # Normalized counts
)

## Plot ----
p6 <- ggplot() +
  geom_step(data = df_hist_idrome, aes(x = polar, y = count, color = "IDRome"), 
            linetype = "solid", size = 1) +
  geom_step(data = df_hist_library, aes(x = polar, y = count, color = "Library"), 
            linetype = "solid", size = 1) +
  theme_bw() +
  labs(x = "Polar", y = "Frequency") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1.05), xlim = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1.05, step_tick)) +
  scale_x_continuous(breaks = seq(0, 1, 0.5)) +
  scale_color_manual(name = NULL, values = color_hist)

# Save plot
ggsave(plot = p6, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/polar_dist_idrome_vs_library.pdf", device = "pdf", 
       width = 4, height = 2, units = "in", dpi = 450)

ggsave(plot = p6, filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/polar_dist_idrome_vs_library.png", device = "png", 
       width = 4, height = 2, units = "in", dpi = 450)



# Merge plots
ggarrange(l1, l2 + rremove("ylab") + rremove("y.text"), l3 + rremove("ylab") + rremove("y.text"),
          p1, p2 + rremove("ylab") + rremove("y.text"), p3 + rremove("ylab") + rremove("y.text"), 
          p4, p5 + rremove("ylab") + rremove("y.text"), p6 + rremove("ylab") + rremove("y.text"), 
          ncol = 3, nrow = 3, 
          common.legend = TRUE,
          legend = "top", heights = c(0.5),
          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
          align = "h", vjust = 0.1, hjust = 0.1)

# Save plot
ggsave(filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/Extended_figure1.pdf", device = "pdf", 
       width = 10, height = 5, units = "in", dpi = 450)

ggsave(filename = "D:/Documentos/ARCHIVOS/Ext.Fig1/DATA/Extended_figure1.png", device = "png", 
       width = 10, height = 5, units = "in", dpi = 450)

