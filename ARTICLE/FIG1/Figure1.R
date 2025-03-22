# Load required packages
library(ggplot2)
library(ggpubr)
loadfonts()

# Function for performing FRET efficiency
FRET.boxplot <- function(dir.fret, 
                         format.plot = "png",
                         ymax_axis = 3, 
                         ymin_axis = 0.85,
                         yaxis_ticks = 0.5, 
                         bios_name = TRUE,
                         ymax_name = 2.85, 
                         bios_vec = c()) {
  
  # Obtain the name of each construct
  temp_files <- list.files(dir.fret)
  
  for (bios in temp_files) {
    
    # Obtain a directory for each construct
    temp_path <- file.path(dir.fret, bios, "DATA", paste0(bios, ".csv"))
    
    # Verify if the file of each construct
    if (file.exists(temp_path) == TRUE) {
      
      # Load file
      fret_ratio <- read.csv(file = file.path(dir.fret, bios, "DATA", 
                                              paste0(bios, ".csv")), 
                             header = TRUE)
      
      # Create color palette
      color_boxplot <- c("#add8e6", "#92b8dc", "#7698d1", 
                         "#5b78c7", "#3f58bd", "#2438b2", 
                         "#0818a8")
      
      # Select the max value from each condition
      max1 <- max(fret_ratio[fret_ratio$Treatment == 200,]$Normalized)
      max2 <- max(fret_ratio[fret_ratio$Treatment == 400,]$Normalized)
      max3 <- max(fret_ratio[fret_ratio$Treatment == 600,]$Normalized)
      max4 <- max(fret_ratio[fret_ratio$Treatment == 800,]$Normalized)
      max5 <- max(fret_ratio[fret_ratio$Treatment == 1000,]$Normalized)
      max6 <- max(fret_ratio[fret_ratio$Treatment == 1500,]$Normalized)
    
      # Perform statistics t-test for each condition
      # t.test for 0 vs 200
      p1 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 200,]$Normalized)[[3]]
      
      
      # t.test for 0 vs 400
      p2 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 400,]$Normalized)[[3]]
      
      
      # t.test for 0 vs 600
      p3 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 600,]$Normalized)[[3]]
      
      # t.test for 0 vs 800
      p4 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 800,]$Normalized)[[3]]
      
      # t.test for 0 vs 1000
      p5 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 1000,]$Normalized)[[3]]
      
      # t.test for 0 vs 1500
      p6 <- t.test(fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1,]$Normalized, 
                   fret_ratio[fret_ratio$Treatment == 1500,]$Normalized)[[3]]
      
      # Significance level 
      signif_level <- c(ns = 1, "*" = 0.05, "**" = 0.01, 
                        "***" = 0.001, "****" = 0.0001)
      
      
      # Merge p-values for each condition
      a <- c(p1, p2, p3, p4, p5, p6)
      
      
      # Evaluate p-values for each condition
      # For 0 vs 200
      i <- c(a[1] > 0.05, a[1] <= 0.05, a[1] <= 0.01, a[1] <= 0.001, a[1] <= 0.0001)
      # For 0 vs 400
      h <- c(a[2] > 0.05, a[2] <= 0.05, a[2] <= 0.01, a[2] <= 0.001, a[2] <= 0.0001)
      # For 0 vs 600
      j <- c(a[3] > 0.05, a[3] <= 0.05, a[3] <= 0.01, a[3] <= 0.001, a[3] <= 0.0001)
      # For 0 vs 800
      k <- c(a[4] > 0.05, a[4] <= 0.05, a[4] <= 0.01, a[4] <= 0.001, a[4] <= 0.0001)
      # For 0 vs 1000
      l <- c(a[5] > 0.05, a[5] <= 0.05, a[5] <= 0.01, a[5] <= 0.001, a[5] <= 0.0001)
      # For 0 vs 1500
      m <- c(a[6] > 0.05, a[6] <= 0.05, a[6] <= 0.01, a[6] <= 0.001, a[6] <= 0.0001)
      
      # Extract significance values from p-values
      p1_box <- (names(signif_level[i]))[length((names(signif_level[i])))] # p1
      p2_box <- (names(signif_level[h]))[length((names(signif_level[h])))] # p2
      p3_box <- (names(signif_level[j]))[length((names(signif_level[j])))] # p3
      p4_box <- (names(signif_level[k]))[length((names(signif_level[k])))] # p4
      p5_box <- (names(signif_level[l]))[length((names(signif_level[l])))] # p5
      p6_box <- (names(signif_level[m]))[length((names(signif_level[m])))] # p6
      
      # Convert the treatment column to a factor
      fret_ratio$Treatment <- as.factor(fret_ratio$Treatment)
      
      # Create boxplot
      plot <- ggplot(data = fret_ratio) +
        # Add boxplot layer
        geom_boxplot(aes(x = Treatment, y = Normalized, 
                         group = Treatment), fill = color_boxplot,
                     outlier.shape = NA) +
        # Add scatter layer
        geom_jitter(aes(x = Treatment, y = Normalized, 
                        group = Treatment), color = "black") +
        # Add theme layer
        labs(x = "[NaCl] (mM)", y = "Normalized\nDxAm/DxDm") +
        theme_bw() +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              panel.grid = element_blank()) +
        coord_cartesian(ylim = c(ymin_axis, ymax_axis)) +
        scale_y_continuous(breaks = seq(ymin_axis, ymax_axis, yaxis_ticks)) +
        annotate(geom = "text", x = 2, y = max1 + 0.1, label = p1_box) +
        annotate(geom = "text", x = 3, y = max2 + 0.1, label = p2_box) +
        annotate(geom = "text", x = 4, y = max3 + 0.1, label = p3_box)  +
        annotate(geom = "text", x = 5, y = max4 + 0.1, label = p4_box) +
        annotate(geom = "text", x = 6, y = max5 + 0.1, label = p5_box) +
        annotate(geom = "text", x = 7, y = max6 + 0.1, label = p6_box) +
        
        # Add an if statement for adding the name of the construct
        if (bios_name == TRUE) {
          
          # For adding the desired name 
          annotate(geom = "text", x = 4, y = ymax_name, label = paste0("IDR", bios),
                   size = 3)
          
        } else {
          
          # For adding the name put it in the folder FRET
          annotate(geom = "text", x = 4, y = ymax_name, label = bios,
                   size = 3)
        }
      
      # Convert bios to numeric and check against bios_vec
      bios_numeric <- as.numeric(bios)
      
      # Check if plot needs to be save in a independent variable
      if (bios_numeric %in% bios_vec) {
        
        # Save plot in a independent variable 
        assign(x = paste0("plt_", bios), 
               value = plot, 
               envir = .GlobalEnv)
        
        # Print confirmation
        print(paste0("variable plt_", bios, " has been assigned."))
        
      }
      
      # Save plot
      ggsave(plot = plot, filename = file.path(dir.fret, bios, "PLOTS", paste0(bios, ".", format.plot)),
             device = format.plot, width = 6, height = 4, units = "in", 
             dpi = 400) 
      
      
    } else {
      
      next
      
      
    }
  }
  
  # Show messages
  message("Plots created with succeed")
  
}



# Run function
FRET.boxplot(dir.fret = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/FRET/FRET/", 
             format.plot = "pdf", 
             ymax_axis = 2.1, 
             yaxis_ticks = 0.25, 
             ymax_name = 2.05, 
             bios_vec = c(1, 2, 3, 8, 67, 163))


FRET.boxplot(dir.fret = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/FRET/FRET/", 
             format.plot = "png", 
             ymax_axis = 2.1, 
             yaxis_ticks = 0.25, 
             ymax_name = 2.05)


# Add modifications to plots
plt_001 <- plt_001 + 
           ggtitle("TDCP43-C") +
           theme(plot.title = element_text(hjust = 0.5, size = 20))

plt_002 <- plt_002 + 
           ggtitle("FUS-N") +
           theme(plot.title = element_text(hjust = 0.5, size = 20)) +
           rremove("ylab")

plt_003 <- plt_003 + 
           ggtitle("FUS-C") +
           theme(plot.title = element_text(hjust = 0.5, size = 20)) +
           rremove("ylab")

plt_008 <- plt_008 + 
           ggtitle("p53-C") +
           theme(plot.title = element_text(hjust = 0.5, size = 20)) +
           rremove("ylab")

plt_067 <- plt_067 + 
           ggtitle("Ash1-C") +
           theme(plot.title = element_text(hjust = 0.5, size = 20)) +
           rremove("ylab")

plt_163 <- plt_163 + 
           ggtitle("RvCAHS1 LR") +
           theme(plot.title = element_text(hjust = 0.5, size = 20)) +
           coord_cartesian(ylim = c(0.85, 4)) +
           scale_y_continuous(breaks = seq(0.85, 4.5, 0.8)) +
           annotate(geom = "text", x = 4, y = 3.85, label = "IDR163", size = 3) +
           rremove("ylab")


# Combine plots
ggarrange(plt_001, plt_002, plt_003, plt_008, plt_067, plt_163,
          ncol = 6, nrow = 1, 
          labels = c("b", "c", "d", "e", "f", "g"),
          align = "h", hjust = 0.1)

# Save plot
ggsave(filename = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/FIG1/Figure1_panels_b_to_g.pdf", device = "pdf", 
       width = 22, height = 4, units = "in", dpi = 450)

ggsave(filename = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/FIG1/Figure1_panels_b_to_g.png", device = "png", 
       width = 22, height = 4, units = "in", dpi = 450)

