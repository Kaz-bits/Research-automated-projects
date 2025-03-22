# Load data (contains the 200 IDRs with sparrow values)
df1_sparrow <- read.csv(file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/IDRBS_library_sparrow_200.csv",
                        header = TRUE)

# Load data (contains the isoelectric point)
df2_sparrow <- read.csv(file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/Extended_fig1_data.csv", 
                        header = TRUE)

# Load data (contains the 188 IDRs needed for the analysis)
df3_sparrow <- read.csv(file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/IDRBS_library_sparrow_188.csv", 
                        header = TRUE)

# Load data (contains the 200 IDRs with delta and response values)
df_library <- read.csv(file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/all_biosensors.csv", 
                       header = TRUE)

# Load data (contains the region plot from CIDER phase diagram)
df_regions <- read.csv(file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/Library_regionplot.csv", 
                       header = TRUE)


# Save quartiles (1st and 3rd)
i <- summary(df_library$mean_delta)[[2]] # 1st
h <- summary(df_library$mean_delta)[[5]] # 3rd

# Classify IDR by delta FRET response
temp_class <- ifelse(df_library$mean_delta < i, yes = "Low", 
                     no = ifelse(df_library$mean_delta > h, yes = "High", no = "Medium"))


# Add the response column to df_library data set
df_library$Response <- temp_class

# Remove LEA4-5 (IDRBS-201)
df_library <- df_library[!df_library$construct == 201, ]

# Add data from df1_sparrow to df_library 
df_library <- cbind(df_library, df1_sparrow)

# Arrange columns
df_library <- df_library[, c(5, 2:4, 6:ncol(df_library))]

# Rename columns
names(df_library)[2] <- "delta"

# Add isolectric point to df_library
df_library$pI <- df2_sparrow$pI

# Add CIDER region to df_library
df_library$Plot_region <- df_regions$Plot.region

# Save file with 200 IDRs
write.csv(x = df_library, file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/ARTICLE/IDRBS_library_sparrow_200.csv", 
          quote = FALSE, row.names = FALSE)

# Filter data to obtain the 188 IDRs needed for the analysis
df_library <- df_library[as.integer(substr(df_library$Construct, 7, 9)) %in% df3_sparrow$Construct, ]


# Save file with 188 IDRs
write.csv(x = df_library, file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/ARTICLE/IDRBS_library_sparrow_188.csv", 
          quote = FALSE, row.names = FALSE)



# Classifcation of the data based on 188 IDRs ----
# Load data (contains the 200 IDRs with sparrow values)
df_sparrow <- read.csv(file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/ARTICLE/IDRBS_library_sparrow_188.csv",
                       header = TRUE)

# Save quartiles (1st and 3rd)
i <- summary(df_sparrow$delta)[[2]] # 1st
h <- summary(df_sparrow$delta)[[5]] # 3rd

# Classify IDR by delta FRET response
temp_class <- ifelse(df_sparrow$delta < i, yes = "Low", 
                     no = ifelse(df_sparrow$delta > h, yes = "High", no = "Medium"))


# Add the response column to df_library data set
df_sparrow$Response <- temp_class

# Save file with 188 IDRs
write.csv(x = df_library, file = "C:/Users/Cesar/OneDrive/Escritorio/Ext.Fig1/DATA/ARTICLE/IDRBS_library_sparrow_188.csv", 
          quote = FALSE, row.names = FALSE)

