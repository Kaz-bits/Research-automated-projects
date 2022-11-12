# Paqueterías
library(ggplot2)
library(plotly)

# Cargar archivo de DELTA FRET
df_fret_200_delta <- read.csv(file = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/FRET_delta_200.csv", 
                              header = TRUE)

# Cargar archivo de datos de CIDER
df_fret_200_cider <- readxl::read_excel(path = "/media/kaz-bits/TOSHIBA EXT/Project_IDP_D2P2/DATA/DATA FOR R/FRET/Data_FRET/COMPLETE_DATA/LIBRARY/CIDER _Biblioteca.xlsx", 
                                        sheet = 1, col_names = TRUE)


# Extraer los biosensores que se han analizado hasta el momento
temp1 <- df_fret_200_delta$Construct
temp2 <- as.numeric(substr(df_fret_200_cider$Entry, 7, 9))

# Obtener datos de CIDER de biosensores analizados
df_fret_200_cider <- df_fret_200_cider[(temp2 %in% temp1), ]

# Eliminar valor de SED1
df_fret_200_delta <- df_fret_200_delta[!(df_fret_200_delta$Construct == 201), ]

# Juntar data frames
df_fret_200_delta <- cbind(df_fret_200_delta, df_fret_200_cider)

# Determinar la cantidad de biosensores de respuesta
# alta, media y baja

# Biosensores de respuesta alta, baja e intermedia 
# de acuerdo con los cuartiles
temp_25 <- unname(quantile(df_fret_200_delta$Mean_Delta)[2]) # 25%
temp_75 <- unname(quantile(df_fret_200_delta$Mean_Delta)[4]) # 75%

# Cuartiles de 25, 50 y 75%
temp_75_delta <- df_fret_200_delta[df_fret_200_delta$Mean_Delta > temp_75, ] # Alta
temp_25_delta <- df_fret_200_delta[df_fret_200_delta$Mean_Delta < temp_25, ] # Baja
temp_50_delta <- df_fret_200_delta[df_fret_200_delta$Mean_Delta <= temp_75 &
                               df_fret_200_delta$Mean_Delta >= temp_25, ]

# Agregar columnas de respuesta a cada data frame
temp_25_delta$Response <- "Low"
temp_50_delta$Response <- "Medium"
temp_75_delta$Response <- "High"

# Juntar dataframes anteriores
df_fret_200_delta <- rbind(temp_25_delta, temp_50_delta, temp_75_delta)

# Convertir columna "Response" a factores
df_fret_200_delta$Response <- as.factor(df_fret_200_delta$Response)

df_fret_200_delta$κ
# Graficar datos de acuerdo con la respuesta
fig3d <- plot_ly(data = df_fret_200_delta, x = ~NCPR, y = ~κ, z = ~Mean_Delta,
                 color = ~Response, colors = c('red', "blue", "darkorchid"),
                 marker = list(size = 8))

fig3d <- fig3d %>% add_markers()

fig3d <- fig3d %>% layout(scene = list(xaxis = list(title = 'Net Charge Per Residue'),
                                       yaxis = list(title = 'Kappa'),
                                       zaxis = list(title = expression(Delta * "FRET"))),
                          paper_bgcolor = 'rgb(243, 243, 243)',
                          plot_bgcolor = 'rgb(243, 243, 243)')

fig3d


# Valores arriba de SED1
temp_sed1 <- df_fret_200_delta[(df_fret_200_delta$Construct == 201), ]$Mean_Delta
temp_all <- df_fret_200_delta[!(df_fret_200_delta$Construct == 201), ]$Mean_Delta
length(temp_all[(temp_all > temp_sed1)])

