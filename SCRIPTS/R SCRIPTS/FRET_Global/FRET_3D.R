# Paqueterías
library(ggplot2)
library(plotly)

# Cargar archivo de DELTA FRET
df_fret_200 <- read.csv(file = "E:/Screening biblioteca3/FRET/all_biosensors.csv",
                        header = TRUE)

# Cargar archivo de datos de CIDER
df_fret_200_cider <- readxl::read_excel(path = "C:/Users/HP/Documents/Datos biblioteca de biosensores/Biblioteca_CIDER.xlsx", 
                                        sheet = 1, col_names = TRUE)

# Extraer los biosensores que se han analizado hasta el momento
temp1 <- df_fret_200$construct
temp2 <- as.numeric(substr(df_fret_200_cider$Entry, 7, 9))

# Obtener datos de CIDER de biosensores analizados
df_fret_200_cider <- df_fret_200_cider[(temp2 %in% temp1), ]

# Eliminar valor de SED1
df_fret_200 <- df_fret_200[!(df_fret_200$construct == 201), ]

# Juntar data frames
df_fret_200 <- cbind(df_fret_200, df_fret_200_cider)

df_fret_200$plot_r[which(df_fret_200$plot_r == 0)] <- "High"
df_fret_200$plot_r[which(df_fret_200$plot_r == 1)] <- "Medium"
df_fret_200$plot_r[which(df_fret_200$plot_r == 2)] <- "Low"



# Cargar archivos
IDR_new_cider_vs_IDRfret$CESAR[which(IDR_new_cider_vs_IDRfret$CESAR == 0 )] <- 'High'
IDR_new_cider_vs_IDRfret$CESAR[which(IDR_new_cider_vs_IDRfret$CESAR == 1 )] <- 'Medium'
IDR_new_cider_vs_IDRfret$CESAR[which(IDR_new_cider_vs_IDRfret$CESAR == 2 )] <- 'Low'



IDR_new_cider_vs_IDRfret$CESAR <- as.factor(IDR_new_cider_vs_IDRfret$CESAR)

fig3d <- plot_ly(IDR_new_cider_vs_IDRfret, x = ~NCPR, y = ~kappa,
                 z = ~DeltaFRET,
                 color = ~CESAR, colors = c('red', "blue", "darkorchid"), marker = list(size = 8))
fig3d <- fig3d %>% add_markers()
fig3d <- fig3d %>% layout(scene = list(xaxis = list(title = 'NCPR'),
                                       yaxis = list(title = 'Kappa'),
                                       zaxis = list(title = 'Delta FRET')),
                          paper_bgcolor = 'rgb(243, 243, 243)',
                          plot_bgcolor = 'rgb(243, 243, 243)')
fig3d