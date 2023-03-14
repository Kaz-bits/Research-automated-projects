# Paqueterías
library(imager)
library(ggplot2)
library(ggpubr)
library(grid)
library(ggmsa)

# Lista de imagenes
temp_files <- list.files(path = "C:/Users/HP/Documents/La biblioteca de biosensores/FRET ratio y localizacion subcelular/Biosensores Confocal/Biosensores_confocal _jpg/")

for (a in temp_files) {
  
  # Cargar imagen
  image <- load.image(file = file.path("C:/Users/HP/Documents/La biblioteca de biosensores/FRET ratio y localizacion subcelular/Biosensores Confocal/Biosensores_confocal _jpg", a))
  
  # Crear data frame
  temp <- data.frame("Columna2" = temp_files[1])
  
  # Extraer nombres de cada IDR
  temp_files <- substr(temp_files, 1, 9)
  
  # Crear una faceta de 3x7
  # Crear data frame
  temp <- data.frame("Columna1" = paste0(substr(a, 1, 5), "-", substr(a, 7, 9)))
  
  # Cambiar nombre de la IDR
  temp$Columna2 <- paste0(substr(temp$Columna2, 1, 5), "-", substr(temp$Columna1, 7, 9))
  
  # Construir facetas para cada imagen de las estructuras
  p <- ggplot(data = temp) +
    facet_wrap(~ Columna1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.line = element_line(color = "white"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", colour = NA)) +
    annotation_raster(image, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  
  # Guardar variables 
  assign(x = paste0("p", substr(a, 1, 3)), value = p)
  
}

# Juntar gráficos "p" anteriores en facetas de 3x7 
fig_alphafold_str <- ggarrange(p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, 
                               p011, p012, p013, p014, p015, p016, p017, p018, p019, p020,
                               p021, p022, p023, p024, p025, p026, p027, p028, p029, p030,
                               p031, p032, p033, p034, p035, p036, p037, p038, p039, p040,
                               p041, p042, p043, p044, p045, p046, p047, p048, p049, p050,
                               p051, p052, p053, p054, p055, p056, p057, p058, p059, p060,
                               p061, p062, p063, p064, p065, p066, p067, p068, p069, p070,
                               p071, p072, p073, p074, p075, p076, p077, p078, p079, p080,
                               p081, p082, p083, p084, p085, p086, p087, p088, p089, p090,
                               p091, p092, p093, p094, p095, p096, p097, p098, p099, p100,
                               p101, p102, p103, p104, p105, p106, p107, p108, p109, p110,
                               p111, p112, p113, p114, p115, p116, p117, p118, p119, p120,
                               p121, p122, p123, p124, p125, p126, p127, p128, p129, p130,
                               p131, p132, p133, p134, p135, p136, p137, p138, p139, p140,
                               p141, p142, p143, p144, p145, p146, p147, p148, p149, p150,
                               p151, p152, p153, p154, p155, p156, p157, p158, p159, p160,
                               p161, p162, p163, p164, p165, p166, p167, p168, p169, p170,
                               p171, p172, p173, p174, p175, p176, p177, p178, p179, p180,
                               p181, p182, p183, p184, p185, p186, p187, p188, p189, p190,
                               p191, p192, p193, p194, p195, p196, p197, p198, p199, p200,
                               ncol = 14, nrow = 15)


#Guardar gráfico de fracciones hélice / desorden
ggsave(plot = fig_alphafold_str, filename = "E:/ALL/Project_NN_proteins/PLOTS/all_IDRS_200.pdf",
       device = "pdf", width = 18, height = 20, units = "in",
       dpi = 400)

ggsave(plot = fig_alphafold_str, filename = "E:/ALL/Project_NN_proteins/PLOTS/all_IDRS_200.png",
       device = "png", width = 18, height = 20, units = "in",
       dpi = 400)






# Lista de imagenes
temp_files <- list.files(path = "C:/Users/HP/Documents/La biblioteca de biosensores/FRET ratio y localizacion subcelular/Biosensores Confocal/Biosensores_confocal_NaCl_jpg")

for (a in temp_files) {
  
  # Cargar imagen
  image <- load.image(file = file.path("C:/Users/HP/Documents/La biblioteca de biosensores/FRET ratio y localizacion subcelular/Biosensores Confocal/Biosensores_confocal_NaCl_jpg/", a))
  
  # Crear data frame
  temp <- data.frame("Columna2" = temp_files[1])
  
  # Extraer nombres de cada IDR
  temp_files <- substr(temp_files, 1, 9)
  
  # Crear una faceta de 3x7
  # Crear data frame
  temp <- data.frame("Columna1" = paste0(substr(a, 1, 5), "-", substr(a, 7, 9)))
  
  # Cambiar nombre de la IDR
  temp$Columna2 <- paste0(substr(temp$Columna2, 1, 5), "-", substr(temp$Columna1, 7, 9))
  
  # Construir facetas para cada imagen de las estructuras
  p <- ggplot(data = temp) +
    facet_wrap(~ Columna1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.line = element_line(color = "white"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", colour = NA)) +
    annotation_raster(image, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  
  # Guardar variables 
  assign(x = paste0("p", substr(a, 1, 3)), value = p)
  
}

# Juntar gráficos "p" anteriores en facetas de 3x7 
fig_alphafold_str <- ggarrange(p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, 
                               p011, p012, p013, p014, p015, p016, p017, p018, p019, p020,
                               p021, p022, p023, p024, p025, p026, p027, p028, p029, p030,
                               p031, p032, p033, p034, p035, p036, p037, p038, p039, p040,
                               p041, p042, p043, p044, p045, p046, p047, p048, p049, p050,
                               p051, p052, p053, p054, p055, p056, p057, p058, p059, p060,
                               p061, p062, p063, p064, p065, p066, p067, p068, p069, p070,
                               p071, p072, p073, p074, p075, p076, p077, p078, p079, p080,
                               p081, p082, p083, p084, p085, p086, p087, p088, p089, p090,
                               p091, p092, p093, p094, p095, p096, p097, p098, p099, p100,
                               p101, p102, p103, p104, p105, p106, p107, p108, p109, p110,
                               p111, p112, p113, p114, p115, p116, p117, p118, p119, p120,
                               p121, p122, p123, p124, p125, p126, p127, p128, p129, p130,
                               p131, p132, p133, p134, p135, p136, p137, p138, p139, p140,
                               p141, p142, p143, p144, p145, p146, p147, p148, p149, p150,
                               p151, p152, p153, p154, p155, p156, p157, p158, p159, p160,
                               p161, p162, p163, p164, p165, p166, p167, p168, p169, p170,
                               p171, p172, p173, p174, p175, p176, p177, p178, p179, p180,
                               p181, p182, p183, p184, p185, p186, p187, p188, p189, p190,
                               p191, p192, p193, p194, p195, p196, p197, p198, p199, p200,
                               ncol = 14, nrow = 15)


#Guardar gráfico de fracciones hélice / desorden
ggsave(plot = fig_alphafold_str, filename = "E:/ALL/Project_NN_proteins/PLOTS/all_IDRS_200_NaCl.pdf",
       device = "pdf", width = 18, height = 20, units = "in",
       dpi = 400)

ggsave(plot = fig_alphafold_str, filename = "E:/ALL/Project_NN_proteins/PLOTS/all_IDRS_200_NaCl.png",
       device = "png", width = 18, height = 20, units = "in",
       dpi = 400)


