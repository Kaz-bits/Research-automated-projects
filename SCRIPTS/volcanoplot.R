#Paqueterías----

library(ggplot2)
library(ggpubr)

#Cargar base de datos Foldchange----

fold2 <- readxl::read_xlsx(path = "DATA/DATA FOR R/MAplot_At_Atg_Dataset.xlsx", 
                          col_names = TRUE)

#Volcanoplot condicional----

#Crear una función que genere el gráfico deseado al momento 
#de indicar el AGI

volcano <- function(AGI) {
  #Agregar una variable que contenga el AGI de interés
  volcan_ID <- (grep(pattern = AGI, x = fold2, fixed = FALSE, 
                     value = TRUE))
  #Agregar una variable que contenga al gráfico deseado
  volcan_pl <- ggplot() +
    geom_jitter(data = fold2, aes(x = log2FC, y = -log10(`P-value`),
                                  color = "Proteoma"), alpha = 0.8) +
    geom_jitter(data = fold2[fold2$ID == AGI,], aes(x = log2FC, y = -log10(`P-value`),
                                                    color = "Búsqueda"), size = 1.6) +
    labs(x = "log2FC", y = "-log10P") +
    theme_classic2() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.key.size = unit(0.8, "cm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = "Base de\ndatos",
                       values = c("blue", "gray"))
  #Agregar return para que genere el gráfico solamente
  if (nrow(fold2[fold2$ID == AGI,] == 1)) {
    return(volcan_pl)
  } else {
    message(paste0("No existe un AGI igual a ", AGI))
  }
}

#Ejecutar función para demostrar su funcionalidad.
#Al ejecutar la función con el AGI, si este se localiza en
#la base de datos, generará el gráfico
#Si el AGI no se localiza, generará como output un mensaje

volcano("AT1G01010") #Genera el gráfico
volcano("AT1G0101") #Genera mensaje 

