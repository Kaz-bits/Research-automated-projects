#Paqueterías----
library(tidyverse)
library(seqinr)
library(ggrepel)

#Cargar archivos----
#Cargar archivo con secuencias de letras "O"

fasta <- read.fasta(file = "SEC_O.txt", seqtype = "AA", 
                    as.string = FALSE, set.attributes = FALSE)

#Cargar archivo con rangos de desorden

rangos <- read_tsv(file = "ATG_ranges.txt", 
                   col_names = c("ATG", "BEGIN", "END"))

#Guardar los ATG en un vector (27411) llamado ATG_unique

ATG_unique <- unique(rangos$ATG)


#Generación de fasta con letras "d" (disordered)----

#Cambiar aminoácidos por letras d (disordered) 

for (ATGs in 1:length(ATG_unique)) {
  atg_ranges <- rangos %>% filter(ATG == ATG_unique[ATGs])
  for (i in 1:nrow(atg_ranges)) {
  fasta[[ATG_unique[ATGs]]][pull(atg_ranges[i, "BEGIN"]):pull(atg_ranges[i, "END"])] <- "d"
  
  }
}

#Guardar secuencias con letras d (disordered)

write.fasta(as.list(fasta), names(fasta), "sequences_d_fasta.txt") 


#Caracterización del desorden de los FTs----

#Esto genera la primer columna con el identificador ATG
df1 <- data.frame(Identificador = names(fasta))

#Esto genera la segunda columna con la longitud de cada proteína
df1$longitud <- lapply(fasta, function(x) length(x))

#Aquí se generan la tercer y cuarta columna, para residuos ordenados y 
#desordenados que existen por secuencia
df1$total_disordered_residues <- lapply(fasta, function(x) table(x)["d"])
df1$total_ordered_residues <- lapply(fasta, function(x) table(x)["O"])

#Número de segmentos ordenados y desordenados
for (a in 1:length(ATG_unique)) {
  data_1 <- data.frame(segmentos = nrow(filter(rangos, ATG == ATG_unique[a])[1]))
}

#Todos los segmentos desordenados
df1$total_disordered_segments <- lapply(fasta, function(x) 
  length(strsplit(paste0(x, collapse = ""), 
                  split = "O")[[1]][strsplit(paste0(x, collapse = ""), split = "O")[[1]] != ""]))

#Todos los segmentos ordenados
df1$total_ordered_segments <- lapply(fasta, function(x) 
  length(strsplit(paste0(x, collapse = ""), split = "d")[[1]][strsplit(paste0(x, collapse = ""), 
                                                                       split = "d")[[1]] != ""]))
#Cantidad del segmento mayor
df1$Mayor_segmento_desordenado <- nchar(lapply(fasta, function(x) 
  max(strsplit(paste0(x, collapse = ""), split = "O")[[1]][strsplit(paste0(x, collapse = ""), 
                                                                    split = "O")[[1]] != ""])))
#Porentaje de desorden
df1$Porcentaje_desordenado <- format(as.numeric(df1$total_disordered_residues)*100/as.numeric(df1$longitud), 
                                     digits = 2)

#Porcentaje de orden
df1$Porcentaje_ordenado <- format(as.numeric(df1$total_ordered_residues)*100/as.numeric(df1$longitud),
                                  digits = 2)



#Filtrado de FTs----

#Cargar archivo con los factores de transcripción proporcionado
TFs <- read.delim("TXT FILES/TFs_arabidopsis.txt")


#La base de datos indica que hay 1997 valores, pero hay 15 
#espacios en blanco por lo que debemos hacer un sort y ciclar desde
#el valor 16. Con esto obtenemos todos los códigos de los TFs 
#que se encuentran en nuestra base de datos (27411)

TF_number <- c() #crear vector vacío

for (t in sort(TFs[,1])[16:1997]) {
  for (p in grep(t, ATG_unique)) {
    TF_number <- append(TF_number, p)
  }
}

#Se contraron 1959 AGIs de los 1982 TFs en la base de datos de 27411. 
#Luego, se deben pasar estos valores de TF_number a identificadores ATG

ATGs_TFs <- data.frame("ATG_TF" = ATG_unique[TF_number])


#Comprobar que la longitud del factor corresponda con la base de datos
grep("AT1G68150", ATG_unique)
length(fasta[[ATG_unique[5133]]]) #Tiene 374 según TAIR10
nchar(TFs$PEP[1]) #Tiene 374 según la base de datos de TFs


#Obtención de secuencias de los factores----





#Construcción de base de datos para factores----

#Transformar ATGs a un dataframe
df1_TFs <- data.frame(ATGs_TFs)
head(df1_TFs)

#La longitud de cada TF según SEC_O
df1_TFs$longitud <- lapply(fasta[ATG_unique[TF_number]], 
                           function(x) length(x))

#Regiones desordenadas de los TFs
df1_TFs$total_disordered_residues <- lapply(fasta[ATG_unique[TF_number]], 
                                            function(x) table(x)["d"])
#Regiones ordenadas de los TFs
df1_TFs$total_ordered_residues <- lapply(fasta[ATG_unique[TF_number]], 
                                         function(x) table(x)["O"])

#Todos los segmentos desordenados
df1_TFs$total_disordered_segments <- lapply(fasta[ATG_unique[TF_number]], function(x) 
  length(strsplit(paste0(x, collapse = ""), 
                  split = "O")[[1]][strsplit(paste0(x, collapse = ""), 
                                             split = "O")[[1]] != ""]))

#Todos los segmentos ordenados
df1_TFs$total_ordered_segments <- lapply(fasta[ATG_unique[TF_number]], function(x) 
  length(strsplit(paste0(x, collapse = ""), 
                  split = "d")[[1]][strsplit(paste0(x, collapse = ""), 
                                             split = "d")[[1]] != ""]))

#Cantidad del segmento mayor
df1_TFs$Mayor_segmento_desordenado <- nchar(lapply(fasta[ATG_unique[TF_number]], function(x) 
  max(strsplit(paste0(x, collapse = ""), split = "O")[[1]][strsplit(paste0(x, collapse = ""), 
                                                                    split = "O")[[1]] != ""])))
#Porentaje de desorden
df1_TFs$Porcentaje_desordenado <- format(as.numeric(df1_TFs$total_disordered_residues)*100/as.numeric(df1_TFs$longitud), 
                                     digits = 2)

#Porcentaje de ordenamiento
df1_TFs$Porcentaje_ordenado <- format(as.numeric(df1_TFs$total_ordered_residues)*100/as.numeric(df1_TFs$longitud),
                                  digits = 2)


#Comprobar que el dataframe se construyó correctamente
head(df1_TFs)


#Correlación de Pearson combinaciones----

#Realizar uan correlación de Pearson comparando distintos
#parámetros calculados

cor.test(as.numeric(df1_TFs$longitud), as.numeric(df1_TFs$Porcentaje_desordenado), 
         method = "pearson", conf.level = 0.95)

cor.test(as.numeric(df1_TFs$longitud), as.numeric(df1_TFs$total_disordered_segments), 
         method = "pearson", conf.level = 0.95)




