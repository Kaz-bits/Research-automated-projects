#Paqueterías----
library(Peptides)
library(seqinr)
library(tidyverse)

#Cargar archivo fasta----

seqAA <- read.fasta(file = "TAIR_bien.txt", seqtype = "AA", 
                    as.string = "FALSE", set.attributes = FALSE)

#Extraccion de residuos de regiones desordenadas----

disor_list <- list() #Crear lista vacía

for (s in 1:length(seqAA)) {
  disor_vector <- c()
  temp1 <- rangos %>% filter(ATG == ATG_unique[s])
  
  for (i in 1:nrow(temp1)) {
    temp2 <- paste0(seqAA[[s]][as.numeric(temp1[i, "BEGIN"]):as.numeric(temp1[i, "END"])], collapse = "")
    disor_vector[i] <- temp2
  }
  seq_disordered <- paste0(disor_vector, collapse = "")
  disor_list[s] <- as.list(seq_disordered)
}

#Obtención de residuos individuales----

df_n <- data.frame() #Crear dataframe vacío
tb <- vector()
for (a in 1:length(disor_list)) {
  for (b in disor_list[[a]][1]) {
    for (c in aaComp(b)) {
      for (d in c["Acidic", "Number"]) {
        
        tb[a] <- d
        
          #df_n <- data.frame("Tiny" = tb)
          #df_n$Small <- tb
          #df_n$Aliphatic <- tb
          #df_n$Aromatic <- tb
          #df_n$NonPolar <- tb
          #df_n$Polar <- tb
          #df_n$Charged <- tb
          #df_n$Basic <- tb
          df_n$Acidic <- tb
    
      }
    } 
  }
}

head(df_n, n = 8)

#Obtención de porcentaje de residuos individuales----

for (a in 1:length(disor_list)) {
  for (b in disor_list[[a]][1]) {
    for (c in aaComp(b)) {
      for (d in c["Acidic", "Mole%"]) {
        
        tb[a] <- d
        
        #df_n$Tiny_percent_disordered <- tb
        #df_n$Small_percent_disordered <- tb
        #df_n$Aliphatic_percent_disordered <- tb
        #df_n$Aromatic_percent_disordered <- tb
        #df_n$NonPolar_percent_disordered <- tb
        #df_n$Polar_percent_disordered <- tb
        #df_n$Charged_percent_disordered <- tb
        #df_n$Basic_percent_disordered <- tb
        df_n$Acidic_percent_disordered <- tb

      }
    } 
  }
}
      
head(df_n, n = 8)

#Obtención de longitud de las regiones----

for (a in 1:length(disor_list)) {
  for (b in disor_list[[a]][1]) {
    for (c in aaComp(b)) {
      for (d in ((c["Acidic", "Number"]*100)/nchar(b))) {
        
        tb[a] <- d
        
        #df_n$Tiny_percent_total <- tb
        #df_n$Small_percent_total <- tb
        #df_n$Aliphatic_percent_total  <- tb
        #df_n$Aromatic_percent_total  <- tb
        #df_n$NonPolar_percent_total <- tb
        #df_n$Polar_percent_total  <- tb
        #df_n$Charged_percent_total <- tb
        #df_n$Basic_percent_total  <- tb
        df_n$Acidic_percent_total  <- tb
        
      }
    } 
  }
}

head(df_n, n = 8)

#Colocar los códigos ATG en el dataframe df_n en la 
#primero columna para su identificación

ATG_seqAA <- names(seqAA) #Extraer ATGs del archivo de 27411
df_n <- data.frame("ATG" = ATG_seqAA, df_n) #Agregar ATGs


#Determinacion de composicion por cada residuo invidual----

seq_disordered <- strsplit(paste0(disor_vector, collapse = ""), split = "")[[1]]

table(seq_disordered)[aaList()]

df_residuos <- data.frame(aa = aaList(), table(seq_disordered)[aaList()])[,c(1,3)]
df_residuos$Freq[is.na(df_residuos$Freq)] <- 0
#df_final columnas # de A, # de C, 
#Percentage # de A en residuos desordenados,
#Percentage # de A respecto a la longitud total de la proteina 

#Depositar todo en un dataframe.


#Para obtener los rangos de un ATG
ranges_rev <- rangos[order(rangos$BEGIN, decreasing = TRUE),]

#Para separar o dividir la secuencia del fasta (la primera)
ord_seq <- c()
for (i in 1:length(seqAA)) {
  ord_seq[i] <- paste0(seqAA[[i]], collapse = "")
}
head(ord_seq)

#Loop para obtener los residuos de aminoacido juntos (temp) y
#obtener la secuencia sin las regiones desordenadas (ord_seq)

for (i in 1:nrow(rangos)) {
  for (j in 1:length(seqAA)) {
  temp <- paste0(seqAA[[j]][rangos[i, "BEGIN"]:rangos[i, "END"]], collapse = "")
  ord_seq <- gsub(pattern = temp, replacement = "", x = ord_seq)
  }
}

head(ord_seq)
#Determinar la composición de aminoácidos de la nueva secuencia
aaComp(ord_seq)

#Generar la secuencia separada por residuo de aminoacido
ord_seq <- strsplit(ord_seq, "")[[1]]

#Obtener un dataframe con los residuos por aminoacido y su
#frecuencia de aparicion en la secuencia. El primer dataframe genera todos los
#aminoacidos y su frecuencia en la secuencia de estudio
df_residuos_ordenados <- data.frame(aa = aaList(), table(ord_seq)[aaList()])[,c(1,3)]

#A todas los renglones donde no haya nigún valor asignado, se le coloca el cero
#especificando que dicha secuencia no cuenta con nigun aminoacido del que sea
#especificado
df_residuos_ordenados$Freq[is.na(df_residuos_ordenados$Freq)] <- 0



#Determinacion de composicion por cada residuo invidual (bien)----

#Crear un dataframe que tenga como columnas los nombres de cada 
#residuo de aminoácido y nombrarlo (en este caso) como "aa"
aa <- as.data.frame(t(data.frame("ATG" = aaList(), row.names = aaList())))
aa

#Esta es una prueba para saber que funciona con las primeras
#diez secuencias de seqAA
ten <- disor_list[1:10]

#Genera un loop anidado (dos for), el primer "for" proporciona la
#longitud del 1 al 27411 para que ese resultado sea ahora el input
#en el segundo "for". 

#El segundo "for" lo que hace es obtener la frecuencia de aparicion
#de cada aminoacido (o letra), y se coloca la columna "aa" = aaList()
#para asegurarnos de que se cuenten todos los residuos, y los que no
#aparezcan se les ponga un NA en automatico

#Ese valor se guarda en la variable "b" y se añade a otra variable
#llamada residuos en forma de dataframe. Se agregan 0 a todos aquellos
#valores que sean NA (si es que hay) y finalmente se añade ese resultado
#como un nuevo renglón

for (a in 1:length(disor_list)) {
  for (b in data.frame("aa" = aaList(), table(strsplit(disor_list[[a]], 
                                                       split = ""))[aaList()])[,c(1,3)]) {
    residuos <- as.data.frame(b) #Guardar todo en un dataframe
    residuos$b[is.na(residuos$b)] <- 0 #renglon nuevo sin NA
    aa[nrow(aa) + 1, ] <- residuos[,1] #agregar renglon a al dataframe "aa"
  }
}

#Al final vamos a obtener un dataframe con el doble de renglones (24711 * 2)
#porque se filtraron los nombres de los aminoacidos, para eliminarlos, hay que
#aplicar el comando de abajo y guardar todo en el mismo dataframe. Como van de
#2 en 2, se coloca nrow(aa) para que de el total de renglones, y elimine esos
#renglones al colocar el signo de "menos" al inicio

aa <- aa[-seq(from = 2, to = nrow(aa), by = 2), ] #Eliminar todas las letras 

#Como aun queda el primero renglon con letras, lo eliminanos igual del dataframe

aa <- aa[-1,] #Eliminar el primer renglon

#Verificamos

head(aa, n = 10) #Verificar el dataframe 
nrow(aa) #Comprobar que sean todas las secuencias

#Al final ese dataframe se añade al ya creado, el orden sigue siendo el mismo,
#no se afectó.


#Generación de base de datos completa de aminoácidos----

#Juntar ambos dataframes para tener el dataframe final, pero primero
#hacer una copia del dataframe original

df_final <- data.frame(df_n, aa)

#guardar dataframe en un archivo .csv
write.csv(x = df_final, file = "df_arabidopsis.csv")


#Caracterización estructural----

#cargar archivo con informacion sobre plegamiento
struc_class <- read.table(file = "TXT FILES/TAIR_MW50kDA_SCOP.txt", 
                          sep = "\t", header = TRUE, skip = c(1), 
                          col.names = c("ATG", "structural_class", "info"))

#Eliminamos la columan info para trabajar con las demás
struc_class$info <- NULL

#Filtrar los que coinciden con los 27411 códigos que tenemos y quitas los que no
#tienen información de estructura

ATG_struc_class <- struc_class %>% filter(ATG %in% ATG_unique) %>% 
  filter(!is.na(structural_class))

#Eliminar espacios en blanco
ATG_struc_class <- ATG_struc_class[ATG_struc_class$structural_class != "",]

#Mantener informacion de "all alpha"
ATG_struc_class <- ATG_struc_class[ATG_struc_class$structural_class == "all alpha",]

#Filtrar ATG_struc_class anterior con las 27411 secuencias
ATG_alpha_27411 <- ATG_struc_class %>% filter(ATG %in% ATG_unique)

#Agregar columna con base de datos obtenida (TAIR)
ATG_alpha_27411$database <- "TAIR"

#Filtrar ATG_struc_class con FTs significativos
ATG_alpha_FT <- ATG_struc_class %>% filter(ATG %in% ATGs_TFs$ATG_TF)

#Agregar columna con base de datos obtenida (FT)
ATG_alpha_FT$database <- "FT"

#Juntar informacion en un dataframe
ATG_alpha_total <- data.frame()
ATG_alpha_total <- rbind(ATG_alpha_total, ATG_alpha_27411, ATG_alpha_FT)

#Cargar dataframe enorme
df_proteome <- read.csv(file = "C:/Users/Windows/Desktop/Lab_103/Servicio social/LINUX_COURSE_DATA/df_arabidopsis_proteome.csv")
df_proteome$X <- NULL

#filtrar con el dataframe enorme para obtener toda la informacion
df_alpha <- data.frame(matrix(nrow = 0, ncol = 76)) 
colnames(df_alpha) <- colnames(df_proteome)

for (i in ATG_alpha_total$ATG) {
  df_alpha[nrow(df_alpha) + 1,] <- df_proteome[df_proteome$ATG == i,]
}

#ordernar datos y agregar columna extra
ATG_alpha_total <- ATG_alpha_total[order(ATG_alpha_total$ATG),]
df_alpha <- df_alpha[order(df_alpha$ATG),]

#juntar dataframes
df_alpha <- cbind(ATG_alpha_total[, c(2,3)], df_alpha)
df_alpha <- df_alpha[, c(3, 1,2, 4:78)]
df_alpha <- df_alpha[order(df_alpha$database),]

write.csv(x = df_alpha, file = "C:/Users/Windows/Desktop/Lab_103/Servicio social/LINUX_COURSE_DATA/df_alpha.csv")



###Obtención de secuencias del mayor segmento de IDR (revisar)----

#filtrar los ATG en el archivo de rangos para obtener los FTs 
#solamente
rangos[grep("AT1G01010.1", rangos$ATG),]

rangos_TF <- data.frame(ATG = "", BEGIN = "", END = "")

for (a in df1_TFs$ATG_TF) {
  for (b in grep(a, rangos$ATG)) {
    rangos_TF[nrow(rangos_TF) + 1,] <- rangos[b,]
  }
}

#Eliminar la primer columna vacía
rangos_TF <- rangos_TF[-1,]

idr <- data.frame(ATG = rangos_TF$ATG, IDR = "")
idr <- data.frame(IDR = "")

for (i in rangos_TF$ATG) { #identificadores en i
  for (j in grep(i, names(seqAA))){ #vector de posiciones en j
    for (k in seqAA[j][[1]]) { #secuencias completas en k
      for (l in 1:nrow(rangos_TF)) {
        idr[nrow(idr) + 1,] <- paste0(k[rangos_TF[l, "BEGIN"]:rangos_TF[l, "END"]], collapse = "") #temporal
      }
    }
  }
}

View(idr)
View(rangos_TF)

paste0(k[rangos_TF[l, "BEGIN"]:rangos_TF[l,"END"]], collapse = "")


rangos_test <- rangos_TF[1:15, ]

idr_test <- data.frame(IDR = "")
for (i in rangos_test$ATG) { #identificadores en i
  for (j in grep(i, names(seqAA))){ #vector de posiciones en j
    for (k in seqAA[j][[1]]) { #secuencias completas en k
      for (l in 1:nrow(rangos_test)) {
        idr_test[nrow(idr_test) + 1,] <- paste0(k[rangos_test[l, "BEGIN"]:rangos_test[l, "END"]], collapse = "") #temporal
      }
    }
  }
}

idr_test <- data.frame(IDR = "")
for (x in rangos_test$ATG) {
  for (y in nrow(rangos_test)) {
    idr_test[nrow(idr_test) + 1,] <- paste0(seqAA[grep(x, names(seqAA))][[1]][rangos_test[y, "BEGIN"]:rangos_test[y, "END"]], 
                                            collapse = "")
  }
}

seqAA[grep("AT1G01030.1", names(seqAA))][[1]][rangos_test[9, "BEGIN"]:rangos_test[9, "END"]]
View(idr_test)

View(rangos_test)
