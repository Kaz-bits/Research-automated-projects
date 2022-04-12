#NOTA IMPORTANTE----

#Las variables que lleven en su nombre la palabra "temp" son 
#temporales, por lo que deben eliminarse para evitar tener
#cargado de variables que ocupen espacio en el ambiente de Rstudio


#Paqueterías----
library(Peptides)
library(seqinr)
library(tidyverse)

#Cargar archivo fasta----

seqAA <- read.fasta(file = "DATA/DATA FOR R/TAIR_bien.txt", seqtype = "AA", 
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



#Determinacion de composicion por cada residuo invidual (bien)----

#Crear un dataframe que tenga como columnas los nombres de cada 
#residuo de aminoácido y nombrarlo (en este caso) como "aa"
aa <- as.data.frame(t(data.frame("ATG" = aaList(), row.names = aaList())))

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

#Verificamos que contiene datos escritos

head(aa, n = 10) #Verificar el dataframe 
nrow(aa) #Comprobar que sean todas las secuencias

#Agregar porcentaje de cada aminoácido individual al dataframe
#llamado "aa"

#Loop para obtener porcentajes. Crear primero un dataframe para
#escribir la información en él
temp <- data.frame(t(data.frame("ATG" = aaList(), row.names = aaList())))

for (a in 1:length(disor_list)) {
  for (b in disor_list[[a]]) {
      
    temp1 <- (as.numeric(aa[a,])*100)/(nchar(b))
    temp[nrow(temp) + 1,] <- temp1
  
  }
}

#Eliminar el primer renglón que contiene valores caracter
temp <- temp[-1,]

#Cambiar nombres del datframe del porcentaje de aminoácidos para
#no confundir con las demás columnas

names <- c() #vector vacío

for (a in aaList()) {
  for (b in (gsub(x = (paste(a,"_percent_disor")), replacement = "",
                  pattern = " "))) {
  
  names[length(names) + 1] <- b   
  colnames(temp) <- names
  
  }
}


#Generación de base de datos completa de aminoácidos----

#Juntar ambos dataframes para tener el dataframe final. Se deben
#ordenar los datos para que coincidan los ATG al juntar los dataframes

#Juntar dataframes anteriores: df_n, aa y temp en dicho orden
df_temp <- data.frame(df_n, aa, temp)

#Ordenar dataframe unido anteriormente: df_temp
df_temp <- df_temp[order(df_temp$ATG),]

#Unir df1 con df_temp para generar la base de datos completa
df_proteome <- data.frame(df1, df_temp)

#Verificamos que los ATG coincidan entre los dataframes
df_proteome[1:10,c(1,10)]#primero 10
df_proteome[1000:1020,c(1,10)]

#Eliminamos la columna 10 y obtenemos la base de datos
df_proteome <- df_proteome[,-10]


#Modificaciones adicionales a la base de datos completa----

#Convertir cada columna a la clase de objeto que le corresponde
#ya sea caracter, integer, double, etc.

#Verificar la clase de cada columna

for (a in 1:length(df_proteome)) {
  print(class(df_proteome[1,a]))
}

#Muchas columnas tiene clases distintas, por lo que deben
#ser cambiadas para evitar errores futuros

df_proteome_temp <- df_proteome #hacer copia de la base de datos

#Código para obtener el valor numérico de la columna 3 y 4

temp_vec <- c() #crear vector vacío

for (a in 1:nrow(df_proteome)) {
  for (b in df_proteome[,3][[a]][[1]]) {
    
    temp_vec[length(temp_vec) + 1] <- b
    
  }
}

#Añadir el vector temp_vec a la colunma 3 de la base de datos
#completa

df_proteome$total_disordered_residues <- temp_vec

#Realizar lo mismo para la columna 4 de la base de datos

temp_vec <- c() #crear vector vacío

for (a in 1:nrow(df_proteome)) {
  for (b in df_proteome[,4][[a]][[1]]) {
    
    temp_vec[length(temp_vec) + 1] <- b
    
  }
}

#Añadir el vector temp_vec a la colunma 4 de la base de datos
#completa

df_proteome$total_ordered_residues <- temp_vec


#Las columnas 5 y 6 tiene también formato de lista, por lo que 
#deben cambiarse a tipo integer

temp_vec <- c() #usar vector vacío anterior

for (a in 1:nrow(df_proteome)) {
  for (b in df_proteome[,5][[a]]) {
    
    temp_vec[length(temp_vec) + 1] <- b
    
  }
}


#Añadir el vector temp_vec a la columan 5 de la base de datos
#completa

df_proteome$total_disordered_segments <- temp_vec

#Aplicar lo mismo para la columna 6 de la base de datos


temp_vec <- c() #usar vector vacío anterior

for (a in 1:nrow(df_proteome)) {
  for (b in df_proteome[,6][[a]]) {
    
    temp_vec[length(temp_vec) + 1] <- b
    
  }
}


#Añadir el vector temp_vec a la columan 6 de la base de datos
#completa

df_proteome$total_ordered_segments <- temp_vec


#Las columnas 8 y 9 son de tipo caracter, por lo que deben ser
#cambiadas a tipo double (numérico)

df_proteome$Porcentaje_desordenado <- as.double(df_proteome[,8])
df_proteome$Porcentaje_ordenado <- as.double(df_proteome[,9])

#Como existen valor NA en las columnas 8 y 9, se deben cambiar a
#ceros para evitar sesgos en los gráficos

df_proteome[is.na(df_proteome[,8]),8] <- 0
df_proteome[is.na(df_proteome[,9]),9] <- 0

#Ahora, a partir de la columna 36 hasta la 75, todo es de tipo
#caracter, sin embargo, de la 36 a la 55 debe ser tipo integer y
#de la 56 a la 75 de tipo doble (numérico)

temp_vec <- lapply(df_proteome[,36:55], as.integer)

for (a in 36:55) {
  for (b in length(a:55)) {
    
    df_proteome[,a] <- temp_vec[[21-b]]
    
  }
}


#Una vez realizado el paso anterior, las columnas 56 a 75
#deben ser transformadas a valor numérico tipo double

temp_vec <- lapply(df_proteome[,56:75], as.double)

for (a in 56:75) {
  for (b in length(a:75)) {
    
    df_proteome[,a] <- temp_vec[[21-b]]
    
  }
}


#Añadir secuencias de la IDR más larga a la base de datos----

#Para ello, se reciclará el código del apartado de 
#extracción de residuos de IDR

disor_list1 <- c() #lista vacia

for (a in 1:length(seqAA)) {
  disor_vector <- c()
  temp1 <- rangos %>% filter(ATG == ATG_unique[a])
  
  for (b in 1:nrow(temp1)) {
    temp2 <- paste0(seqAA[[a]][as.numeric(temp1[b, "BEGIN"]):as.numeric(temp1[b, "END"])], collapse = "")
    disor_vector[b] <- temp2
  }
  
    disor_list1[[length(disor_list1) + 1]] <- disor_vector
    
}
  

#Después del loop anterior, obtener las secuencias por ATG y extraer la 
#de mayor longitud 

temp_IDR <- c() #vector vacio

for (a in 1:length(disor_list1)) {
  temp_IDR[length(temp_IDR) + 1] <- disor_list1[[a]][(order(nchar(disor_list1[[a]]), 
                                                            na.last = T, 
                                                            decreasing = T))][1]
}

#Guardar IDRs en un archivo

write.table(x = temp_IDR, file = "DATA/DATA FOR R/DATA FOR ANALYSIS//IDRs_proteome.txt", 
            quote = F, row.names = F)

#Colocar las IDRs en una columna en el dataframe llamado "df_proteome". 
#Para ello, las secuencias llevan un orden según el AGI del vector
#ATG_unique, por lo que se debe construir un dataframe con los AGIs de
#dicho vector y las IDRs del archivo temp_IDR

df_temp_IDR <- data.frame("AGI" = ATG_unique, "IDR" = temp_IDR)

#El dataframe df_tem_IDR debe ordenarse con base en el AGI, para poder
#añadir la columna de las IDRs a la base de datos completa

df_temp_IDR <- df_temp_IDR[order(df_temp_IDR[,1]),]

#Ahora, el AGI tiene el orden correcto comparado con la base de datos
#completa df_proteome, por lo que se debe añadir la columna de IDR a
#dicha base de datos

df_proteome$IDR <- df_temp_IDR[,2]
df_proteome <- df_proteome[,c(1,2,76,3:75)]

#Añadir secuencias completas a la base de datos----

temp_vec <- c() #vector vacio

for (a in 1:length(seqAA)) {
  temp_vec[length(temp_vec) + 1] <- paste(seqAA[a][[1]], collapse = "")
  
}

#Generar un dataframe con los AGI de ATG_unique para ordenar los datos
#de las secuencias

df_temp_seq <- data.frame("AGI" = ATG_unique, 
                          "Sequence" = temp_vec)

#Ordenar dataframe anterior

df_temp_seq <- df_temp_seq[order(df_temp_seq[,1]),]

#Añadir columna de las secuencias a la base de datos completa

df_proteome$sequence <- df_temp_seq[,2]
df_proteome <- df_proteome[,c(1,2,77,3:76)]

#Comprobamos que la longitud corresponde con la secuencia añadida

nchar(df_proteome_temp[1,3])

#Cambiar la columna de longitud a tipo integer para obtener la base de
#datos completa y finalizada

df_proteome$longitud <- as.integer(df_proteome[,2])

#Añadir la longitud de la IDR más larga para su uso posterior en PONDR

df1_TFs$longitud_IDR <- nchar(df1_TFs$IDR)

#Cambiamos de posición para la longitud de la IDR

df1_TFs <- df1_TFs[,c(1,2,3,79,4:78)]

#Guardar base de datos en un archivo .csv
write.csv(x = df_proteome, file = "DATA/DATA FOR R/DATA FOR ANALYSIS/df_proteome.csv")


#Obtención de IDRs de los factores----

#Empleando la variable ATGs_TFs donde se encuentran los AGI de los
#factores, extraemos los datos de la base de datos

#Verificar cuáles ATGs se encuentran en la base de datos respecto a
#a la de los factores

df_proteome[,1] %in% ATGs_TFs[,1]

#Emplear el vector lógios para extraer los renglones respectivos

df1_TFs <- df_proteome[(df_proteome[,1] %in% ATGs_TFs[,1]),]

#Extraer todas las IDRs de la base de datos creada anteriormente
#y guardarlas en un archivo

IDRs_TFs <- df1_TFs[4]

#Para que AGADIR pueda analizar las 1958 secuencias, se dividen las IDRs
#en tres archivos, que contengan a: 653, b:653 y c:652

#Para a
write.table(x = IDRs_TFs[1:653,], file = "DATA/DATA FOR R/DATA FOR ANALYSIS/IDRs_TFs", 
            row.names = F, quote = F)
#Para b
write.table(x = IDRs_TFs[654:1306,], file = "DATA/DATA FOR R/DATA FOR ANALYSIS/IDRs_TFs", 
            row.names = F, quote = F)
#Para c
write.table(x = IDRs_TFs[1307:1958,], file = "DATA/DATA FOR R/DATA FOR ANALYSIS/IDRs_TFs", 
            row.names = F, quote = F)


#Una vez hecho el análisis, cargar archivos de agadir y combinarlos
#para obtener el porcentaje para cada secuencia de forma completa

AGADIR1 <- read.delim("DATA/DATA FOR R/AGADIR1.txt", header=FALSE, comment.char="#")
AGADIR2 <- read.delim("DATA/DATA FOR R/AGADIR2.txt", header=FALSE, comment.char="#")
AGADIR3 <- read.delim("DATA/DATA FOR R/AGADIR3.txt", header=FALSE, comment.char="#")

#Combinar todos los porcentajes de AGADIR en un solo dataframe

AGADIR <- c(AGADIR1[,3], AGADIR2[,3], AGADIR3[,3])

#Un factor no cuenta con IDRs, por lo que se eliminará de la base de
#datos

df1_TFs <- df1_TFs[-17,]

#Añadir a la base de datos df1_TFs la columna de alfa hélice (AGADIR)

df1_TFs$alfa_score <- AGADIR
df1_TFs <- df1_TFs[,c(1:4,78,5:77)]

#Guardar archivo completo en un formato txt

write.table(x = IDRs_TFs, file = "DATA/DATA FOR R/DATA FOR ANALYSIS/IDRs_TFs", 
            row.names = F, quote = F)

#Obtención de alfa score de todo el proteoma----

write.table(x = temp_IDR[1:6853], file = "DATA/DATA FOR R/DATA FOR ANALYSIS//IDRs_protome.txt", 
            quote = F, row.names = F)

write.table(x = temp_IDR[6854:13706], file = "DATA/DATA FOR R/DATA FOR ANALYSIS//IDRs_proteome.txt", 
            quote = F, row.names = F)

write.table(x = temp_IDR[13707:20559], file = "DATA/DATA FOR R/DATA FOR ANALYSIS//IDRs_proteome.txt", 
            quote = F, row.names = F)

write.table(x = temp_IDR[20560:27411], file = "DATA/DATA FOR R/DATA FOR ANALYSIS//IDRs_proteome.txt", 
            quote = F, row.names = F)

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



