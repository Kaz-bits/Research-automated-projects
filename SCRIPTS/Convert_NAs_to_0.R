
#Tranformar cada columna que tenga NAs a "ceros", uno por una, ya que
#cada columna es una clase diferente

#Para comprobrar la clase de cada columna
for (i in 3:9) {
  print(class(df_proteome[1,i]))
}


#Para transformar los NAs de cada columna según su clase
df_proteome[(df_proteome[,3] == " NA"), 3] <- 0
df_proteome[(df_proteome[,4] == " NA"), 4] <- 0
df_proteome[is.na(df_proteome[,7]),7] <- 0
df_proteome[is.na(df_proteome[,8]),8] <- 0
df_proteome[is.na(df_proteome[,9]),9] <- 0 



