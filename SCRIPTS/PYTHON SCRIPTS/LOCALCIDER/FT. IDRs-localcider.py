#Modulos requeridos----

import os
import protfasta #Instalar
import pandas as pd
from Bio import SeqIO #Instalar
from localcider.sequenceParameters import SequenceParameters #Instalar

#Obtener el archivo fasta y dividirlo en archivos individuales por
#cada secuencia que tenga el archivo. 

#Antes de iniciar, cambiar el directorio actual de trabajo de
#Python en donde estarán los archivos fasta

os.chdir('DATA/DATA FOR PYTHON/FASTA_CIDER/IDRs_99/INDIVIDUAL_99/') #Nuevo dir

#Realizar una limpieza del archivo fasta para eliminar aminoácidos
#inválidos usando el modulo de "protfasta"

import protfasta

#Remover los aminácidos inválidos

temp_fasta = protfasta.read_fasta('DATA/DATA FOR PYTHON/FASTA_CIDER/IDRs_99/IDRs_99p_FASTA.fasta', 
invalid_sequence_action='remove')

#Guardar archivo fasta limpiado

protfasta.write_fasta(temp_fasta, 
'DATA/DATA FOR PYTHON/FASTA_CIDER/IDRs_99/IDRs_99p_FASTAc.fasta', 
linelength=200)

#Crear una lista con cada uno de los archivos individuales de las 
#secuencias

filelist = os.listdir(".")

#Crear una lista vacía

list_of_IDRs = []

#Poblar la lista vacía con los parámetros de CIDER para cada uno de los
#archivos individuales que contienen las secuencias

#Para obtener el valor de kappa

temp_kappa = [] #Lista temporal vacía
for file in filelist:
    temp_kappa.append((SequenceParameters(sequenceFile=file)).get_kappa())
  
#Para obtener el valor de hidropatía

temp_hidropatia = [] #Lista temporal vacía
for file in filelist:
   temp_hidropatia.append((SequenceParameters(sequenceFile=file)).get_uversky_hydropathy())
  
#Para obtener el valor de carga neta media

temp_carganeta = [] #Lista temporal vacía
for file in filelist:
   temp_carganeta.append((SequenceParameters(sequenceFile=file)).get_mean_net_charge())
  
#Para obtener el valor de desorden promovido

temp_desorden = [] #Lista temporal vacía
for file in filelist:
   temp_desorden.append((SequenceParameters(sequenceFile=file)).get_fraction_disorder_promoting())
  
#Para obtener el valor de FCR (constante para todas las secuencias)

temp_FCR = [] #Lista temporal vacía
for file in filelist:
   temp_FCR.append((SequenceParameters(sequenceFile=file)).get_FCR())
  

#Juantar todas las listas en un dataframe utilizadno el modulo de
#Pandas

#Crear una lista de listas con las variables generadas

CIDER_lists = temp_FCR, temp_kappa, temp_hidropatia, temp_carganeta, temp_desorden


#Crear el dataframe con el modulo de Pandas y transponer las columnas
#usando la función "T"

df_CIDER = pd.DataFrame(CIDER_lists, 
index = ['FCR', 'kappa', 'hydropathy', 'charge', 'disorder']).T

#Exportar dataframe a un archivo de txt para ser usado en R

os.chdir('../') #Retroceder directorios


#Guardar archivo en el directorio de elección

df_CIDER.to_csv('DATA/DATA FOR PYTHON/FASTA_CIDER/IDRs_99/CIDER_values.txt')
