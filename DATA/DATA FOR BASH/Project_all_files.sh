#!bin/bash

#TODOS LOS ARCHIVOS QUE SE HAN CREADO PARA LA CARACTERIZACIÓN DEL PROYECTO DE TF SE LOCALIZAN EN ESTE PROGRAMA


#---------------------------------------------------------------------------------------------------------------------------------------
#ESTE PRIMER PROGRAMA PERMITE OBTENER LAS BASES DE DATOS DE D2P2, TAIR10 Y ARCHIVOS DE ANÁLISIS POSTERIORES.


#PARTE 1: CANDADOS
#===============================================================================

#-------------------------------------
TAIR=TAIR10_pep_20110103_representative_gene_model
dic=genomes.protein.gz
vlxt=vlxt.disrange.gz
#-------------------------------------


#Archivo 1: fasta de TAIR10
if [[ ! -f $TAIR ]]
then
        echo $(wget https://www.arabidopsis.org/download_files/Proteins/TAIR10_protein_lists/TAIR10_pep_20110103_representative_gene_model)
else
        echo -e "\nArchivo $TAIR encontrado"
fi

#Archivo 2: diccionario (D2P2)
if [[ ! -f $dic ]]
then
	echo $(wget http://d2p2.pro/downloads/genomes.protein.gz)
else
	echo "Archivo $dic encontrado"
fi

#Archivo 3: predictor (D2P2)
if [[ ! -f $vlxt ]]
then
        echo $(wget http://d2p2.pro/downloads/vlxt.disrange.gz)
else
        echo "Archivo $vlxt encontrado"
fi
#===============================================================================

#===============================================================================






#PARTE 2:IDENTIFICADORES (ATG, ATM Y ATC)
#===============================================================================


#--------------------------------
chr_G=$(zgrep "AT[0-5]G" $TAIR)
chr_M=$(zgrep ">ATM" $TAIR)
chr_C=$(zgrep ">ATC" $TAIR)
#--------------------------------


#Código para verificar si existen identificadores para los
#cinco cromosomas, para mitocondria y cloroplasto

if [[ $chr_G =~ AT[0-5]G ]]
then
        total=$(zgrep "AT[0-5]G" $TAIR | wc -l)
        echo -e "\nExisten códigos para los cinco cromosomas en Arabidopsis,"
        echo -e "con un total de $total identificadores.\n"
        for n_chr in AT1G AT2G AT3G AT4G AT5G
        do
                for c_chr in $(zgrep $n_chr $TAIR -o | wc -l)
                do
                                echo -e "Divididos en $c_chr"
                done
        done
fi


#Para mitocondria y cloroplasto tenemos el siguiente ccódigo

if [[ $chr_M =~ ATM ]] && [[ $chr_C =~ ATC ]]
then
        total_M=$(zgrep ">ATM" $TAIR -o | wc -l)
        total_C=$(zgrep ">ATC" $TAIR -o | wc -l)
        echo -e "\nExisten $total_M códigos para mitocondria, y"
        echo -e "$total_C códigos para cloroplasto.\n"
        total=$(echo $total + $total_M + $total_C | bc)
        echo -e "En total, suman $total identificadores.\n"
fi

#===============================================================================





#PARTE 3:ARCHIVOS
#===============================================================================

###Códigos de TAIR que no están en D2P2
ta=$(zgrep -Eo "AT.G[0-9]{5}" TAIR10_pep_20110103_representative_gene_model | sort | uniq | sponge tair_fil.txt)
at=$(zgrep "^at" genomes.protein.gz | grep -Eo "AT.G[0-9]{5}" | sort | uniq | sponge codes_for_tair.txt)

echo $(grep -v -f codes_for_tair.txt tair_fil.txt | sponge codes_no_tair.txt)

#for i in $(codes_no_tair.txt)
#do
#  echo "El código $i no se encuentra en la base de datos de D2P2, por lo tanto, se trabajará sin él"
#done

###Archivo 1: Unilinea
echo $(seqkit seq TAIR10_pep_20110103_representative_gene_model -w 0 | sponge TAIR_unilinea.txt)

###Archivo de TAIR sin los códigos faltantes en d2p2
echo $(grep -A1 -f codes_no_tair.txt TAIR_unilinea.txt -v | sponge TAIR_bien.txt)

###Archivo 2: ATG/longitud del CDS
echo $(grep -Eo "AT.G[0-9].*" TAIR_bien.txt | sed "s/\..*=/\t/g" | sponge ATG_LONG.txt)

###Archivo 3:Fasta temporal  header/secuencia con O
ara=$(grep -Eo "AT.G[0-9]{5}" TAIR_bien.txt)


for i in $ara
do
        LONG=$(grep "^>$i" TAIR_bien.txt | sed "s/^>.*LENGTH=//")
        SEC=$(seq -sO $(( $LONG+1 )) | tr -d '[:digit:]')
        HED=$(grep "^>$i" TAIR_bien.txt| grep ">")
        echo -e $HED\n$SEC >> SEC_O.txt
done

#===============================================================================

#---------------------------------------------------------------------------------------------------------------------------

#TODOS LOS ARCHIVOS QUE SE GENEARON PARA EL FILTRADO DE DATOS DE TAIR10, GENOMES Y VLXT ESTAN EN ESTA PARTE

echo $(grep -Eo ">AT.G[0-9]{5}\..{2}" TAIR_bien.txt | sed "s/>//" | sed "s/ //" | sponge ATG_MG_TAIR_bien.txt)
echo $(grep ">at" genomes.protein.gz | sponge at.txt)
echo $(



rm ATG_MG_TAIR_bien.txt
~
~
~
~
~
~
