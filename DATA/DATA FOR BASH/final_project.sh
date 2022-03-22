#!/bin/bash



#PROGRAMA_FINAL

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






#PARTE 2:IDENTIFICADORES (ATG, ATM Y ATC)
#===============================================================================


#--------------------------------
chr_G=$(zgrep "AT[0-5]G" $TAIR)
chr_M=$(zgrep ">ATM" $TAIR)
chr_C=$(zgrep ">ATC" $TAIR)

chr=$(if [[ n_chr =~ AT1G ]] || [[ n_chr =~ AT2G ]] || [[ n_chr =~ AT3G ]] || [[ n_chr =~ AT4G ]] || [[ n_chr =~ AT5G ]] 
then
	for x in 
	echo "1"
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
				echo -e "$c_chr para el cromosoma "
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
