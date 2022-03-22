#!/bin/bash

#ESTA ES UNA PRUEBA PARA CORRER EL PROGRAMA PRINCIPAL EN get_data.sh


#-------------Variables 1---------------

dic=genomes.protein.gz
vlxt=vlxt.disrange.gz
fasta=Araport11_cds_20160703_representative_gene_model.gz
TAIR=TAIR10_pep_20110103_representative_gene_model

#-------------------------------------



#Hagamos que nuestro programa descargue los archivos necesarios en caso de que no 
#se encuentren en el sitio de ejecución del programa

#Para los archivos necesarios. Estos son los candados

#=============================================================

#Archivo 1
#if [[ ! -f $fasta ]]
#then
#	echo $(wget https://www.arabidopsis.org/download_files/Genes/Araport11_genome_release/Araport11_blastsets/Araport11_cds_20160703_representative_gene_model.gz)
#else
#	echo "Archivo $fasta encontrado"
#fi

#Archivo 2
#if [[ ! -f $dic ]]
#then
#		echo $(wget http://d2p2.pro/downloads/genomes.protein.gz)
#else
#		echo "Archivo $dic encontrado"
#fi

#Archivo 3
#if [[ ! -f $vlxt ]]
#then
#	echo $(wget http://d2p2.pro/downloads/vlxt.disrange.gz)
#else
#	echo "Archivo $vlxt encontrado"
#fi

#=============================================================



#------------Variables 2-------------

chr_G=$(zgrep "AT[0-5]G" $fasta)
chr_M=$(zgrep ">ATM" $fasta)
chr_C=$(zgrep ">ATC" $fasta)


#---------------------------------------------------------------------------

#Código para verificar si existen identificadores para los
#cinco cromosomas y para mitocondria y cloroplasto

#if [[ $chr_G =~ AT[0-5]G ]]
#then
#	total=$(zgrep "AT[0-5]G" $fasta | wc -l)
#	echo -e "\nExisten códigos para los cinco cromosomas en Arabidopsis," 
#	echo -e "con un total de $total identificadores.\n"
#	for n_chr in AT1G AT2G AT3G AT4G AT5G
#	do
#		for c_chr in $(zgrep $n_chr $fasta -o | wc -l)
#		do
#				echo -e "Divididos en $c_chr"
#		
#		done
#	done
#fi



#Para mitocondria y cloroplasto tenemos el siguiente ccódigo

#if [[ $chr_M =~ ATM ]] && [[ $chr_C =~ ATC ]]
#then 
#	total_M=$(zgrep ">ATM" $fasta -o | wc -l)
#	total_C=$(zgrep ">ATC" $fasta -o | wc -l)
#	echo -e "\nExisten $total_M códigos para mitocondria, y"
#	echo -e "$total_C códigos para cloroplasto.\n"
#	total=$(echo $total + $total_M + $total_C | bc)
#	echo -e "En total, suman $total identificadores.\n"
#fi

#-----------------------------------------------------------------------------

#El archivo de genomes.protein.gz viene un código al inicio, el cual indicia
#la especie, hacemos una búsqueda para filtrar los de arabidopsis

#at=$(zgrep -E "AT.G[0-9]{5}.[0-9]" Araport11_cds_20160703_representative_gene_model.gz -o | sponge araport_ID.txt)


#echo $at






#----------------------------------------------------------------------------------------------------------



at=$(grep -Eo "AT.G[0-9]{5}" | sort | uniq | sponge ATGs_de_TAIR10.txt)
codes=$(zgrep "^at" $dic | sponge genome_codes.txt)

#Ejecutar estos comandos paso por paso:


#echo $at 
#echo $codes

#echo $(grep -f ATGs_de_TAIR10.txt genome_codes.txt | sponge codes_for_TAIR10.txt)
#echo $(cut -f3 codes_for_TAIR10.txt | sponge final_codes.txt)


#echo $(zgrep -of final_codes $vlxt | sponge common_matched.txt)
#echo $(grep -vf common_matched.txt final_codes | sponge no_matched_vlxt.txt) 
#echo $(zgrep -f no_matched_vlxt.txt genome_codes.txt | sponge ATGs_no_matched_in_vlxt.txt)


#eliminar archivos creados durante el proceso


#echo "No se encontraron $(zgrep -f no_matched_vlxt.txt genome_codes.txt | wc -l) identificadores en vlxt con el erchivo $fasta"

#echo $(rm codes_for_araport11.txt)
#echo $(rm final_codes)


#-----------------------------------------------------------------------------------------------------------

#Archivo 1: fasta unilinea

#echo $(seqkit seq TAIR10_pep_20110103_representative_gene_model -w 0 | TAIR10_pep_20110103_representative_gene_model)

#Archivo 2:ATG \t longitud del CDS

#echo $(grep "^>" TAIR10_pep_20110103_representative_gene_model -A1 | sed "s/^M.*/O/g")



for seq_1 in $(head -n 6 TAIR10_pep_20110103_representative_gene_model | grep -E "AT.G[0-9]{5}" -o)
do
	O_seq=$(grep $seq_1 TAIR10_pep_20110103_representative_gene_model -A1 | grep -v "^>" | sed "s/[A-Z]/O/g")
	header=$(grep $seq_1 TAIR10_pep_20110103_representative_gene_model | grep ">")
	len=$(grep $seq_1 TAIR10_pep_20110103_representative_gene_model -A1 | grep -v "^>" | sed "s/[A-Z]/O/g" | wc -c)
	echo -e "$header\n$O_seq\n$len"
done







