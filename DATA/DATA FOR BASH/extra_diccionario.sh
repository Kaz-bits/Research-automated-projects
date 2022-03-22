#!/bin/bash

#Generar un archivo diccionario a partir de genomes.protein.gz usando
#el archivo TAIR10, empleando el modelo genético 1. 

#---------------------------------------------------
dic=genomes.protein.gz
TAIR=TAIR10_pep_20110103_representative_gene_model
vlxt=vlxt.disrange.gz
#---------------------------------------------------



#PARTE 1: EXTRACCIÓN DE MODELOS GENÉTICO DE TAIR10
#========================================================================
#EXTRAER TODOS LOS IDENTIFICADORES CON MODELO GENÉTICO 1 DE TAIR10

#echo $(grep -E ">AT.G[0-9]{5}.1" $TAIR -o | sponge AT_1_TAIR10.txt)
#echo $(grep -E ">AT.G[0-9]{5}.1" $TAIR -o | wc -l) #HAY 25362 IDENTIFICADORES PARA MODELO GENÉTICO 1 EN TAIR10

#=========================================================================



#PARTE 2: EXTRACCIÓN DE MODELOS GENÉTICOS DE GENOMES
#=========================================================================
#EXTRAER TODOS LOS IDENTIFICADORES DE GENOMES.PROTEIN.GZ DE ARABIDOPSIS UNICAMENTE

#echo $(zgrep "^at" $dic | sponge genome_codes_protein.txt) #LO DE ARABIDOPSIS. SON 35381 
#echo $(grep -E "AT.G[0-9]{5}.1" genome_codes_protein.txt | sponge genomes_codes_at1.txt) #EXTRAER TODOS CON MODELO GENÉTICO 1 
#echo $(grep -E "AT.G[0-9]{5}.1" genome_codes_protein.txt | wc -l) #HAY 27378 MODELO GENÉTICO 1

#=========================================================================



#PARTE 3: COMPARACIÓN ENTRE GENOMES (27378) VS VLXT
#=========================================================================
#COMPARAR AMBOS ARCHIVOS PARA SABER CUÁNTOS ID HAY EN VLXT PARA GENOMES

#echo $(cut -f3 genomes_codes_at1.txt | sponge at1_vlxt_id.txt) #EXTRAER LOS CÓDIGOS QUE USA VLXT PUESTOS EN GENOMES
#echo $(zcat $vlxt | cut -f1 | zgrep -xf at1_vlxt_id.txt | sponge vlxt_codes_for_GENOMES.txt) #OBTENER AQUELLOS QUE COINCIDEN

#CONTAR CUANTOS ID DE VLXT HAY PARA EL MODELO GENETICO

#x=$(uniq -d vlxt_codes_for_GENOMES.txt | wc -l) #PARA OBTENER UN ID SOLO QUE SE REPITE EN EL ARCHIVO
#y=$(uniq -u vlxt_codes_for_GENOMES.txt | wc -l) #PARA OBTENER UN ID QUE NO SE REPITE
#echo "$x + $y" | bc #LOS CONTAMOS. HAY 26951 EN VLXT QUE COINCIDEN CON GENOMES 

#==========================================================================



#PARTE 4: COMPARACIÓN ENTRE TAIR (25362) Y GENOMES (27378)
#=========================================================================

#i=$(sed "s/^>//" AT_1_TAIR10.txt | sponge AT_1_TAIR10_final.txt)
#echo $i
#echo $(zgrep -f AT_1_TAIR10_final.txt genomes_codes_at1.txt | sponge genomes_ATGs_for_TAIR10.txt) #LOS QUE COINCIDEN
#echo $(wc -l genomes_ATGs_for_TAIR10.txt) #HAY 27357

#=========================================================================



#PARTE 5: COMPARACIÓN ENTRE TAIR-GENOMES (25357) VS VLXT
#=========================================================================

#echo $(cut -f3 genomes_ATGs_for_TAIR10.txt | sponge vlxt_TAIR10_id.txt) #OBTENER LOS QUE COINCIDEN
#echo $(zcat $vlxt | cut -f1 | grep -xf vlxt_TAIR10_id.txt | sponge vlxt_codes_for_TAIR10.txt)

#CONTAR CUANTOS HAY

#a=$(uniq -d vlxt_codes_for_TAIR10.txt | wc -l) #PARA OBTENER UN ID SOLO QUE SE REPITE EN EL ARCHIVO
#b=$(uniq -u vlxt_codes_for_TAIR10.txt | wc -l) #PARA OBTENER UN ID QUE NO SE REPITE
#echo "$a + $b" | bc #LOS CONTAMOS. HAY  EN VLXT QUE COINCIDEN CON TAIR10

#=========================================================================











#PARTE 6: CREACIÓN DEL DICCIONARIO
#=========================================================================

#HACEMOS UNA COMPARACIÓN ENTRE TAIR10 Y GENOMES (CON TODOS LOS IDENTIFICADORES)
#echo $(grep -E ">AT.G[0-9]{5}.[0-9]" TAIR_bien.txt -o | sed "s/^>//" | sort | uniq | sponge ATGs_TAIR10_27416.txt)
#echo $(wc -l ATGs_TAIR10_27416.txt) #HAY 27416
#echo $(grep -f ATGs_TAIR10_27416.txt genome_codes_protein.txt | sort | sponge ATGs_for_TAIR10_IDs_27411.txt) #DICCIONARIO
#echo $(wc -l ATGs_for_TAIR10_IDs_27411.txt) #COINCIDEN 27411 

#=========================================================================



#PARTE 7: COMPARACIÓN ENTRE DICCIONARIO (27411) VS VLXT
#==========================================================================

#echo $(cut -f3 ATGs_for_TAIR10_IDs_27411.txt | sponge ATGs_for_TAIR10_IDs_only_27411.txt)
#echo $(zgrep -w -f ATGs_for_TAIR10_IDs_only_27411.txt $vlxt | sort | uniq | sponge IDs_for_TAIR10.txt) #187514 líneas
#echo $(cut -f1 IDs_for_TAIR10.txt | sort -k1 | uniq | wc -l) #26993

#TRADUCCIÓN DE RANGOS A ATGS

file=ID_ranges_by_word.txt


if [[ -f ID_ranges_by_word.txt ]] && [[ -f ATGs_for_TAIR10_IDs_27411.txt ]]
then	
	for ATGs in $(cat ATGs_for_TAIR10_IDs_27411.txt | cut -f2)
	do
		id=$(cut -f3 ATGs_for_TAIR10_IDs_27411.txt); echo $id
	
		if [[ $(grep -q $id ID_ranges_by_word.txt) ]]
		then
			grep "$id" ID_ranges_by_word.txt | sed "s/$id/$ATGs/g" | sponge -a ATG_ranges.txt
		else
			echo -e "$ATGs\t0\t0" | sponge -a ATG_ranges.txt
		fi
	done
else
	echo "Archivo no localizado"
fi






#echo $(grep $x ATGs_for_TAIR10_IDs_27411.txt -v | sponge IDs.txt)
#echo $(grep -f $IDs ID_ranges_by_word.txt | cut -f1-3 | sponge rangos.txt)
ID_ATG=$(sponge -a $IDs $rangos)
#echo $ID_ATG






#----------------------------------------------------------------------------------------------------
#LO SIGUIENTES COMANDOS SON DE PRUEBA, AL PARECER NO SON CONSISTENTES, NO EMPLEAR

#echo $(zgrep -w -f ATGs_for_TAIR10_IDs_only_27411.txt $vlxt | sponge ID_ranges_by_word.txt) 

#LAS SIGUIENTES INSTRUCCIONES BUSCAN "DEPURAR" O ELIMINAR AQUELLOS IDS QUE NO CORRESPONDEN A ARABIDOPSIS

#echo $(cat IDs_for_TAIR10.txt | cut -f1 | uniq -u | sort | sponge IDs_from_vlxt.txt) 
#echo $(cat ATGs_for_TAIR10_IDs_27411.txt | sponge IDs_from_genomes_27411.txt)
#echo $(grep -Ff IDs_from_vlxt.txt IDs_from_genomes_27411.txt | sponge IDs_no_matched_in_27411_u.txt)	

#echo $(cat IDs_for_TAIR10.txt | cut -f1 | uniq -d | sort | sponge IDs_from_vlxt.txt)
#echo $(cat ATGs_for_TAIR10_IDs_27411.txt | sponge IDs_from_genomes_27411.txt)
#echo $(grep -Ff IDs_from_vlxt.txt IDs_from_genomes_27411.txt | sponge IDs_no_matched_in_27411_d.txt)

#EL ARCHIVO FINAL QUEDA:

#echo $(cat IDs_no_matched_in_27411_u.txt IDs_no_matched_in_27411_d.txt > IDs_for_TAIR10_debugged.txt)
#-----------------------------------------------------------------------------------------------------

#==========================================================================





#ELIMINAR ARCHIVOS UNA VEZ HECHO EL ANÁLISIS
#===============================================

#---------------------------------------------------------------
#CONSERVAR LOS SIGUIENTES ARCHIVOS:

#genome_codes_protein.txt
#ATGs_TAIR10_27416.txt
#ATGs_for_TAIR10_IDs_27411.txt
#IDs_for_TAIR10_debugged.txt

#---------------------------------------------------------------




#BORRAR LOS SIGUIENTES ARCHIVOS UNA VEZ EJECUTADO EL PROGRAMA
#---------------------------------------------------------------
#rm AT_1_TAIR10.txt
#rm genomes_codes_at1.txt
#rm at1_vlxt_id.txt
#rm vlxt_codes_for_GENOMES.txt
#rm AT_1_TAIR10_final.txt
#rm genomes_ATGs_for_TAIR10.txt
#rm vlxt_TAIR10_id.txt
#rm vlxt_codes_for_TAIR10.txt

#A PARTIR DE AQUÍ SON LOS ARCHIVOS DESDE EL PASO 6

#rm ATGs_for_TAIR10_IDs_only_27411.txt
#rm vlxt_ATGs_codes_for_TAIR10.txt
#--------------------------------------------------------------


#TRADUCCIÓN DE RANGOS A ATGS

#USAMOS EL ARCHIVO LLAMADO 





















#================================================
