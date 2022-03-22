#!/bin/bash


for i in $(grep "5403860" ATGs_for_TAIR10_IDs_27411.txt | cut -f2)
do
	echo $(grep $i Araport11_unilinea.txt -A1)
done
