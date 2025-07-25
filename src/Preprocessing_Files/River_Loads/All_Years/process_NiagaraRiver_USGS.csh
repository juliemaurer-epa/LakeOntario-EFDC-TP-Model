#!/bin/csh

sed -e 's/,/ /g' -e 's/\// /g' Niagara_River_USGS_TP_Loads_2013.csv > Niagara_River_USGS_2013.txt

sed -e 's/,/ /g' -e 's/\// /g' Niagara_River_USGS_TP_Loads_2018.csv > Niagara_River_USGS_2018.txt

exit
