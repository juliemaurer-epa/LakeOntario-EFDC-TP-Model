#!/bin/csh

sed -e 's/,/ /g' -e 's/\// /g' Black_River_Loads.csv > Black_River_2013.txt

sed -e 's/,/ /g' -e 's/\// /g' Trent_River_Loads.csv > Trent_River_2013.txt

sed -e 's/,/ /g' -e 's/\// /g' Humber_River_Loads.csv > Humber_River_2013.txt

sed -e 's/,/ /g' -e 's/\// /g' Twelve_Mile_Creek_River_Loads.csv > Twelve_Mile_Creek_River_2013.txt


exit
