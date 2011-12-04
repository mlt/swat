REM split file into 4
REM top 6 lines -> head
REM bottom -> tail
REM left/middle/right -> 1/2/3

bin\head -n +6 %1 > tmp\%1.head
bin\tail -n +7 %1 > tmp\%1.tail

bin\grep -o "^.\{11\}" tmp\%1.tail > tmp\%1.1
bin\grep -o "^.\{28\}" tmp\%1.tail | bin\sed "s/^.\{12\}//g" > tmp\%1.flocnst.csv
bin\grep -o "^.\{96\}" tmp\%1.tail | bin\sed "s/^.\{29\}//g" > tmp\%1.2
bin\grep -o "^.\{113\}" tmp\%1.tail | bin\sed "s/^.\{97\}//g" > tmp\%1.nh3cnst.csv
bin\grep -o "^.\{147\}" tmp\%1.tail | bin\sed "s/^.\{114\}//g" > tmp\%1.3
bin\grep -o "^.\{164\}" tmp\%1.tail | bin\sed "s/^.\{148\}//g" > tmp\%1.cbodcnst.csv
bin\grep -o ".\{152\}$" tmp\%1.tail > tmp\%1.4

