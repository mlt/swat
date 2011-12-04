REM combine 4 -> 1
REM left/middle/right -> 1/2/3

bin\awk "NR==FNR{a[FNR]=$0;next} {print a[FNR],$0}" tmp\%1.1 tmp\%1.flocnst.csv > tmp\%1.1_f
bin\awk "NR==FNR{a[FNR]=$0;next} {print a[FNR],$0}" tmp\%1.1_f tmp\%1.2 > tmp\%1.1_2
bin\awk "NR==FNR{a[FNR]=$0;next} {print a[FNR],$0}" tmp\%1.1_2 tmp\%1.nh3cnst.csv > tmp\%1.1_n
bin\awk "NR==FNR{a[FNR]=$0;next} {print a[FNR],$0}" tmp\%1.1_n tmp\%1.3 > tmp\%1.1_3
bin\awk "NR==FNR{a[FNR]=$0;next} {print a[FNR],$0}" tmp\%1.1_3 tmp\%1.cbodcnst.csv > tmp\%1.1_c
bin\awk "NR==FNR{a[FNR]=$0;next} {print a[FNR],$0}" tmp\%1.1_c tmp\%1.4 > tmp\%1.1_4
bin\cat tmp\%1.head tmp\%1.1_4 > out\%1
