Split fixed width input resource file into several small files vertically.
So we can modify the data in Excel or other tools conveniently.

original file -> head/tail
tail-> 1 / flo / 2 / nh3 / 3 / cbo / 4

01splitall.bat -> do split, copy csv files to csv folder
...here we modify the csv files...
02combineall.bat -> do combination, copy combined dat file to out folder

awk/sed/grep is used here.