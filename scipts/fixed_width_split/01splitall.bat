@echo off
if not exist tmp md tmp
for %%f in (*.dat) do call _split %%f
if not exist csv md csv
copy tmp\*.csv csv /y
