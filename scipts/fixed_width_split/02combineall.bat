@echo off
@copy csv\*.csv tmp /y
if not exist out md out
for %%f in (*.dat) do call _combine %%f
