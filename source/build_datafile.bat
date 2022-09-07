@echo off
if exist "LittleTrain.data" del LittleTrain.data
mad4 LittleTrain.data ..\data *.* -1 -nolog -n
pause
