@echo off
echo Creating backup...
"c:\program files\winrar\winrar" a LittleTrain -r
timestamp LittleTrain.rar /d
move *.rar ..\..\Backups

