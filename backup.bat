@echo off
echo Creating backup...
"c:\program files\winrar\winrar" a LittleTrain -r
rem "c:\program files\winrar\winrar" d LittleTrain *.a *.o *.ppu
timestamp LittleTrain.rar /d
move *.rar ..\..\Backups

