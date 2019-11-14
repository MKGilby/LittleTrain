@echo off
echo Creating backup...
"c:\program files\winrar\winrar" a FNS_AAT.rar -r
timestamp FNS_AAT.rar /d
move *.rar ..\_Backups

