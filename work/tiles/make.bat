cd new
mkconv2 scripts1.mc2
cd ..
src\tilebuild sprites.txt
for %%i in (*.png) do pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
