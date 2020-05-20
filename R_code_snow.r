# prima cosa fare sewd
setwd("C:/lab/")
install.packages("ncdf4") # per caricare immagini da Copernicus
library(ncdf4)
library(raster)

# per visualizzare il file "NC", va prima importato
# raster importa una singola immagine es. neve il 18/5
# brick riporta vari livelli di immagine es. neve dal 18/5 al ...
raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# se da Warning message: ... 
# ci dice che non tutto il sistema è stato importato. 
# Di tutta la terra abbiamo preso solo la parte sull'Europa
# L'immagine è stata tagliata

# cambiamo colore
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# plottare il dato
plot(snowmay,col=cl)
# copertura nevosa indicata con il bianco
# ci aspettiamo che la copertura nevosa sia calata

# scaricare file dal IOL
# creare una nuova cartella su Lab ed inserire le immagini del file Zip






