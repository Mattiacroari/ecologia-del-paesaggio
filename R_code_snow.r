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
# cambiare setwd
setwd("C:/lab/snow")

# Esercizio da codici IOL, importare intero pacchetto di file
# put all files into the folder
  # rlist=list.files(pattern=".png", full.names=T)
#save raster into list
#con lappy
  # list_rast=lapply(rlist, raster)
  # EN <- stack(list_rast)
  # plot(EN)

# guardare il nome del file
rlist <- list.files(pattern=".tif")
rlist 
# lapply applica un comando ad una serie di dati
list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast) # diamo un nome all'insieme di tutti i file (stack)
plot(snow.multitemp, col=cl) 

# per vedere l'immagine iniziale e immagine finale
# si notano le differenze
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
# valore legenda diverso
# mettere funzione zlim=c(0-250)   0-250 sono i numeri che vogliamo
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

# fare differenza tra le due immagini
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
# colorRampPalette nuova
cldiff <- colorRampPalette(c('blue','white','red'))(100) # rosso è massima differenza, blu minima, bianca poco
plot(difsnow, col=cldiff)




