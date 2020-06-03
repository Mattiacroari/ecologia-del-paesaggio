setwd("C:/lab/.../")
install.packages("ncdf4") # per caricare dati da Copernicus
library(ncdf4)
library(raster)

raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")   # importo una singola immagine
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)   # cambio colore
plot(snowmay,col=cl)   # plotto singola immagine


# importare l'intero pacchetto di dati scaricati da Copernicus
rlist <- list.files(pattern=".tif")
list_rast <- lapply(rlist, raster)   # tutti i dati sono dentro alla lista "list_rast"
snow.multitemp <- stack(list_rast)   # con stack comprimiamo tutti i dati in una singola immagine. "snowmultitemp" è il nome
plot(snow.multitemp, col=cl)

# per importare la prima e l'ultima immagine in modo tale da vedere le differenze
par(mfrow=c(1,2))   # le immagini sono vicine
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))   # controllare la legenda che combaci, se non combaciano cambiare attraverso comando zlim
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))   # per decidere la legenda guardare i valori massimi e minimi dei dati

# fare differenza tra le due immagini
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100)   # rosso è massima differenza, blu minima, bianca poco
plot(difsnow, col=cldiff)

# previsione per il futuro
source("prediction.r")
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
   # M.C. funzione lineare si vede una previsione ragionevole
   # M.C. "scenari" molto probabile della copertura nevosa
plot(predicted.snow.2025.norm, col=cl)


# misurare come è cambiato nel tempo con comando "boxplot"
boxplot(...)   # se è posizionato verticalmente
boxplot(EN, horizontal=T)
boxplot(EN, horizontal=T,outline=F)   # per rimuovere gli outline
boxplot(EN, horizontal=T,outline=F,axes=T)



# fare zoom su zona del Mediterraneo/MENA
# diversi modi:
   # definire le coordinate:
     ext <- c(..., ..., ..., ...)   # tra parentesi ci vanno le coordinate X e Y (minimi e massimi)
     zoom(snow.multitemp$snow2010r, ext=extension)
     plot(snow.multitemp$snow2010r, col=clb)
     zoom(snow.multitemp$snow2010r, ext=drawExtent())
       # partire dal punto in alto a sinistra, tenere premuto, formare il rettangolo, rilasciare il dito e cliccare una volta.
   # usare comando crop:
     extension <- c(6, 20, 35, 50)   # creo una immagine più piccola di quella originale
     snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
     plot(snow2010r.italy, col=clb)



