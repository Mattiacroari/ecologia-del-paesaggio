setwd("C:/lab/")
library(raster)

# caricare dati con due funzioni
# se raster con tanti livelli si usa "brick"
# se carichiamo un singolo raster si usa "raster"
# in questo caso usiamo la funzione "raster"
# diamo il nome alla prima mappa
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

# adesso plottiamo le immagini con comando par(mfrow...) cambiando anche colori
par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
# foresta con colore verde, corretta? no, la foresta in questo caso è colorata di nero
# la foresta è la classe 2, l'agricoltura è la classe 1
# bisogna invertire la colorRampPalette per avere la fortesta "verde"
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)

# mettere per dato agricoltura sia un valore nullo
# bisogna estrarre soltanto la foresta per fare dei calcoli
# bisogna "annullare" tutto ciò che non è foresta con funzione "cbind(..., NA)"
# riclassifica una immagine raster (cambia i valori) con comando reclassify()
# bisogna togliere il dato "agricoltura" che in questo caso è la classe 1
# associamo anche un nome
d1c.for <- reclassify(d1c, cbind(1,NA))
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for)
# cambiamo il colore del dato foresta da giallo a verde
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for, col=cl)








