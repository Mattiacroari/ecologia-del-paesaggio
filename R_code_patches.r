setwd("C:/lab/")

library(raster)
install.packages("igraph")
library(igraph)
library(ggplot2)

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
# stessa cosa per la seconda immagine
d2c.for <- reclassify(d2c, cbind(1,NA))

# ora facciamo un plot delle due immagini con raffigurata solo la foresta
# ora abbiamo solo la foresta
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

# andiamo a creare i patch
# uniamo i pixel vicini in una singola patch
# funzione chiamata "clump"
# la applichiamo sulla prima mappa con la foresta ed associamo un nome
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)
# se da errore con "igraph" bisogna installare il pacchetto
install.packages("igraph")
library(igraph) # for pacthes
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# scrivere le mappe per inviarle poi a un ricercatore esterno
# salvare dati verso l'esterno
# comando "writeRaster(...)"
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")
# mi trovo sulla Cartella Lab l'immagine nominata d1c.for.patches.tif

# Esercizio: plottare le immagini una affianco all'altra
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)
# due patch gigandi: blu e "azzurro"
# nella seconda mappa queste patch sono frammentate
# molte piu patch visibili e ciò indica un grande distruzione della foresta
# per vedere quante patch sono presenti: scrivere il nome "d1c.for.patches"

# risultati plot
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)

library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
# si vede quante patch c'erano nel 1' periodo e nel 2'
# nella tabella sono messe in ordine alfabetico quindi sono invertite
# prima deforestazione=310 patch
# dopo deforestazione=1212 patch
# si è persa molta foresta e quella rimasta è stata frammentata ed è molto pericoloso
# organismi divisi in piccolissimi gruppi e l'estinzione è più probabile




