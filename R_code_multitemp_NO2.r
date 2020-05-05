# R code per l'analisi dei dati NO2 dal ESA - da gennaio a marzo 2020

setwd("C:/lab")
# vado ad importare le immagini, funzione raster
EN01 <- raster("EN_0001.png")
plot(EN01)   # visualizziamo immagine

# esercizio: importare tutte le immagini 

EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

# se avessimo EN1 ....EN13 avrebbe fatto confusione sull'ordine numerico, mescola le immagini. EN1,EN10,EN11..EN13,EN2

plot(EN02)
plot(EN03)
plot(EN04)
plot(EN05)
plot(EN06)
plot(EN07)
plot(EN08)
plot(EN09)
plot(EN10)
plot(EN11)
plot(EN12)
plot(EN13)

cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN01, col=cl)  # inizio gennaio: il giallo è il massimo di NO2, il rosso il minimo
plot(EN13, col=cl)  # a marzo: il giallo diminuisce molto
# per vederle assieme par()
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

# per fare la differenza tra EN13 e EN01
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) # le zone gialle sono quelle con alte differenze tra le due, quelle blu con poche differenze
plot(difno2, col=cldif)

# esercizio: plottare tutti i dati assieme, le 13 immagini (n primo)
# se fosse stato es. 12 si poteva fare 4x3 quindi veniva un grafico ben suddiviso
# 4 righe per 4 colonne
# se le immagini sono troppo grandi usare dev.off() e rifare...se non continua boooh!?
par(mfrow=c(4,4))  # dopo di che plottare tutte le immagini
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)



