# R code per l'analisi dei dati NO2 dal ESA - da gennaio a marzo 2020

setwd("C:/lab")
# vado ad importare le immagini, funzione raster
library(raster)
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
# provare con:
plot ( EN01, EN02, EN03, EN04, EN05, col=cl)  # plotta il singolo raster e così non si può fare. Bisogna fare un dataset con all'interno tutte le immagini

# day 2
setwd("C:/lab/")

# caricare dato precedente
load("EN.RData")
ls()

# appricare un ciclo (intera lista di dati): list.files()
# creare una cartella, all'interno della cartella Lab

setwd("C:/lab/esa_no2")

# creo rlist contenente tutti i file della cartella esa_no2
rlist <- list.files(pattern="")
rlist # vedo cosa c'è dentro, tutti EN_00....ecc

# uso la funzione lapply()  : per caricare i dati tutti assieme
# applichiamo alla lista "rlist" la funzione "raster". Il tutto è associato ad un nome, "listafinale"
listafinale <- lapply(rlist, raster) # tutti i file sono inseriti in una unica lista

# ora si può fare un plot con tutti i par dell'altro giorno
# tutti i file (bande) li compattiamo in una singola immagine attravero il comando "stack()"
EN <- stack(listafinale) # EN è il nome

# adesso è possibile fare il plot finale con tutte le immagini all'interno
# con centinaia di immagini, invece di plottare ogni singolo dato, si può fare in 4 passaggi:
# 1. si inseriscono tutti in una cartella.
# 2. importare la lista con comando lapply()
# 3. compattare con codice "stack()"
# 4. inviare il plot
cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)







