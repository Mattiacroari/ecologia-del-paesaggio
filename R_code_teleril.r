# Codice R per analisi di immagini satellitari

# packages: raster
install.packages("raster")
library(raster)    # richiamo dati nel pacchetto raster

setwd("C:/lab/")

# ... <- brick prende una immagine dentro una cartella e le da un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

#day 2
# per ricaricare il dato di ieri
setwd("C:/lab/")
load("C:/Lab/R_code_teleril.RData")
load("teleril")     # se non c'è estensione RData (non sempre Windows salva con estensione)
ls()

library(raster)     # le librerie vanno lanciate tutte le volte

plot(p224r63_2011) # significato bande: B1: blue, B2: green, B3: red, B4: near infrared (nir), B5: medium infrared, B6: thermal infrared, B7: medium infrared
cl <- colorRampPalette(c('black','grey','light grey'))(100) # 100 indica le sfumature di colore. Dal nero al grigio chiaro ci sono 100 sfumature
plot(p224r63_2011, col=cl)
# esperimento grey scale low amount of colours
cllow <- colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011, col=cllow)

names(p224r63_2011)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)  
# attch(dataframe) non funziona con il pacchetto raster
# simpbolo che lega la colonna (la banda) al dataset (immagine satellitare): $

# Exercise: plottare la banda dell'infrarosso vicino (B4: nea infrared)
# colorRampPalette che varia dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# multiframe
par(mfrow=c(2,2))       # ci permette di utilizzare "a blocchi" la finestra. Incollare dopo aver inserito immagine
# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
# green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clg)
# red
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_2011$B3_sre, col=clr)
# nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# comando per chiudere automaticamente la finestra grafica
dev.off()

# natural colours
# 3 componenti: R G B
# 3 bands: R = banda del rosso, G = banda del verde, B = banda del blu
#B1: blue - 1
#B2: green - 2
#B3: red - 3
#B4: near infrared (nir) - 4
# plotRGB()

plotRGB(p224r63_2011, r=3, g=2, b=1)     # solo tre componenti per volta
# stretch prende i colori e li distente
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")  # come li vedrebbe un occhio umano. Difficile riconoscere la vegetazione

# nir
# false colours
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # le zone con una pianta sono colorate di rosso, parte agricola è celeste, parti rosa sono piante coltivate

# salvare immagine R su cartella Lab, per salvare in png: png("primografico.png")
pdf("primografico.pdf")  # primografico è il nome del pdf all'interno della cartella
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()


# multiframe
par(mfrow=c(2,1))  # metto le due immagini vicino a confronto
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # colori naturali
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # colori falsati
dev.off() # cancellare le immagini

# nir nella componente red
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# Exercise: nir nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")  # le piante sono verde fluorescente
# nir nella componente blu
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")  # piante sono blu

### day 2
library(raster)
setwd("C:/lab/")

# lista
ls()

# ... <- brick prende una immagine dentro una cartella e le da un nome
p224r63_1988 <- brick("p224r63_1988_masked.grd")
plot("p224r63_1988")
par(mfrow=c(2,2))

# green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg)
# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre, col=clb)
# red
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_1988$B3_sre, col=clr)
# nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre, col=clnir)

dev.off() # chiudere la finestra attuale

# bande dei sensori nel satellite:
   # B1: blue - 1
   # B2: green - 2
   # B3: red - 3
   # B4: near infrared (nir) - 4

plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# Exercise: plot the imag using the nir on the "r" component in the RGB space
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_1988, r=4, g=2, b=1, stretch="Lin") # entrambe corrette

# plot delle due immagini 1988 e 2011
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988") # main è il titolo del grafico
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

# calcolo indice di come sta la vegetazione (Indice di Vegetazione)
# spectral indices
# dvi1988 = nir1988-red1988 se la pianta è sana il valore è alto
# $ lega diversi pezzi tra loro 
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi1988)

# Exercise: calculate dvi for 2011
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi2011)

# multitemporal analysis
difdvi <- dvi2011 - dvi1988
plot(difdvi)
cldifdvi <- coloRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi) # zone in cui la veg è tagliata o in sofferenza è rossa, le piante che stanno meglio sono blu, situazioni stabili sono colore bianco

# visualize the output
# multiframe 1988rgb, 2011rgb, difdvi
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)

dev.off()

# Cambiare la risoluzione
p224r63_2011lr <- aggregate(p224r63_2011, fact=10) # aggrega i pixel e fa risoluzione piu bassa
p224r63_2011
p224r63_2011lr
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# lower resolution
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50
# original 30m -> resampled 1500m
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# dvi2011 low resolution
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
dev.off() #????
plot(dvi2011lr50)

# dvi1988 low resolution
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

# diffvilr low resolution
difdvilr50 <- dvi2011lr50 - dvi1988lr50
cldifvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvilr50,col=cldifdvi)

# multiframe 
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)





