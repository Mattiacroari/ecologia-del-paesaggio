# Ananisi multitemporali con terreno suddiviso in varie classi di copertura del suolo 

setwd("C:/lab")

library(raster)

# uso BRICK per caricare tutte le singole bande di immagini satellitari 
# carico le immagini riguardanti le deforestazioni 1 e 2 

defor1 <- brick("defor1_.png")
defor2 <- brick("defor2_.png")

# DEFOR1 ho tre bande, metto Infrarosso vicino alla banda R, nella componente G inserisco la banda R, nella componente R inserisco la banda G

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin") # immagine della foresta pluviale dove le piante sono in rosso
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# faccio due classi per classificare tutto quello che è forsta
# funzione unsuperclass è per creare le classi nonn supervisionate (non gli diamo un imput)
# devo caricare perô RStoolbox

install.packages("RStoolbox")
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2) # è una $map

d1c
# d1c$map è la mia mappa

plot(d1c$map)

# cambio i colori 

cl <- colorRampPalette(c('black','green'))(100) # ho la foresta in verde e tutto il resto in nero
plot(d1c$map, col=cl)


# Classifico anche la seconda immagine

d2c <- unsuperClass(defor2, nClasses=2) 
d2c
# d2c$map è la mia mappa

plot(d2c$map)
cl <- colorRampPalette(c('black','green'))(100) # ho la foresta in verde e tutto il resto in nero
plot(d2c$map, col=cl)

dev.off()

plotto le due immagine appena ottenute
par(mfrow=c(2,1))
plot(d2c$map, col=cl)
plot(d1c$map, col=cl)


# QUANTIFICO ADESSO LA PERCENTUALE di foresta persa (in base al numero di pixels appartenenti ad ogni classe)
# MAPPA 1
freq(d1c$map) # mi conta i pixel per ogni classe
              # n.di pixel area foresta = 305095
              # n.di pixel area aperta = 36197
              
# calcolo il totale e poi le proporzioni (che x100 mi da la percentuale) tra le due classi
# freq = freq della mappa per 100/il totale

totd1 <- 305095+36197
totd1   # mi da 341292

percent1 <- freq(d1c$map) * 100 / totd1
percent1   # mi mostra le percentuali (89.4% e 10.6%)

# MAPPA 2

freq(d2c$map) # frequenza 2 è la classe della foresta
totd2 <- 178625+164101
totd1 
percent2 <- freq(d2c$map) * 100 / totd2
percent2 # che è 47.8% e 52.2%(di foresta)

# CREO UN DATAFRAME, una picoola tabella con i vari volri di percentuali

cover <- c("Agriculture","Forest")
before <- c(10.6,89.4)
after <- c(47.8,52.2)
# creo le colonne

output <- data.frame(cover,before,after)

# ADESSO devo plottare i valori
# richimao "ggplot2"

library(gglpot2)


#### day 2

setwd("C:/lab/")
load("defor.RData")
ls()    # vedere cosa c'è nel file

install.packages("ggplot2")
library(ggplot2)
library(raster)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

output # visualizzare la tabella costruita precedentemente. Si vede come l'agricoltura è passato da 11% a 48%. mentre la foresta è passata da 89 a 52%

# andiamo a fare degli istogrammi su dataset output, cioe le percentuali di copertura agricola e forestale
# in X c'è agricoltura (agr) o foresta (for)
# in Y le percentuali delle varie classi
# il colore si basa sulla copertura

# istogrammi della copertura prima della deforestazione
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
# avremo una bassa % di agricoltura e una alta % di foresta

# exercise: plotta l'istogramma della superficie dopo la deforestazione
# after si trova nella tabella lanciata con output
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")
# plot degli istogrammi vicini, cosi visibili assieme
# fare altro pacchetto perche par, con ggplot, non funziona
install.packages("gridExtra")
library(gridExtra)  # oppure: require(Extra)

# grid.arrange(plot1, plot2, nrow = 1)
# prende vari plot(es 1 e 2) e li mette nella stessa immagine (stessa funzione di )
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")
grid.arrange(grafico1, grafico2, nrow = 1)  # al posto di "plot1" e "plot2" inserire i nomi dei grafici precedentemente creati








