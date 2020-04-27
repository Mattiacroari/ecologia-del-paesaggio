# Argomento = land cover 

setwd(C....

install.packages("RStoolbox")
library(RStoolbox)
library(raster)

# brick= impila i dati e portali su R

p224r63_2011 <- brick("p224r63_2011_masked.hdr")

# con le tre componenti, red=4 del vicino infrarosso,green=nel rosso, blue

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# della nostra immagine le classi sembrano 4
# p224r63_2011c = "c" nel senso classificato

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# plotto la mappa       $map = mappa generata
# valori pixel da 1 a 4

plot(p224r63_2011c$map)

# ma cambio i colori(che me li ha messi di standard R)

clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 

plot(p224r63_2011c$map, col=clclass)

# proviamo a mettere le classi uguali a 2 invece che 4

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

# in funzione del numero di classi aumenta l'incertezza
# possiamo fare piu mappe con piu o meno classi per vedere le difernze tra i cluster dei pixel 
# con due classi incertezza bassa

      
# exercise: caricare il workspace point_pattern.Rdata (load("...")) e creare un grafico
 # andare in Session/Set Working Directory/Choose Directory
      library(spatstat)
library(rgdal)   # per le coastline
setwd("...")
load("point_pattern.RData")
ls()
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)   
      
      
      
# interpolazione dei valori

# 1a cosa da fare. Guardare la tabella dei dati e vedere quale variabile mi interessa
# head(covid)   a me interessa colonna "cases"
# funzione marks = valori che do ai dati del point pattern e lo associo alla colonna cases
head(covid)
view(covid) # vedi intera lista di paesi con numero di casi
      
marks(covids) <- covid$cases

# s ( per stima )
# smooth dei punti spaziali del covid

s <- Smooth(covids)

plot(s)

# aggiungo titolo, colori diversi(colourpalette), aggiungo i punti e aggiungo coastiles al plot s

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# GRFICO = stima dei casi. Molto alta verso DX(zona cina) 

text(covids)    # per aggiungere il valore dei punti 


# MAPPA FINALE
# paragonare i due grafici plottati, quello sulla densita e quello sull interpolazione

par(mfrow=c(2,1))

# densità
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

 


# TESI SAN MARINO
# setto la working directory
# prima cosa carico i dati 

load("Tesi.Rdata")
ls()  # per vedere cosa c'è dentro
head(Tesi)

# grafico densita dei punti perche come varibili ho le coordinate

library(spatstat)  # per usare ppp

# devo fare un ppp per crearlo mi serve coordinata x, coord.y, c(xmin,xmax),c(ymin,ymax) cioe i miniti della x e della y
# uso funzione che si chiama summery che mi dice i valori min e max delle coordinate
# x va da 12.42 a 12.46 (mettero in realta 12.41 e 12.47 per stare un po piu larghi)
# y da 43.91 a 43.93 ( aumento di 1 per estendere)

attach(Tesi)
summary(Tesi)

Tesippp <- ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.94))

# grafico della densità

dT <- density(Tesippp)

plot(dT)

points(Tesippp,col="green")      
      
      

      
      
      
      
      
      
   
