# R_code_exam.r

# inserire i singoli script
### 1. R code first
### 2. R code spatial
### 3. R code spatial2
### 4. R code point pattern
### 5. R code teleril
### 6. R code landcover
### 7. R code multitemp
### 8. R code multitempNO2
### 9. R code snow
### 10. R code patches
### 11. R code crop
### Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

##################################### 1.
#####################################
#####################################
# R code first 

install.packages("sp")

# richiamo il pacchetto 

library(sp)

data(meuse)

# M.C. se digito solo meuse avro tutta la tabella

meuse 
# M.C. visualizzo data set solo nelle prime righe
head(meuse)
# names = nome delle variabili
names(meuse)

# M.C. summery= si puo fare un abstact delle info del dataset e delle funzioni che contiene
summary(meuse)

# M.C. pairs= funzione che crea grafico mostruoso= correlazione tra le varie variabili(tutte insieme)
pairs(meuse)

# M.C. c'è il modo di ridurre il numero di variabili nella funzione pairs
# M.C. ~ = TILDE, è un simbolo che significa UGUALE

pairs(~ cadmium + copper + lead , data = meuse) 

# EXERCISE 

# M.C. [,3:6]= vuol fare un subset ([]), la "," vuol dire parti da, e 3:6 vuol dire dalla colonna 3 alla 6
# M.C. pairs(meuse[,3:6]) = pairs(~cadmium+copper+lead+zinc,data = meuse)
# M.C. " " si usano per argomento di testo

pairs(meuse[,3:6],
      col = "red",                                         # M.C. Cambio colore dei punti
      pch = 19,                                            # M.C. Cambio la forma dei punti, pch=point character
      labels = c("var1", "var2", "var3"),                  # M.C. Change labels of diagonal
      main = "This is a nice pairs plot in R")             # M.C. Add a main title
      
      
# AGGIUNGO ELEVATION (che è la 7ima variabile)
pairs(meuse[,3:7],
# il resto poi tutto uguale


# FUNZIONI PANNEL

# M.C. <- = dai nomi alle punzioni, ho chiamato con un certo nome un blocco di codici
# M.C. faccio indice di correlazione tra x e y (tra due variabili)

# PRIMA FUNZIONE
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)

     usr <- par("usr"); on.exit(par(usr))
     par(usr = c(0, 1, 0, 1))
     
 
# M.C. SECONDA FUNZIONE -> panel.smoothing = È un plot di punti con "lowes" = smoother locale per mostrare una linea di correlazione tra variabili

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
   cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
   points(x, y, pch = pch, col = col, bg = bg, cex = cex)


# M.C. TERZA FUNZIONE -> panel.histograms = ISTOGRAMMI,fa un istogramma delle distrubuzioni di frequenza

panel.histograms <- function(x, ...)
usr <- par("usr"); on.exit(par(usr))


# M.C. Decido cosa mettere nei vari spazi del grafico (upper pannel,lower,diagonale(cioe centrale))

pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)


EXERCISE: mettere come lower panel lo smoothing, come diagonal apnel gli istogrammi e come upper panel le correlazioni 

pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)


# FUNZIONE PLOT

# M.C. come sono relazionati tra loro cadmio e rame?
# M.C. $ = in R collega un pezzo con un altro, nostro caso collega la colonna col proprio data set

plot(meuse$cadmium,meuse$copper)

# M.C. oppure allego il dataframe con attach= spiega a R che utilizzeremo sempre quel dataset per le alre funzuoni (cosi non uso $)

attach(meuse)

plot(cadmium,copper) 

plot(cadmium, copper, pch=17, col="green", main="primo plot")

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame") 

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2)


##################################### 2.
#####################################
#####################################
#R code spatial
# R spaziale : funzioni spaziali in Ecologia del Paesaggio

install.packages("sp")

# richiamo il pacchetto sp 
library(sp)

# M.C. richiamo i dati "meuse"
data(meuse)

head(meuse)

# M.C. plot cadium e lead, devo allegare database

attach(meuse)

# M.C. plotto e coloro e uso diverso carattere(pch=point character) e aumento dimensione(cex=character exageration)

plot(cadmium,lead,col="red",pch=19,cex=2)

# exercise : plot di copper e zinco con simbolo triangolo e colore verde

plot(copper,zinc,col="green",pch=17,cex=2)

# M.C. " " le uso quando ho un testo

# M.C. cambiare le etichette nel grafico (x e y label)

plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

# M.C. multiframe (per mettere piu di un grafico nella stessa finestra), par è la funzione (poi decido se i grafici li volgio in riga o in colonna)
# M.C. c(1,2) = una riga e due colonne , nele due colonne ho messo i due grafici sullo stesso piano, la stessa riga
# M.C. sotto copio e incollo i due grafici da unire , (puo essere utile per es.analisi multitemporale (t0 e t1))

par(mfrow=c(1,2))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# M.C. adesso due righe e una colonna

par(mfrow=c(2,1))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# multiframe automatico
install.packages("GGalli")

# M.C. prendo il dataset Meuse e lo mando nella funzione scaricata "ggpairs"
# M.C. [,3:6] = dalla terza colonna alla sesta colonna per avere tutti gli elementi (li vedo da head(meuse))
# M.C. grafico= sull asse diagonale le singole variabili e la distribuzione dei dati 
# M.C. coefficienti di correlazione, 0.92 = molto correlati

library(GGally)
ggpairs(meuse[,3:6])

# M.C. Spatial, devo spiegare a R che "meuse" ha delle coordinate x e y
# M.C. head(meuse) vedo le coordinate

head(meuse)

# M.C. inserire per primo il dataset e poi speigo che ho x y , gli ho spiegato quali sono le mie coordinate
# M.C. uso ~ per gruppo di coordinate

coordinates(meuse)=~x+y

plot(meuse)

# M.C. inserisco nella funzione il dataset per creare grafico spaziale grazie a spplot (plotto i dati spazialmente)
# M.C. grafico= come si ditribuisce lo zinco attorno al fiume, vicino all'acqua (giallo) valori molto alti, lontani dal fiume puntini neri valore basso

spplot(meuse,"zinc")


##################################### 3.
#####################################
#####################################
# R code spatial2
      
# libreria sp
library(sp)

# dati da usare
data(meuse)
head(meuse)       # M.C. controlla cosa c'è dentro al dataset

#coordinate del dataset (dataframes in R)
coordinates(meuse)=~x+y

# M.C. spplot dei dati di zinco, tabella della distribuzione 
spplot(meuse,"zinc")

# Exercise: spplot dei dati di rame
head(meuse)      # M.C. per capire come si chiamano i nomi delle colonne
names(meuse)     # M.C. come sopra

spplot(meuse,"copper")

# M.C. bubble per plottare i dati. Funzione "bubble" nel pacchetto sp, rappresento uguale a spplot MA i valori piu alti hanno bolle piu grandi 
bubble(meuse,"zinc")

# Exercise: bubble del rame, colore rosso
bubble(meuse,"copper",col="red")
    # M.C. il grafico del rame ha dei valori piuttosto alti nella parte vicino al fume e piu bassi allontanadosi dal fiume 
    # M.C. il fiume è molto inquinato 
# foraminiferi (Sofia), carbon capture (Marco)
# array             
foram <- c(10, 20, 35, 55, 67, 80)         # M.C. array o vettore--- per inserire dataset in R, si è creato un oggetto
carbon <- c(5, 15, 30, 70, 85, 99)         # M.C. significa che la serie di numeri si chiamano carbon e foram
plot(foram, carbon,col="green", cex=2, pch=11)

# Dati dall'esterno (es. tabella su Covid-19)
# cartella da creare su Windows: C:/lab     specifica il percorso 
# Windows:   setwd("C:/lab")

# funzione per leggere una tabella, nominata covid
covid <- read.table("covid_agg.csv",head=TRUE)



##################################### 4.
#####################################
#####################################
# R code point pattern
# Codice per analisi dei point patterns, dati legati ai punti
install.packages("ggplot2") # M.C. per installare un pacchetto
install.packages("spatstat")

library(ggplot2)
library(spatstat)

setwd("C:/lab")     

covid <- read.table("covid_agg.csv", head=T)  # M.C. per importare dati di una tabella

head(covid)

plot(covid$country,covid$cases)    # M.C. $ collega un pezzo ad un altro, in questo caso collega la colonna al proprio dataset altrimenti non riconosce la colonna
plot(covid$country,covid$cases,las=0)  # M.C. per metterlo verticale
plot(covid$country,covid$cases,las=1)  # M.C. las=1 le etichette di asse Y diventano orizzontali
plot(covid$country,covid$cases,las=2)  # M.C. las=2 le etichette della asse X sono verticali
plot(covid$country,covid$cases,las=3)  # M.C. laberls verticali

plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5) # M.C. "cex.axis" per rimpicciolire le scritte di asse X

# ggplot2
# M.C. mpg è un dataset di prassi già all'interno di ggpl0t2
data(mpg)
head(mpg)
      
# M.C. Quello che serve a ggplot2 per creare un grafico è 1. data set 2. aestetics, cioè le variabili 3. La geometria con cui si vuole visualizzare
    # data
    # aes
    # tipo di geometria
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()

# ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()     # M.C. size=cases dimensione variabile in base ai dati


# M.C. density intende la densità dei punti in una mappa
# M.C. create dataset for spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

d <- density(covids)
plot(d)
points(covids)

# save the .RData


setwd("C:/lab")  # M.C. recuperiamo i dati all'interno della cartella Lab
load("point_pattern.RData")
ls()    # M.C. per vedere cosa c'è dentro il file
library(spatstat)

plot(d)

# M.C. cambiare colori nella mappa --> palette
cl <- colorRampPalette(c('yellow','orange','red')) (100)  # M.C. cl è il nome. (100) indica il numero di gradazioni di colore, che vanno dal yellow al red
plot(d, col=cl)  # M.C. creare grafico di "d" con i colori di "cl"

# Exercise: plot della mappa della densita dal verde al blu
cl1 <- colorRampPalette(c('yellow','green','blue')) (100)
points(covids)  # M.C. inserire punti sulla mappa

# M.C. posso anche mettere i confini degli altri stati
# M.C. coastlines = nome del nuovo file
# M.C. readOGR è parte di una libreria GDAL (libreia Geospaziale che permette di leggere qualsiasi tipo di file raster o vettoriale)
# M.C. rGDAL libreria di R
# M.C. installo con le virgolette perche devo uscire da GitHub
coastlines <- readOGR("ne_10m_coastline.shp") # M.C. se da errore può mancare la libreria, non devono essere dentro ad un'altra cartella
install.packages("rgdal")   # M.C. installare il pacchetto
library(rgdal)
    # M.C. a questo punto posso rimandare il comando
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# Exercise: plot della mappa di densità con una nuova colorazione, e aggiunta delle coastlines
cl <- colorRampPalette(c('lightblue','blue','black')) (100)
plot(d, col=cl)
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)





##################################### 5.
#####################################
#####################################
# R code teleril
# Codice R per analisi di immagini satellitari

# packages: raster
install.packages("raster")
library(raster)    # M.C. richiamo dati nel pacchetto raster

setwd("C:/lab/")

# M.C. ... <- brick prende una immagine dentro una cartella e le da un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)
# M.C : Ho B1,B2,... cioe diverse bande con diverse riflettanze nelle diverse lunghezze d'onda
# M.C : in tutto 7 sensori 
   # M.C. B1: blue
   # M.C. B2: green
   # M.C. B3: red
   # M.C. B4: near infrared (nir)
   # M.C. B5: medium infrared
   # M.C. B6: thermal infrared
   # M.C. B7: medium infrared
      
#day 2
# M.C. per ricaricare il dato di ieri
setwd("C:/lab/")
load("C:/Lab/R_code_teleril.RData")
load("teleril")     # M.C. se non c'è estensione RData (non sempre Windows salva con estensione)
ls()

library(raster)     # M.C. le librerie vanno lanciate tutte le volte

plot(p224r63_2011) # M.C. significato bande: B1: blue, B2: green, B3: red, B4: near infrared (nir), B5: medium infrared, B6: thermal infrared, B7: medium infrared
cl <- colorRampPalette(c('black','grey','light grey'))(100) # M.C. 100 indica le sfumature di colore. Dal nero al grigio chiaro ci sono 100 sfumature
plot(p224r63_2011, col=cl)
# esperimento grey scale low amount of colours
cllow <- colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011, col=cllow)

names(p224r63_2011)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)  
# M.C. attch(dataframe) non funziona con il pacchetto raster
# M.C. simpbolo che lega la colonna (la banda) al dataset (immagine satellitare): $

# Exercise: plottare la banda dell'infrarosso vicino (B4: nea infrared)
# M.C. colorRampPalette che varia dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# multiframe
par(mfrow=c(2,2))       # M.C. ci permette di utilizzare "a blocchi" la finestra. Incollare dopo aver inserito immagine
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

# M.C. comando per chiudere automaticamente la finestra grafica
dev.off()

# natural colours
# M.C. 3 componenti: R G B
# M.C. 3 bands: R = banda del rosso, G = banda del verde, B = banda del blu
# M.C. B1: blue - 1
# M.C. B2: green - 2
# M.C. B3: red - 3
# M.C. B4: near infrared (nir) - 4
# plotRGB()

plotRGB(p224r63_2011, r=3, g=2, b=1)     # M.C. solo tre componenti per volta
# M.C. stretch prende i colori e li distente
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")  
# M.C. come li vedrebbe un occhio umano. Difficile riconoscere la vegetazione
      # M.C. allora aggiungo anche NIR ( tramite r=4 ) 4= infrarosso vicino 
      # M.C. scalo tutti i colori di uno per formare FALSE COLOR
      
# nir
# false colours
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # M.C. le zone con una pianta sono colorate di rosso, parte agricola è celeste, parti rosa sono piante coltivate

# M.C. salvare immagine R su cartella Lab, per salvare in png: png("primografico.png")
pdf("primografico.pdf")  # M.C. primografico è il nome del pdf all'interno della cartella
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()


# multiframe
par(mfrow=c(2,1))  # M.C. metto le due immagini vicino a confronto
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # M.C. colori naturali
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # M.C. colori falsati
dev.off() # M.C. cancellare le immagini

# M.C. nir nella componente red
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# Exercise: nir nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")  # M.C. le piante sono verde fluorescente
# M.C. nir nella componente blu
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")  # M.C. piante sono blu

### day 3
library(raster)
setwd("C:/lab/")

# lista
ls()

# M.C. ... <- brick prende una immagine dentro una cartella e le da un nome
p224r63_1988 <- brick("p224r63_1988_masked.grd")
plot("p224r63_1988")
par(mfrow=c(2,2))   # M.C. uso ancora par,mfrow per arere piu immagini nella stessa frame

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
      # M.C. i valori sono molto alti nel NIR ed idica un elevata presenza di piante
      # M.C. i valori del blu e del rosso invece sono valori bassi (perche assorbite dalle inate per la fotosintesi)

dev.off() # M.C. chiudere la finestra attuale

# M.C. bande dei sensori nel satellite:
   # M.C. B1: blue - 1
   # M.C. B2: green - 2
   # M.C. B3: red - 3
   # M.C. B4: near infrared (nir) - 4
   # M.C. B4: near infrared (nir)
   # M.C. B5: medium infrared
   # M.C. B6: thermal infrared
   # M.C. B7: medium infrared
      
# M.C. natural colours (come occhio umano), componenti R,G, e B ( computer plotta 3 bande per volta)
plotRGB(p224r63_2011, r=3, g=2, b=1)
      # M.C. appare nera l'immagine e allora devo strecchare i colori e l'immagine 
      # M.C. stretch=lin cioè lineare

plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
   # M.C. immagine come la vedrebbe occhio umano ma la vegetazione fitta puo essere confusa con ombre
   # M.C. allora aggiungo anche NIR ( tramite r=4 ) 4= infrarosso vicino 
   # M.C. scalo tutti i colori di uno per formare FALSE COLOR
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_1988, r=4, g=2, b=1, stretch="Lin") # M.C. entrambe corrette
      

# plot delle due immagini 1988 e 2011
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988") # M.C. main è il titolo del grafico
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

# M.C. calcolo indice di come sta la vegetazione (Indice di Vegetazione)
# spectral index
      # M.C. pianta sana riflette molto nell'INFRAROSSO (alta riflettanza) e poco nel blu e rosso
      # M.C. uso l'indice chiamato DVI (different vegetation index)
# M.C.dvi1988 = nir1988-red1988 se la pianta è sana il valore è alto
# M.C. $ lega diversi pezzi tra loro 
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi1988)

# Exercise: calculate dvi for 2011
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi2011)

# multitemporal analysis
difdvi <- dvi2011 - dvi1988
plot(difdvi)
cldifdvi <- coloRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi) # M.C. zone in cui la veg è tagliata o in sofferenza è rossa, le piante che stanno meglio sono blu, situazioni stabili sono colore bianco

# visualize the output
# M.C. voglio 3 immagini di fila [mfrow=c(3,1)], immagine del 2011, del 1988 e l'immagine di confronto del DVI
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)

dev.off()

# Cambiare la risoluzione
p224r63_2011lr <- aggregate(p224r63_2011, fact=10) # M.C. aggrega i pixel e fa risoluzione piu bassa
p224r63_2011     # M.C. vedo tutte le info tra cui anche la risoluzione (che è 30m)
p224r63_2011lr   # M.C. invece ha 300 al posto che 30 (perche è stata moltiplicata per 10)
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# lower resolution
   # M.C. provo a diminuitre ancora di piu la risoluzione (risoluzione 1500m con fattore=50)
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50
# original 30m -> resampled 1500m
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# dvi2011 low resolution
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
dev.off()
plot(dvi2011lr50)

# dvi1988 low resolution
     # M.C. Diminuisco la risoluzione dell'immagine del 1988 (risoluzione 1500m con fattore=50)
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



##################################### 6.
#####################################
#####################################
# R code landcover
# Argomento = land cover 

setwd("C:/lab")

install.packages("RStoolbox")
library(RStoolbox)
library(raster)

# M.C. brick= impila i dati e portali su R

p224r63_2011 <- brick("p224r63_2011_masked.hdr")

# M.C. con le tre componenti, red=4 del vicino infrarosso,green=nel rosso, blue

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# M.C. della nostra immagine le classi sembrano 4
# M.C. p224r63_2011c = "c" nel senso classificato

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# M.C. plotto la mappa       $map = mappa generata
# M.C. valori pixel da 1 a 4

plot(p224r63_2011c$map)

# M.C. ma cambio i colori(che me li ha messi di standard R)

clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 

plot(p224r63_2011c$map, col=clclass)

# proviamo a mettere le classi uguali a 2 invece che 4

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

# M.C. in funzione del numero di classi aumenta l'incertezza
# M.C. possiamo fare piu mappe con piu o meno classi per vedere le difernze tra i cluster dei pixel 
# M.C. con due classi incertezza bassa

      
# Parte 2
      
# exercise: caricare il workspace point_pattern.Rdata (load("...")) e creare un grafico
 # andare in Session/Set Working Directory/Choose Directory
library(spatstat)
library(rgdal)   # M.C. per le coastline
setwd("...")
load("point_pattern.RData")
ls()
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)   
      
      
      
# interpolazione dei valori

# M.C. 1' cosa da fare. Guardare la tabella dei dati e vedere quale variabile mi interessa
# M.C. head(covid)   a me interessa colonna "cases"
# M.C. funzione marks = valori che do ai dati del point pattern e lo associo alla colonna cases
head(covid)
view(covid) # M.C. vedi intera lista di paesi con numero di casi
      
marks(covids) <- covid$cases

# s ( per stima )
# M.C. smooth dei punti spaziali del covid

s <- Smooth(covids)

plot(s)

# M.C. aggiungo titolo, colori diversi(colourpalette), aggiungo i punti e aggiungo coastiles al plot s

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# M.C. GRFICO = stima dei casi. Molto alta verso DX(zona cina) 

text(covids)    # M.C. per aggiungere il valore dei punti 


# MAPPA FINALE
# M.C. paragonare i due grafici plottati, quello sulla densita e quello sull interpolazione

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
# M.C. setto la working directory
# M.C. prima cosa carico i dati 

load("Tesi.Rdata")
ls()  # M.C. per vedere cosa c'è dentro
head(Tesi)

# M.C. grafico densita dei punti perche come varibili ho le coordinate

library(spatstat)  # per usare ppp

# M.C. devo fare un ppp per crearlo mi serve coordinata x, coord.y, c(xmin,xmax),c(ymin,ymax) cioe i miniti della x e della y
# M.C. uso funzione che si chiama summery che mi dice i valori min e max delle coordinate
# M.C. x va da 12.42 a 12.46 (mettero in realta 12.41 e 12.47 per stare un po piu larghi)
# M.C. y da 43.91 a 43.93 ( aumento di 1 per estendere)

attach(Tesi) # M.C. molto importante
summary(Tesi)

Tesippp <- ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.94))

# grafico della densità

dT <- density(Tesippp)

plot(dT)

points(Tesippp,col="green")      


# SAN MARINO pt. 2
# M.C. prima cosa settare setwd() su R
# M.C. verificare che ci sia .RData salvata l'altra volta
setwd("C:/lab")
load("sanmarino.RData")
ls()   # M.C. per vedere l'interno del file sanmarino.RData
       # M.C. dt è associato alla densità
       # Tesi 
       # M.C. Tesippp indica il point pattern, incida latitudine e longitudine e da qui si ottiene una density
# M.C. dT= density map, Tesi=dataset originale, Tesi_ppp=point pattern
library(spatstat)
# M.C. carico mappa di densità dei dati, quanto sono densi i punti di campionamento? direttamente legata ai prati aridi.
plot(dT)
points(Tesippp, col="green")

# interpolazione
# M.C. con head si visualizzano solo alcuni valori, prime 6 righe in questo caso
head(Tesi)   # M.C. Species_richness sono i dati che ci servono
marks(Tesippp) <- Tesi$Species_richness # M.C. mark è un valore che viene incollato, prendiamo i singli punti di campionamento e li associamo al primo punto di pointpatter e cosi via
# M.C. associato al point pattern il valore che ci interessava, cioè la richezza di specie
# M.C. si crea una mappa continua formata da pixel con valori diversi tra loro
# M.C. stima di valore che non sono stati interpolati
interpol <- Smooth(Tesippp)    # M.C. Smooth dei valori individuati legati con marks
plot(interpol)
points(Tesippp, col="green")   # M.C. i valori piu bassi sono a nord e sud-ovest. valori alti sono a ovest e sud-est


# caricare librerira rgdal
library(rgdal)
sanmarino <- readOGR("San_Marino.shp")   # M.C. OGR=file vettoriale
plot(sanmarino)   # M.C. file vettoriale fatto con punti,linee e coordinate. questo è un poligono
plot(interpol, add=T)   # M.C. add=T aggiunge pezzi alla mappa precedente, altrimenti cancella la mappa appena caricata. tutto sovrapposto
points(Tesippp, col="green")  # M.C. aggiungere i punti
# M.C. per mettere la mappa di san marino sopra gli altri dati
plot(sanmarino, add=T)

# Exercise: plot multiframe di DENSITà e interpolazione. le immagini sono una sopra l'altra
par(mfrow=c(2,1))    # M.C. 2 righe e 1 colonna. c indica un gruppo di oggetti contenuto tra le ()

plot(dT, main="Density of points")
points(Tesippp,col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")

# mettere le immagini una affianco all'altra, modificare il mfrow
par(mfrow=c(1,2))
plot(dT, main="Density of points")
points(Tesippp,col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp,col="green")



##################################### 7.
#####################################
#####################################
# R code multitemp
# Ananisi multitemporali con terreno suddiviso in varie classi di copertura del suolo 

setwd("C:/lab")

library(raster)

# M.C. uso BRICK per caricare tutte le singole bande di immagini satellitari 
# M.C. carico le immagini riguardanti le deforestazioni 1 e 2 

defor1 <- brick("defor1_.png")
defor2 <- brick("defor2_.png")

# M.C. DEFOR1 ho tre bande, metto Infrarosso vicino alla banda R, nella componente G inserisco la banda R, nella componente R inserisco la banda G

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin") # M.C. immagine della foresta pluviale dove le piante sono in rosso
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# M.C. faccio due classi per classificare tutto quello che è forsta
# M.C. funzione unsuperclass è per creare le classi nonn supervisionate (non gli diamo un imput)
# M.C. devo caricare perô RStoolbox

install.packages("RStoolbox")
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2) # M.C. è una $map

d1c
# M.C. d1c$map è la mia mappa

plot(d1c$map)

# M.C. cambio i colori 

cl <- colorRampPalette(c('black','green'))(100) # M.C. ho la foresta in verde e tutto il resto in nero
plot(d1c$map, col=cl)


# M.C. Classifico anche la seconda immagine

d2c <- unsuperClass(defor2, nClasses=2) 
d2c
# M.C. d2c$map è la mia mappa

plot(d2c$map)
cl <- colorRampPalette(c('black','green'))(100) # M.C. ho la foresta in verde e tutto il resto in nero
plot(d2c$map, col=cl)

dev.off()

# M.C. plotto le due immagine appena ottenute
par(mfrow=c(2,1))
plot(d2c$map, col=cl)
plot(d1c$map, col=cl)


# M.C. QUANTIFICO ADESSO LA PERCENTUALE di foresta persa (in base al numero di pixels appartenenti ad ogni classe)
# MAPPA 1
freq(d1c$map) # M.C. mi conta i pixel per ogni classe
              # M.C. n.di pixel area foresta = 305095
              # M.C. n.di pixel area aperta = 36197
              
# M.C. calcolo il totale e poi le proporzioni (che x100 mi da la percentuale) tra le due classi
# M.C. freq = freq della mappa per 100/il totale

totd1 <- 305095+36197
totd1   # M.C. mi da 341292

percent1 <- freq(d1c$map) * 100 / totd1
percent1   # M.C. mi mostra le percentuali (89.4% e 10.6%)

# MAPPA 2

freq(d2c$map) # M.C. frequenza 2 è la classe della foresta
totd2 <- 178625+164101
totd1 
percent2 <- freq(d2c$map) * 100 / totd2
percent2 # M.C. che è 47.8% e 52.2%(di foresta)

# M.C. CREO UN DATAFRAME, una picoola tabella con i vari volri di percentuali

cover <- c("Agriculture","Forest")
before <- c(10.6,89.4)
after <- c(47.8,52.2)
# M.C. creo le colonne, prima e dopo disboscamento

output <- data.frame(cover,before,after)

# M.C. ADESSO devo plottare i valori
# richimao "ggplot2"

library(gglpot2)


#### day 2

setwd("C:/lab/")
load("defor.RData")
ls()    # M.C. vedere cosa c'è nel file

install.packages("ggplot2")
library(ggplot2)
library(raster)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

output # M.C. visualizzare la tabella costruita precedentemente. Si vede come l'agricoltura è passato da 11% a 48%. mentre la foresta è passata da 89 a 52%

# M.C. andiamo a fare degli istogrammi su dataset output, cioe le percentuali di copertura agricola e forestale
# M.C. in X c'è agricoltura (agr) o foresta (for)
# M.C. in Y le percentuali delle varie classi
# M.C. il colore si basa sulla copertura

# M.C. istogrammi della copertura prima della deforestazione
p1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
# M.C. avremo una bassa % di agricoltura e una alta % di foresta
plot(p1)
# exercise: plotta l'istogramma della superficie dopo la deforestazione
# M.C. after si trova nella tabella lanciata con output
p2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")
plot(p2)
# M.C. plot degli istogrammi vicini, cosi visibili assieme
# M.C. fare altro pacchetto perche par, con ggplot, non funziona
install.packages("gridExtra")
library(gridExtra)  # oppure: require(Extra)

# grid.arrange(plot1, plot2, nrow = 1)
# M.C. prende vari plot(es 1 e 2) e li mette nella stessa immagine (stessa funzione di )
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")
grid.arrange(grafico1, grafico2, nrow = 1)  # M.C. al posto di "plot1" e "plot2" inserire i nomi dei grafici precedentemente creati

dev.off()  # chiudere il lavoro


# data 06/05/202
# recupero dati di ieri
library(ggplot2)
library(gridExtra)
cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)
output <- data.frame(cover,before,after)
output

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow = 1)

# mettere grafico da 0 a 100 sull'asse y: ylim(0, 100)
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

grid.arrange(grafico1, grafico2, nrow = 1) # plotto i due nuovi grafici




##################################### 8.
#####################################
#####################################
# 8. R code multitempNO2

# R code per l'analisi dei dati NO2 dal ESA - da gennaio a marzo 2020

setwd("C:/lab")
# M.C. vado ad importare le immagini, funzione raster
library(raster)
EN01 <- raster("EN_0001.png")
plot(EN01)   # M.C. visualizziamo immagine

# esercizio: importare tutte le immagini 
# M.C. se metto lo zero prima del numero me le mette tutte in ordine
      
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

# M.C. se avessimo EN1 ....EN13 avrebbe fatto confusione sull'ordine numerico, mescola le immagini. EN1,EN10,EN11..EN13,EN2

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
plot(EN01, col=cl)  # M.C. inizio gennaio: il giallo è il massimo di NO2, il rosso il minimo
plot(EN13, col=cl)  # M.C. a marzo: il giallo diminuisce molto
# M.C. per vederle assieme par()
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

# M.C. per fare la differenza tra EN13 e EN01
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) # M.C. le zone gialle sono quelle con alte differenze tra le due, quelle blu con poche differenze
plot(difno2, col=cldif)

# esercizio: plottare tutti i dati assieme, le 13 immagini (n primo)
# M.C. se fosse stato es. 12 si poteva fare 4x3 quindi veniva un grafico ben suddiviso
# M.C. 4 righe per 4 colonne
# M.C. se le immagini sono troppo grandi usare dev.off() e rifare...se non continua boooh!?
par(mfrow=c(4,4))  # M.C. dopo di che plottare tutte le immagini
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
plot ( EN01, EN02, EN03, EN04, EN05, col=cl)  # M.C. plotta il singolo raster e così non si può fare. Bisogna fare un dataset con all'interno tutte le immagini

# day 2
# M.C. come faccio ad imporatre tutti ifile allo stesso tempo? SE HO TANTI FILES!!
# M.C. devo creare una cartella all'interno della cartella LAB e cambiare la working directory
# M.C. metto tutte le immagini nella nuova cartella "esa_no2" e la imposto come working directory

setwd("C:/lab/")

# caricare dato precedente
load("EN.RData")
ls()

# M.C. appricare un ciclo (intera lista di dati): list.files()
# creare una cartella, all'interno della cartella Lab

setwd("C:/lab/esa_no2")

# creo rlist contenente tutti i file della cartella esa_no2
rlist <- list.files(pattern="")
rlist # M.C. vedo cosa c'è dentro, tutti EN_00....ecc

# M.C. uso la funzione lapply()  : per caricare i dati tutti assieme
# M.C. applichiamo alla lista "rlist" la funzione "raster". Il tutto è associato ad un nome, "listafinale"
listafinale <- lapply(rlist, raster) # M.C. tutti i file sono inseriti in una unica lista
listafinale   # M.C. visualizzo 13 elementi in lista con le singole bande
      
# M.C. ora si può fare un plot con tutti i par dell'altro giorno
# M.C. tutti i file (bande) li compattiamo in una singola immagine attravero il comando "stack()"
EN <- stack(listafinale) # M.C. EN è il nome

# M.C. adesso è possibile fare il plot finale con tutte le immagini all'interno
# M.C. con centinaia di immagini, invece di plottare ogni singolo dato, si può fare in 4 passaggi:
# M.C. 1. si inseriscono tutti in una cartella.
# M.C. 2. importare la lista con comando lapply()
# M.C. 3. compattare con codice "stack()"
# M.C. 4. inviare il plot
cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)

# Day 3
library(raster)
setwd("C:/lab/")
setwd("C:/lab/esa_no2") # M.C. richiamo la cartella
# M.C. nella cartella ci sono file .png
# M.C. usiamo "list.file()" per unire tutti i file in una lista e la associamo ad un nome
rlist <- list.files(pattern=".png")

# M.C. uso la funzione lapply()  : per caricare i dati tutti assieme
# M.C. applichiamo alla lista "rlist" la funzione "raster".
listafinale <- lapply(rlist, raster)
# M.C. ora creo un pacchetto con la lista di file.
# M.C. il pacchetto lo chiamo EN
EN <- stack(listafinale)
EN # M.C. mostra cosa c'è dentro. Dimensione, Nomi, Classe ecc
# M.C. abbiamo preso i singoli file (13 in totale)
# M.C. registrato per ogni pixel la quantità di NO2
# M.C. lo stack di file si chiama EN
# M.C. scorporiamo l'ultimo e il primo per vedere la differenza di NO2 tra fine e inizio
# M.C. trovo il valore del pixel del prima immagine il valore dello stesso pixel della ultima immagine
difEN <- EN$EN_0013 - EN$EN_0001
# M.C. facciamo una colorRampPalette
# M.C. plottiamo per avere l'immagine
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(difEN, col=cld)
# M.C. zone in cui NO2 è calato drasticamente, indicate in ROSSO
# M.C. zone con poche differenze sono BLU
# M.C. BIANCO se non ci sono differenze

# plot dell'intero set
# cambiamo la Palette
cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)
# M.C. simile al comando par. ma è più semplice creare uno stack di dati e plottarli come sopra.


# M.C. misuriamo come è variato il NO2 nel tempo
# M.C. utilizzo di un BOXPLOT
boxplot(EN)   # M.C. è posizionato verticalmente
boxplot(EN, horizontal=T) # M.C. messo orizzontalmente

# M.C. per rimuovere gli outline
boxplot(EN, horizontal=T,outline=F)
# M.C. abbiamo in valori di azoto da 0-250
boxplot(EN, horizontal=T,outline=F,axes=T) # M.C. probabile non campi perche comando gia inserito


##################################### 9.
#####################################
#####################################
# 9. R code snow
      
# M.C. prima cosa fare setwd
setwd("C:/lab/")
install.packages("ncdf4") # M.C. per caricare immagini da Copernicus
library(ncdf4)
library(raster)

# M.C. per visualizzare il file "NC", va prima importato
# M.C. raster importa una singola immagine es. neve il 18/5
# M.C. brick riporta vari livelli di immagine es. neve dal 18/5 al ...
raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# M.C. se da Warning message: ... 
# M.C. ci dice che non tutto il sistema è stato importato. 
# M.C. Di tutta la terra abbiamo preso solo la parte sull'Europa
# M.C. L'immagine è stata tagliata

# cambiamo colore
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# plottare il dato
plot(snowmay,col=cl)
# M.C. copertura nevosa indicata con il bianco
# M.C. ci aspettiamo che la copertura nevosa sia calata

# scaricare file dal IOL
# M.C. creare una nuova cartella su Lab ed inserire le immagini del file Zip
# M.C. cambiare setwd
setwd("C:/lab/snow")

# Esercizio da codici IOL, importare intero pacchetto di file
# mettere tutti i file dentro la cartella
  # rlist=list.files(pattern=".png", full.names=T)
# salvare il raster nella lista
# con lappy
  # list_rast=lapply(rlist, raster)
  # EN <- stack(list_rast)
  # plot(EN)

# M.C. guardare il nome del file
rlist <- list.files(pattern=".tif")
rlist 
# M.C. lapply applica un comando ad una serie di dati
list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast) # M.C. diamo un nome all'insieme di tutti i file (stack)
plot(snow.multitemp, col=cl) 

# per vedere l'immagine iniziale e immagine finale
# M.C. si notano le differenze
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
# M.C. valore legenda diverso
# M.C. mettere funzione zlim=c(0-250)   0-250 sono i numeri che vogliamo
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

# fare differenza tra le due immagini
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
# colorRampPalette nuova
cldiff <- colorRampPalette(c('blue','white','red'))(100) # M.C. rosso è massima differenza, blu minima, bianca poco
plot(difsnow, col=cldiff)

# scaricare prediction.r e metterlo nella cartella snow
source("prediction.r")

# scarico immagine da IOL
# 
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
# M.C. funzione lineare si vede una previsione ragionevole
# M.C. "scenari" molto probabile della copertura nevosa
plot(predicted.snow.2025.norm, col=cl)



##################################### 10.
#####################################
#####################################
# R code patches
setwd("C:/lab/")

library(raster)
install.packages("igraph")
library(igraph)
library(ggplot2)

# M.C. caricare dati con due funzioni
# M.C. se raster con tanti livelli si usa "brick"
# M.C. se carichiamo un singolo raster si usa "raster"
# M.C. in questo caso usiamo la funzione "raster"
# M.C. diamo il nome alla prima mappa
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

# M.C. adesso plottiamo le immagini con comando par(mfrow...) cambiando anche colori
par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
# M.C. foresta con colore verde, corretta? no, la foresta in questo caso è colorata di nero
# M.C. la foresta è la classe 2, l'agricoltura è la classe 1
# M.C. bisogna invertire la colorRampPalette per avere la fortesta "verde"
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)

# M.C. mettere per dato agricoltura sia un valore nullo
# M.C. bisogna estrarre soltanto la foresta per fare dei calcoli
# M.C. bisogna "annullare" tutto ciò che non è foresta con funzione "cbind(..., NA)"
# M.C. riclassifica una immagine raster (cambia i valori) con comando reclassify()
# M.C. bisogna togliere il dato "agricoltura" che in questo caso è la classe 1
# M.C. associamo anche un nome
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
# M.C. ora abbiamo solo la foresta
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

# andiamo a creare i patch
# M.C. uniamo i pixel vicini in una singola patch
# M.C. funzione chiamata "clump"
# M.C. la applichiamo sulla prima mappa con la foresta ed associamo un nome
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)
# M.C. se da errore con "igraph" bisogna installare il pacchetto
install.packages("igraph")
library(igraph) # for pacthes
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# M.C. scrivere le mappe per inviarle poi a un ricercatore esterno
# M.C. salvare dati verso l'esterno
# M.C. comando "writeRaster(...)"
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")
# M.C. mi trovo sulla Cartella Lab l'immagine nominata d1c.for.patches.tif

# Esercizio: plottare le immagini una affianco all'altra
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)
# M.C. due patch gigandi: blu e "azzurro"
# M.C. nella seconda mappa queste patch sono frammentate
# M.C. molte piu patch visibili e ciò indica un grande distruzione della foresta
# M.C. per vedere quante patch sono presenti: scrivere il nome "d1c.for.patches"

# risultati plot
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)

library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
# M.C. si vede quante patch c'erano nel 1' periodo e nel 2'
# M.C. nella tabella sono messe in ordine alfabetico quindi sono invertite
# M.C. prima deforestazione=310 patch
# M.C. dopo deforestazione=1212 patch
# M.C. si è persa molta foresta e quella rimasta è stata frammentata ed è molto pericoloso
# M.C. organismi divisi in piccolissimi gruppi e l'estinzione è più probabile

      
      
##################################
##################################
##################################
# 11. R code crop - simulazione d'esame
      
# lapply applica una lista a tutti i file
# M.C. immagini nella cartella snow
# M.C. mettere la cartella snow come setwd
setwd("C:/lab/snow/")
# M.C. si caricano i dati da Copernicus (non solo Copernicus funziona)
# M.C. per fare un trend temporaneo
      
# esercizio: caricare tutte le immagini snow
   # M.C. diverse tecniche: lapply o caricare una immagine alla volta
   library(raster)
   snowlist <- list.files(pattern=".tif")
   snowlist # M.C. vedo cosa c'è dentro, tutti EN_00....ecc

   # M.C. uso la funzione lapply()  : per caricare i dati tutti assieme
   # M.C. applichiamo alla lista "rlist" la funzione "raster". Il tutto è associato ad un nome, "listafinale"
   listafinalesnow <- lapply(snowlist, raster) # M.C. tutti i file sono inseriti in una unica lista
   SNOW <- stack(listafinalesnow) # M.C. EN è il nome

   # M.C. adesso è possibile fare il plot finale con tutte le immagini all'interno
   # M.C. con centinaia di immagini, invece di plottare ogni singolo dato, si può fare in 4 passaggi:
      # M.C. 1. si inseriscono tutti in una cartella.
      # M.C. 2. importare la lista con comando lapply()
      # M.C. 3. compattare con codice "stack()"
      # M.C. 4. inviare il plot
   cl <- colorRampPalette(c('dark blue','blue','white'))(100)
   plot(SNOW, col=cl)
  
   # procedura prof
   rlist <- list.files(pattern="snow")
   rlist 
   list_rast <- lapply(rlist, raster)
   snow.multitemp <- stack(list_rast)  
   clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
   plot(snow.multitemp,col=clb)
      
      
# fare zoom su un preciso dato
# M.C. "find ext" (Ctrl-F) per trovare comandi su Github
# M.C. possibile definire l'estensione mettendo i numero
# M.C.  tra () mettere i valori di X minima e massima, Y minima e massima
# M.C. per vedere i nomi dei vari file: snow.multitemp  
# M.C. se zoom sulla parte italiana:
   # M.C. 1'<- rettangolo su italia
   # M.C. 2'<- definire le coordinate
# esempio Italia cade tra i 6 e 20 gradi (X) e 35 e 50 (Y)
ext <- c(6, 20, 35, 50)
     # M.C. fare zoom dell'intero set/immagine
zoom(snow.multitemp$snow2010r, ext=extension)
   # M.C. rifare il plot dell'immagine originale per riuscire meglio a fare drawExtent
plot(snow.multitemp$snow2010r, col=clb)
zoom(snow.multitemp$snow2010r, ext=drawExtent()) # M.C. bisogna mettere ext=... Mentre in crop no
# M.C. formare un rettangolo sull'immagine. Un solo click per ritagliare la immagine che si vuole
# M.C. partire dal punto in alto a sinistra, tenere premuto, formare il rettangolo, rilasciare il dito e cliccare una volta.
      
# comando "crop"     
# M.C. "crop" taglia l'immagine solo su quella zona
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)
# M.C. abbiamo crato una nuova immagine piu piccola dell'immagine originale
# M.C. stack e la serie multitemporale del periodo analizzato
# M.C. facciamo un crop dell'intero stack
     
# Esercizio: creare un crop dall'intero stack dell'Italia
snow.multitemp.italy <- crop(snow.multitemp, extension)
plot(snow.multitemp.italy, col=clb)
   # M.C. si vedono i dati snow su tutta l'Italia
   # M.C. vedere i range della legenda, sono molto diverse tra loro.
   # M.C. metterle tutte uguali in modo tale che siano comparabili
snow.multitemp.italy    # M.C. per vedere i dati e capire quali sono i valori minimi e massimi della legenda
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))    # M.C. legende uguali e mappe comparabili
# M.C. "zlim" serve a correggere la legenda
# M.C. analisi su tutto lo stack dell'Italia
# M.C. es. boxplot , regressione tra due immagini, previsione ecc..
# M.C. boxplot: si cerca la mediana dei valori intermedie e si vedono le variazioni nel tempo    
# M.C. valori molto fuori dalla mediana li possiamo mettere come outline
boxplot(snow.multitemp.italy, horizontal=T,outline=F)
  # M.C. i valori massimi di copertura nevosa calano. anche la mediana cala. da qui si capisce che la presenza di neve cala nel tempo
  # M.C. abbiamo importato anche una immagine in piu, la previsione, quindi abbiamo valori negativi non realistici
  # M.C. abbiamo preso tutti i file con .tif, quindi anche la previsione
  # M.C. se mettiamo "snow", abbiamo solo le 5 immagini 
 
# M.C. funzione prediction funziona per qualsiasi dato
# M.C. la normalizzazione significa riportare i dati da 0-255 (in questo caso)
  # M.C. contattare in caso di problemi
      
      
      
      
