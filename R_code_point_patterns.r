# Codice per analisi dei point patterns, dati legati ai punti
install.packages("ggplot2") # per installare un pacchetto
install.packages("spatstat")

library(ggplot2)
library(spatstat)

setwd("C:/lab")     

covid <- read.table("covid_agg.csv", head=T)  # per importare dati di una tabella

head(covid)

plot(covid$country,covid$cases)    # $ collega un pezzo ad un altro, in questo caso collega la colonna al proprio dataset altrimenti non riconosce la colonna
plot(covid$country,covid$cases,las=0)  # per metterlo verticale
plot(covid$country,covid$cases,las=1)  # las=1 le etichette di asse Y diventano orizzontali
plot(covid$country,covid$cases,las=2)  # las=2 le etichette della asse X sono verticali
plot(covid$country,covid$cases,las=3)  # laberls verticali

plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5) # per rimpicciolire le scritte di asse X

# ggplot2
data(mpg)
head(mpg)

# data
# aes
# tipo di geometria
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()

# ggplot di covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()     # size=cases dimensione variabile in base ai dati


# density intende la densità dei punti in una mappa
# create dataset for spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

d <- density(covids)
plot(d)
points(covids)

# save the .RData


setwd("C:/lab")  # recuperiamo i dati all'interno della cartella Lab
load("point_pattern.RData")
ls()    # per vedere cosa c'è dentro il file
library(spatstat)

plot(d)

# cambiare colori nella mappa --> palette
cl <- colorRampPalette(c('yellow','orange','red')) (100)  # cl è il nome. (100) indica il numero di gradazioni di colore, che vanno dal yellow al red
plot(d, col=cl)  # creare grafico di "d" con i colori di "cl"

# Exercise: plot della mappa della densita dal verde al blu
cl1 <- colorRampPalette(c('yellow','green','blue')) (100)
points(covids)  # inserire punti sulla mappa

# 
coastlines <- readOGR("ne_10m_coastline.shp") # se da errore può mancare la libreria, non devono essere dentro ad un'altra cartella
install.packages("rgdal")   # installare il pacchetto
library(rgdal)
    # a questo punto posso rimandare il comando
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# Exercise: plot della mappa di densità con una nuova colorazione, e aggiunta delle coastlines
cl <- colorRampPalette(c('lightblue','blue','black')) (100)
plot(d, col=cl)
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)


