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






