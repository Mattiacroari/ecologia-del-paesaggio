# libreria sp
library(sp)

# dati da usare
data(meuse)
head(meuse)       controlla cosa c'è dentro al dataset

#coordinate del dataset (dataframes in R)
coordinates(meuse)=~x+y

# spplot dei dati di zinco, tabella della distribuzione 
spplot(meuse,"zinc")

# Exercise: spplot dei dati di rame
head(meuse)      per capire come si chiamano i nomi delle colonne
names(meuse)    come sopra

spplot(meuse,"copper")

# bubble per plottare i dati
bubble(meuse,"zinc")

# Exercise: bubble del rame, colore rosso
bubble(meuse,"copper",col="red")

# foraminiferi (Sofia), carbon capture (Marco)
# array             
foram <- c(10, 20, 35, 55, 67, 80)         array o vettore--- per inserire dataset in R, si è creato un oggetto
carbon <- c(5, 15, 30, 70, 85, 99)         significa che la serie di numeri si chiamano carbon e foram
plot(foram, carbon,col="green", cex=2, pch=11)

# Dati dall'esterno (es. tabella su Covid-19)
# cartella da creare su Windows: C:/lab     specifica il percorso 
# Windows:   setwd("C:/lab")

# funzione per leggere una tabella, nominata covid
covid <- read.table("covid_agg.csv",head=TRUE)
