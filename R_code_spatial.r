# R spaziale : funzioni spaziali in Ecologia del Paesaggio

install.packages("sp")

# richiamo il pacchetto sp 
library(sp)

# richiamo i dati "meuse"
data(meuse)

head(meuse)

#plot cadium e lead, devo allegare database

attach(meuse)

#plotto e coloro e uso diverso carattere(pch=point character) e aumento dimensione(cex=character exageration)

plot(cadmium,lead,col="red",pch=19,cex=2)

# exercise : plot di copper e zinco con simbolo triangolo e colore verde

plot(copper,zinc,col="green",pch=17,cex=2)

# " " le uso quando ho un testo

# cambiare le etichette nel grafico (x e y label)

plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinco")

# multiframe (per mettere piu di un grafico nella stessa finestra), par è la funzione (poi decido se i grafici li volgio in riga o in colonna)
# c(1,2) = una riga e due colonne , nele due colonne ho messo i due grafici sullo stesso piano, la stessa riga
# sotto copio e incollo i due grafici da unire , (puo essere utile per es.analisi multitemporale (t0 e t1))

par(mfrow=c(1,2))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# adesso due righe e una colonna

par(mfrow=c(2,1))
plot(copper,zinc,col="green",pch=17,cex=2)
plot(cadmium,lead,col="red",pch=19,cex=2)

# multiframe automatico
install.packages("GGalli")

# prendo il dataset Meuse e lo mando nella funzione scaricata "ggpairs"
# [,3:6] = dalla terza colonna alla sesta colonna per avere tutti gli elementi (li vedo da head(meuse))
# grafico= sull asse diagonale le singole variabili e la distribuzione dei dati 
# coefficienti di correlazione, 0.92 = molto correlati

library(GGally)
ggpairs(meuse[,3:6])

# Spatial, devo spiegare a R che "meuse" ha delle coordinate x e y
# head(meuse) vedo le coordinate

head(meuse)

# inserire per primo il dataset e poi speigo che ho x y , gli ho spiegato quali sono le mie coordinate
# uso ~ per gruppo di coordinate

coordinates(meuse)=~x+y

plot(meuse)

# inserisco nella funzione il dataset per creare grafico spaziale grazie a spplot (plotto i dati spazialmente)
# grafico= come si ditribuisce lo zinco attorno al fiume, vicino all'acqua (giallo) valori molto alti, lontani dal fiume puntini neri valore basso

spplot(meuse,"zinc")
