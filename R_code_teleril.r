# Codice R per analisi di immagini satellitari

# packages: raster
install.packages("raster")
library(raster)    # richiamo dati nel pacchetto raster

setwd("C:/lab/")

# ... <- brick prende una immagine dentro una cartella e le da un nome
p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)



