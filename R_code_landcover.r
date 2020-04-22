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
      
      
 # interpolazione di valori
      
  #per vedere la tabella
 head(covid)
 view(covid) # vedi intera lista di paesi con numero di casi
      
 marks(covids) <- covid$cases
 s <- Smooth(covids)
      
      
      
      

      
      
      
      
      
      
   
