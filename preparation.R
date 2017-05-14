####################################
# Ce fichier (i) charger les données, (ii) les nettoie, 
# (iii) récupère les gps (avec Photon) et 
# (iv) les enregistre dans un .RDS qui va etre lit dans Shiny (global.R).
####################################




# library(raster)
# library(sp)
# library(dismo) 
# library(XML)


#setwd("E:\Data Science\Projet\GÃ©olocalisation")



# Lecture du fichier de donn?es
lieux.latlon<-read.table(file = 'Test.csv',
                         sep = ';', dec = '.',header = T,quote = "")

#lieux.latlon <- lieux.latlon[c(-7,-23),]

# lieux.latlon$ADRESSE <- as.character(lieux.latlon$ADRESSE)
# Encoding(lieux.latlon$ADRESSE) <- "latin1"
# Encoding(lieux.latlon$ADRESSE) <- "uft-8"

# nettoyage de donnees : remplacer les virgules par des espaces
for ( i in 1:nrow(lieux.latlon)) {
  lieux.latlon$ADRESSE<-gsub("[,]"," ",lieux.latlon$ADRESSE)
}


# nettoyage de donnees : suppression des espaces
for ( i in 1:nrow(lieux.latlon)) {
  lieux.latlon$ADRESSE<-gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",lieux.latlon$ADRESSE, perl=T)
}


# Avec Photon ---------

require(devtools)  
devtools::install_github(repo = 'rCarto/photon')
library(photon)


# geocodage
locgeo <- geocode(lieux.latlon$ADRESSE, limit = 1, key = "place")

locgeo$id <- lieux.latlon$ID



saveRDS(locgeo, "locgeo.RDS")
