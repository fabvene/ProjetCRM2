library(raster)
library(sp)
library(dismo) 
library(XML)


#setwd("E:\Data Science\Projet\Géolocalisation")



# Lecture du fichier de donn?es
lieux.latlon<-read.table(file = 'Test.csv',sep = ';', dec = '.',header = T,quote = "")


# nettoyage de donnees : remplacer les virgules par des espaces
for ( i in 1:nrow(lieux.latlon)) {
  lieux.latlon$ADRESSE<-gsub("[,]"," ",lieux.latlon$ADRESSE)
}


# nettoyage de donnees : suppression des espaces
for ( i in 1:nrow(lieux.latlon)) {
  lieux.latlon$ADRESSE<-gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",lieux.latlon$ADRESSE, perl=T)
}



# On cr?e une variable vide, qui va contenir les requ?tes
requetes <- NULL

# Traitement de chacune des adresses
for(i in 1:nrow(lieux.latlon)) {
  
  # Lit l'adresse, et remplace les espaces par "+" (syntaxe de l'API)
  lieu <- lieux.latlon[i, "ADRESSE"]
  lieu <- gsub("\\s+", "+", lieu)
  
  # Construction de la requ?te
  requetes[i] <- paste("http://maps.googleapis.com/maps/api/geocode/xml?address=",
                       lieu,
                       "&sensor=false",
                       sep="") 
}

# Variable dans laquelle on va stocker les r?sultats
lieux.latlon.latlon <- NULL

# Traitement de chacune des requ?tes
# - ATTENTION : prend environ 10 minutes ici
for(i in 1:length(requetes)) {
  
  # Envoie la requ?te, re?oit la r?ponse, analyse sa structure
  reponse.xml <- readLines(requetes[i])
  reponse <- xmlTreeParse(reponse.xml, useInternalNodes=TRUE)
  
  # Traitement des r?sultats ; on ne prend que
  # la premi?re r?ponse (result[1] dans ce qui suit)
  
  # On initialise comme NA
  latitude <- NA
  # Si la valeur existe, remplace NA (sinon reste NA)
  latitude <- xmlValue(reponse[["//result[1]//geometry/location/lat"]])
  
  # Longitude : idem
  longitude <- NA 
  longitude <- xmlValue(reponse[["//result[1]//geometry/location/lng"]]) 
  
  # Champ adresse de la r?ponse : idem
  adresse.recue <- NA 
  adresse.recue <- xmlValue(reponse[["//result[1]//formatted_address"]]) 
  
  # Mise des r?sultats ensemble, sous forme de dataframe
  latlon.etc <- cbind(lieux.latlon[i,], requetes[i], latitude, longitude, adresse.recue) # champ de test
  lieux.latlon.latlon <- rbind(lieux.latlon.latlon, latlon.etc)
  
  # Pause de 3 secondes - courtoisie envers le serveur 
  Sys.sleep(1)
  
}



# Calculate distance in kilometers between two points


earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

earth.dist(2.3517955,48.8318795,2.3214292,48.8918556)

#ajout row names 
row.names(lieux.latlon.latlon)<-lieux.latlon.latlon$ID

#retourne latitude et longitude ? partir de la r?f?rence
ref<-function(x){
  coord<-lieux.latlon.latlon[x,c("latitude","longitude")]
  
  return(coord)
}
ref("7688759")

#retourne latitude ? partir de la r?f?rence
latref<-function(x){
  coord<-lieux.latlon.latlon[x,"latitude"]
  
  return(coord)
}

latref("7688759")

#retourneongitude ? partir de la r?f?rence
longref<-function(x){
  coord<-lieux.latlon.latlon[x,"longitude"]
  
  return(coord)
}

longref("7688759")


lieux.latlon.latlon$ref<-("7688759")
lieux.latlon.latlon$longref<-longref("7688759")
lieux.latlon.latlon$latref<-latref("7688759")



#boucle calcul distance

for ( i in 1:16) {
  lieux.latlon.latlon$distance<-earth.dist(as.numeric(as.character(lieux.latlon.latlon$longitude)),as.numeric(as.character(lieux.latlon.latlon$latitude)),as.numeric(as.character(lieux.latlon.latlon$longref)),as.numeric(as.character(lieux.latlon.latlon$latref)))
}


#tri ds nouvelle table
new.table<-class(lieux.latlon.latlon$distance)
new.table<-lieux.latlon.latlon[order(lieux.latlon.latlon[,10],decreasing=F), ]

# selection des ref ? moins de 1 km

new.table2<-subset(new.table,distance<1)


# Avec Photon ---------

require(devtools)  
devtools::install_github(repo = 'rCarto/photon')
library(photon)


# Lecture du fichier de données
lieux.latlon<-read.table(file = 'Test.csv',sep = ';', dec = '.',header = T,quote = "")


# geocodage
locgeo <- geocode(lieux.latlon$ADRESSE, limit = 1, key = "place")

locgeo$Id <- lieux.latlon$ID


# affichage des résultats
# lieux.latlon$locgeo<-locgeo[,c("location", "lon", "lat")]


# Plot avec ggmap ------------

library(ggmap)
map.Paris <- get_map(c(lon=2.35,lat=48.86), zoom =12, source = "google", maptype = "roadmap")
ggmap(map.Paris)
map.Paris <- ggmap(map.Paris, extent = "device")
map.Paris + geom_point(data = locgeo, aes(x = lon, y = lat))

# Plot avec Leaflet ------------

library(leaflet)

locgeo <- locgeo[-which(locgeo$Id == "6395679"),]

leaflet(data = locgeo) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup=~paste("Id: ",Id, " || Adresse: ",location,street, postcode))


# Distance avec Geosphere ----------

library(geosphere)
distm (c(locgeo$lon[1], locgeo$lat[1]), c(locgeo$lon[9], locgeo$lat[9]), fun = distHaversine)

distances <- NULL
for(i in 1:nrow(locgeo)){
  
  distances[i] <- distm (c(locgeo$lon[1], locgeo$lat[1]), c(locgeo$lon[i], locgeo$lat[i]), fun = distHaversine)
}

head(distances,5)

# Fonctions

# Fonction avec point départ (num-row) et distance maximale. (sur base locgeo) ###################
myfunc <- function(depart,maxdist){
  for(i in 1:nrow(locgeo)){
    
    distances[i] <- distm (c(locgeo$lon[depart], locgeo$lat[depart]), c(locgeo$lon[i], locgeo$lat[i]), fun = distHaversine) /1000
  }
  
  ids <- which(distances<maxdist)
  return(locgeo$location[ids] )

  
}

myfunc(1,2)
#####################################################################################################

#depart=1
#maxdist <- 2000
distances <- NULL


# Fonction avec point départ (num-row) et distance maximale. (sur base lieux.latlon) ###################
myfunc <- function(depart,maxdist){
  for(i in 1:nrow(lieux.latlon)){
    
    distances[i] <- distm (c(as.numeric(as.character(lieux.latlon$longitude[depart])), as.numeric(as.character(lieux.latlon$latitude[depart]))), 
                           c(as.numeric(as.character(lieux.latlon$longitude[i])), as.numeric(as.character(lieux.latlon$latitude[i]))), fun = distHaversine)
  }
  
  ids <- which(distances<maxdist)
  return(lieux.latlon$location[ids] )
  
  
}

myfunc(1,2000)
#####################################################################################################




# Meme fonction mais avec adresse-départ
myfunc2 <- function(adresse_depart,maxdist){
  for(i in 1:nrow(locgeo)){
    
    distances[i] <- distm (c(locgeo$lon[locgeo$location == adresse_depart], locgeo$lat[[locgeo$location == adresse_depart]]), 
                           c(locgeo$lon[i], locgeo$lat[i]), fun = distHaversine)
  }
  
  return(distances[distances<maxdist])
  
}

myfunc2("5 R DES RECULETTES 75013 PARIS",1000)

############## calculer matrice toutes distances (pas utile?) ####
mydist <- distm(locgeo[,c('lon','lat')])
distm(locgeo[1:3,c('lon','lat')])
##################################################################





