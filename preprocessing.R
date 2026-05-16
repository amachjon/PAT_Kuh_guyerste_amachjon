
library(sf)

# Daten einlesen: 

Einl_gpkg <- function(file){
 st_read(paste0("GPS path data/",file)) 
}

k<-Einl_gpkg("R1-HO03.gpkg")

Kuh <- st_layers("GPS path data/R1-HO03.gpkg")


Einl_gpkg_alle <- function(file){
  pfad <- paste0("GPS path data/", file)
  layer_namen <- st_layers(pfad)$name
  
  alle_layer <- lapply(layer_namen, function(l){
    st_read(pfad, layer = l, quiet = TRUE)
  })
  
  names(alle_layer) <- layer_namen
  return(alle_layer)
}


#Liste — jeder Layer als eigenes Element:
daten <- Einl_gpkg_alle("R1-HO03.gpkg")
  
# Einzelnen Layer ansprechen:
daten[["06-25-M"]]

# Alle Layer-Namen anzeigen:
names(daten)