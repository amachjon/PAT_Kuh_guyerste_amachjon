
library(sf)

library("sf")
library("dplyr")
library("purrr")
library(stringr)

# Eine liste aller .gpkg Dateien im Ordner erstellen:
gpkg_files <- list.files("GPS Path data/", pattern = "\\.gpkg$", full.names = TRUE)


# Hilfsfunktion: liest einen Layer ein.
f_layer <- function(file_layer) { #Erstellt eine Funktion mit dem Nahmen f_layer welche eiun filename und ein layername als Imput braucht.
  split <- str_split(file_layer, " _;_ ",simplify = TRUE)  
  file <- split [,1]
  layer <- split [,2]
  
  st_read(file, layer = layer, quiet = TRUE) |> # Liest einen Layer ein. 
    mutate(
      quelle_file  = basename(file), # Fügt den Dateinamen in einer Spalte hinzu.
      quelle_layer  = layer # Fügt den Layername in einer Spalte hinzu.
    )
}

f_files <- function(file) {
  layer_names <- st_layers(file)$name
  file_layer <- paste(file,"_;_",layer_names)
  map_dfr(file_layer, f_layer)
}

alle_daten <- map_dfr(gpkg_files, f_files)