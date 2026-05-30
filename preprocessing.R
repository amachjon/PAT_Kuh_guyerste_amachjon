
library("sf")
library("tidyverse")
library("dplyr")
library("purrr")
library("stringr")
library("tmap")

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

###############################################################################
# Datenstruktur 
# 
alle_daten

alle_daten <- alle_daten |>
  mutate(
    Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

###############################################################################
# Spalten hinzufügen: RASSE 

alle_daten <- alle_daten |>
  mutate(
    Rasse = stringr::str_extract(quelle_file, "(?<=-)[A-Z]{2}")
  )

###############################################################################

# Daten verstehen: 
View(alle_daten)
unique(alle_daten$quelle_file)

# Weg aufzeichnen von einer Kuh am 25.6. am Abend: 

R1_HO01_06_25_A <-  alle_daten |>
  filter(quelle_file == "R1-HO01.gpkg",
         TimeSlice == "06-25-A")

R1_HO01_06_25_A |> 
  ggplot() + 
  geom_sf()

# mit tmap 

tmap_mode("view")

tm_shape(R1_HO01_06_25_A) + 
  tm_dots()

# Herausfinden ob die Tiere der verschiedenen Rassen zusammen zum Melkstand getrieben werden oder separat. 

View(alle_daten)

Filter_1 <- alle_daten |>
  filter(Time >= ymd_hms("2025-06-25 15:35:00"),
         Time <= ymd_hms("2025-06-25 15:37:00")) 
  
Filter_1 |>
  ggplot() +
  geom_sf(aes(color = Rasse))

View(Filter_1)

# Distanz berechnen zu einem definierten Referenzpuntk: 

ref_punkt <- st_sfc(st_point(c(2780425, 1161875)), crs = 2056)

Filter_1 <- Filter_1 |> 
  mutate(dist_to_ref = as.numeric(st_distance(geom, ref_punkt)))

View(Filter_1)

Filter_1 |> 
  ggplot(aes(Rasse, dist_to_ref)) + 
  geom_boxplot()




