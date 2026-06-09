
library("sf")
library("tidyverse")
library("dplyr")
library("purrr")
library("stringr")
library("tmap")

################################################################################
# Daten Einlesen:

# Eine liste aller .gpkg Dateien im Ordner erstellen:
gpkg_files <- list.files("GPS Path data/", pattern = "\\.gpkg$", full.names = TRUE)


# Hilfsfunktion: liest einen Layer ein.
f_layer <- function(filename,layer) { #Erstellt eine Funktion mit dem Nahmen f_layer, welche eiun filename und ein layername als Imput braucht.
  st_read(filename, layer = layer, quiet = TRUE) |> # Liest einen Layer ein. 
    mutate(
      quelle_file  = basename(filename), # Fügt den Dateinamen in einer Spalte hinzu.
      quelle_layer  = layer # Fügt den Layername in einer Spalte hinzu.
    )
}

# 2.Hilfsfunktion Liest ein File ein (alle Layer) und erstellt darau ein Datea Frame im long format.
f_files <- function(filename) { #Erstellt eine Funktion mit dem Nahmen f_file welche eiun filename als Imput braucht.
  layer_names <- st_layers(filename)$name # Listet alle Layer auf welche im file enthalten sind.
  map_dfr(layer_names, \(layer) f_layer(filename, layer)) # Nimt die Liste der Layer und führt füe jedes element die Funktion f_layer aus. (\(layer)´weil map_dfr eigendlich eine Funktion mit nur einem Imput erwartet.)
}

# Alle Date Einlesen in eine grosse Tabelle (long Format)
Kuh_Daten <- map_dfr(gpkg_files, f_files) #Führt die Einlese Funktion für alle gpkg Files durch und fügt die Daten in eiener einzigen Tabelle zusammen. 

###############################################################################
###############################################################################
# Datenstruktur 

alle_daten <- Kuh_Daten |>
  mutate(
    Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

###############################################################################
# Konvinience Spalten hinzufügen: 

# RASSE 
alle_daten <- alle_daten |>
  mutate(
    Rasse = str_extract(quelle_file, "(?<=-)[A-Z]{2}")
  )

# ID pro Kuh und Rasse (Bsp.: R1-HO01 = 01)
alle_daten <- alle_daten |> 
  mutate(ID = str_extract(quelle_file, "\\d{2}(?=\\.gpkg)")
  )

# Rasse und ID 
alle_daten <- alle_daten |> 
  mutate(Rasse_ID = str_extract(quelle_file, "(?<=-)\\w+(?=\\.gpkg)")
  )


# Time > HMS = nur hour, minute und seconds
alle_daten <- alle_daten |> 
  mutate(HMS = str_extract(alle_daten$Time, "\\d{2}:\\d{2}:\\d{2}"))


# Spalte mit Versuchsgruppe hinzufügen:
alle_daten$Grupe <- substr(alle_daten$quelle_file, 1, 2)


####################################################################################
####################################################################################
# Datenbereinigung

# Unnötige Layer entfernen:
daten_ber <- alle_daten[!endsWith(alle_daten$quelle_layer, "lns"), ]

# Der datensatz enthält am 14.7 Morgens nur eine Kuh, weglassen: 
daten_ber <- daten_ber[daten_ber$quelle_layer != "07-14-M", ]

# Auf annähernd realistische Zeiten kürzen
start_M <- as_hms("04:30:00")
end_M   <- as_hms("09:00:00")
start_A <- as_hms("14:15:00")
end_A   <- as_hms("19:15:00")

daten_ber  <- daten_ber  |>
  mutate(HMS = as_hms(HMS)) %>% 
  filter(
    (endsWith(quelle_layer, "M") & HMS >= start_M & HMS <= end_M) |
      (endsWith(quelle_layer, "A") & HMS >= start_A & HMS <= end_A)
  )

# Alle Daten die mit weniger alls 4 Sateliten bestimmt wurden Entfernen. (min 4 Sateliten sind für eine genaue position notwendig.) 
daten_ber  <- daten_ber [daten_ber $NSat >"3",]
# NA daten entfernen: 
daten_ber <- daten_ber[!is.na(daten_ber$NSat),]

# Daten Gruppe 1 für die weitera Analyse auswählen: 
daten_ber <- daten_ber [daten_ber$Grupe == "R1",]
# daten_ber <- daten_ber [daten_ber$Grupe == "R2",]
# daten_ber <- daten_ber [daten_ber$Grupe == "R3",]

# Daten entfernen mit unrelistischen geshwindikeiten: 
# Hilfsfunktionen Definieren:
distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

# Unrelistische ditanzen ZwischenPunkten entfernen (20s für 200m, mehr als 30Kmh im alpinengelände, )
repeat {
  n <- nrow(daten_ber)
  cat("Punkte:", n, "\n")  # zeigt Fortschritt
  
  daten_ber <- daten_ber |>
    group_by(quelle_layer, Rasse_ID) |>
    mutate(
      steplength = distance_by_element(lead(geom), geom)
    ) |>
    filter(
      (is.na(steplength)      | steplength      < 200) &
      (is.na(lag(steplength)) | lag(steplength) < 200) 
    )
  if (nrow(daten_ber) == n) break
}


#####################################################
#####################################################
###### Weg definieren: ############################

# Wanderweg definieren: 
Wander_Weg <-  daten_ber |>
  filter(quelle_layer == "06-25-A" & #Erste Kuh die diesen Weg genommen hat dient als grundlage
         Hour < 17 &
         Rasse_ID == "HO01"
         ) %>% 
  ungroup() %>% 
  arrange(Time) %>%
  summarise(geometry = st_cast(st_combine(geom), "LINESTRING"))

plot(Wander_Weg)

# Wanderweg definieren: 
Strasse <-  daten_ber |>
  filter(quelle_layer == "06-27-M" & #Erste Kuh die diesen Weg genommen hat dient als grundlage
           Hour < 7&
           Rasse_ID == "HO01"
  ) %>% 
  ungroup() %>% 
  arrange(Time) %>%
  summarise(geometry = st_cast(st_combine(geom), "LINESTRING"))



plot(Strasse$geometry)
plot(Wander_Weg$geometry, col = "red", add = TRUE)

plot(st_union(Strasse$geometry, Wander_Weg$geometry))  # setzt Extent auf beide zusammen
plot(Strasse$geometry, add = TRUE)
plot(Wander_Weg$geometry, col = "red", add = TRUE)


daten_ber |>
  ggplot() +
  geom_sf(aes(color = quelle_layer))


st_transform(4326) %>%
  filter(st_coordinates(geom)[, 1] < 9.800,  # Start und Ende aus Karte raus gelesen.
         st_coordinates(geom)[, 1] > 9.7885,) %>%
  st_transform(2056) #%>% 









