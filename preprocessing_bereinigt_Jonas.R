
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

# Unrelistische ditanzen ZwischenPunkten entfernen (20s für 200m, mehr als 30Kmh im alpinengelände / oder zulange keine Messung das es aussagekräftig ist)
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
  filter(is.na(steplength) | steplength > 5) %>%   # mind. 5m Bewegung / Weg glätten
  summarise(geometry = st_cast(st_combine(geom), "LINESTRING"))

plot(Wander_Weg)

# Strasse definieren: 
Strasse <-  daten_ber |>
  filter(quelle_layer == "06-27-M" & #Erste Kuh die diesen Weg genommen hat dient als grundlage
           Hour < 7&
           Rasse_ID == "HO01"
           ) %>% 
  ungroup() %>% 
  arrange(Time) %>%
  filter(is.na(steplength) | steplength > 5) %>% 
  summarise(geometry = st_cast(st_combine(geom), "LINESTRING"))

# Hilfsfunktion für Senkrechte Linien: 
senkrechte_linie <- function(weg_coords, idx, laenge = 100, fenster = 15) {
  idx1 <- idx - fenster # Punkte am anfuang und ende das Fensters ermitteln.
  idx2 <- idx + fenster
  dx <- weg_coords[idx2, 1] - weg_coords[idx1, 1] #Veränderung in x und y richtung feststellen.
  dy <- weg_coords[idx2, 2] - weg_coords[idx1, 2]
  len <- sqrt(dx^2 + dy^2) # Länge des Vektors ermitteln.
  perp_x <- -dy / len # Vektor drehung und normierung auf Länge 1. 
  perp_y <-  dx / len
  mx <- weg_coords[idx, 1] #Position der Line in der mitte des Fensters festlegen. 
  my <- weg_coords[idx, 2]
  st_linestring(rbind( # Linie einfügen
    c(mx - perp_x * laenge/2, my - perp_y * laenge/2),
    c(mx + perp_x * laenge/2, my + perp_y * laenge/2)
  ))
}

# Koordinaten System transformieren: 
str_coords      <- st_coordinates(Strasse$geometry[[1]])
str_coords_4326 <- st_coordinates(st_transform(Strasse, 4326))

# Koordinaten für Start und Ende des Weges ab Karte festgelegt:
S_E <- c(9.7885,9.8000)
#Nächster Punkt zu dieser Koordinate finden: 
S_E_idx  <- sapply(S_E, function(d) which.min(abs(str_coords_4326[, 1] - d)))

# Start und Endline genirieren: 
Start_Ende <- lapply(seq_along(S_E_idx), function(i) {
  senkrechte_linie(str_coords, S_E_idx[i], laenge = 500, fenster = 15)
}) %>%
  st_sfc(crs = st_crs(2056)) %>%
  st_sf(S_E = 1:2, geometry = .)

# Hilfs Funktion zum Messlinien erstellen:
mess_linien_erstellen <- function(weg, lon_min = 9.7885, lon_max = 9.8000, n = 5, laenge = 100, fenster = 5) {
  # Wählt nur den weg aus:
  weg_gefiltert <- weg |>
    st_transform(4326) |>
    st_cast("POINT") |>
    filter(st_coordinates(geometry)[, 1] < lon_max,
           st_coordinates(geometry)[, 1] > lon_min) |>
    st_transform(2056)
  #Liest die Kordinaten aus
  weg_coords  <- st_coordinates(weg_gefiltert$geometry)
  # Berechenet die Kummulierte distanz:
  kum_dist    <- c(0, cumsum(sqrt(diff(weg_coords[,1])^2 + diff(weg_coords[,2])^2)))
  gesamt_dist <- max(kum_dist)
  #Rechnet die Abschnittlängen aus: 
  ziel_dist <- seq(gesamt_dist / (n+1), gesamt_dist * n/(n+1), length.out = n)
  mess_idx  <- sapply(ziel_dist, function(d) which.min(abs(kum_dist - d)))
  #Erstellt die Linien: 
  lapply(seq_along(mess_idx), function(i) {
    senkrechte_linie(weg_coords, mess_idx[i], laenge = laenge, fenster = fenster)
  }) %>%
    st_sfc(crs = st_crs(weg)) %>% # Setzt das richtige koordinatensystem
    st_sf(messline = 1:n, geometry = .) #Erstellt eine Liste von geometrie Objekten. 
}

# Messlinien erstellen: 
ww_messline <- mess_linien_erstellen(Wander_Weg)

str_messline <- mess_linien_erstellen(Strasse)

# Weg + Start und Ende und Messlinein Plotten. 
tmap_mode("view")

tm_shape(Wander_Weg)    + tm_lines(col = "black") +
  tm_shape(Strasse)       + tm_lines(col = "blue")  +
  tm_shape(Start_Ende)    + tm_lines(col = "green") +
  tm_shape(ww_messline)   + tm_lines(col = "red")   +
  tm_shape(str_messline)  + tm_lines(col = "darkgreen")



##############################################################################################


