
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
###############################################################################
###############################################################################
# Datenstruktur 
# 
alle_daten

alle_daten <- alle_daten |>
  mutate(
    Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

###############################################################################
# Spalten hinzufügen: 

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

View(alle_daten)

# R1, R2, R3 extrahieren

alle_daten <- alle_daten |> 
  mutate(Zeitperiode = str_extract(quelle_file, "R1|R2|R3"))

###############################################################################

# Daten verstehen: 
View(alle_daten)
unique(alle_daten$quelle_file)
length(unique(alle_daten$quelle_file))
unique(alle_daten$Rasse_ID)

alle_daten |> 
  filter(str_detect(alle_daten$quelle_file, "R2")) |> 
  pull(TimeSlice) |> 
  unique()

alle_daten |> 
  filter(str_detect(alle_daten$quelle_file, "R1")) |> 
  pull(TimeSlice) |> 
  unique()

alle_daten |> 
  filter(str_detect(alle_daten$quelle_file, "R3")) |> 
  pull(TimeSlice) |> 
  unique()

# Berechnung Dauer Weg: 
Dauer_Weg <- alle_daten %>%
  group_by(quelle_file, TimeSlice) %>%
  summarise(duration_min = as.numeric(difftime(max(Time), min(Time), units = "mins")))

View(Dauer_Weg)
mean(Dauer_Weg$duration_min, na.rm = TRUE)

# Weg aufzeichnen von einer Kuh am 25.6. am Abend als erste Visualisierung: 

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
#################################################################################
#################################################################################
#################################################################################
# Einlesen der Messlinien, welche in ArcGISpro erstellt wurden: 

messlinien <- st_read("Messlinien.shp")
messlinien

st_crs(messlinien) <- 2056

#################################################################################
#################################################################################
#################################################################################
# Berechnungen zur Durchgangsreihenfolge (in welcher Reihenfolge passieren die Tiere die beiden Linien auf dem Weg?) > Was ist die Nächste Zeit die die Tiere bei der Linie haben? > Unten die nächste Distanz berechnet der Kühe jeweiligen Linien > Zeitausgabe hilft dann die Reihenfolge aufzustellen

# Berechnung des nächsten Punktes zu den beiden Messlinien pro Kuh und Weg den sie zurücklegen

dist_l1 <- alle_daten %>% 
  mutate(dist_l1 = st_distance(geom, messlinien[1, ])) %>% 
  group_by(quelle_file, quelle_layer) %>% 
  slice_min(dist_l1, n = 1) %>% 
  ungroup() %>% 
  mutate(messlinie = 1)

dist_l2 <- alle_daten %>% 
  mutate(dist_l2 = st_distance(geom, messlinien[2, ])) %>% 
  group_by(quelle_file, quelle_layer) %>% 
  slice_min(dist_l2, n = 1) %>% 
  ungroup() %>% 
  mutate(messlinie = 2)

dist_time <- bind_rows(dist_l1, dist_l2)

View(dist_time)

View(dist_time)

# Nun RANKEN

dist_time_ranked <- dist_time |> 
  group_by(quelle_layer, messlinie) |>
  arrange(HMS) |> 
  mutate(rank = row_number()) |> 
  ungroup()

View(dist_time_ranked)


# Visualisierung zur Überprüfung ob die Kuh die am die den tiefsten Rank hat (= 1) auch wirklich zur Frühsten Zeit am nächsten bei den Linien war. 

View(dist_time_ranked |> 
  filter(quelle_layer == "07-17-M" & messlinie == "1")) # nur zur Überprüfung ob es die Reihenfolge korrekt genommen hat 


#################################################################################
#################################################################################
#################################################################################
# Visualisierungen 


# für PPP: Kühe an einemn Bestimmten Zeitpunkt: 25-08-07, 5:05

zielzeit <- as.POSIXct("2025-08-07 05:10:00", tz = "UTC")

punkte_zeit <- alle_daten %>%
  mutate(
    diff_sec = abs(as.numeric(Time - zielzeit))
  ) %>%
  group_by(quelle_file) %>%       
  slice_min(diff_sec, n = 1) %>%   
  ungroup()

ggplot() +
  geom_sf(data = messlinien, color = "red", size = 1.2) +
  geom_sf(data = punkte_zeit, aes(color = Rasse_ID), size = 3) 







###################################
###################################

library("sf")
library("tidyverse")
library("dplyr")
library("purrr")
library("stringr")
library("tmap")
library("lubridate")
library("zoo")

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

# Spalte mit Tageszeit: 
alle_daten$Tageszeit <- substr(alle_daten$quelle_layer,7,7)

# Hilfsfunktionen Definieren:
distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

# Distanz zum Nächsten Punkt berechenen
alle_daten <- alle_daten |>
  group_by(quelle_layer, Rasse_ID) |>
  arrange(Time, .by_group = TRUE) |>
  mutate(
    steplength = mapply(distance_by_element, geom, lead(geom))
  )

#####################################################
#####################################################
###### Weg definieren: ############################

Wander_Weg <- alle_daten |>
  filter(quelle_layer == "06-25-A" & #Erste Kuh die diesen Weg genommen hat dient als grundlage
           Hour < 17 &
           Rasse_ID == "HO01"
  ) |>
  ungroup() |>
  arrange(Time) |>
  filter(is.na(steplength) | steplength > 5) |> # mind. 5m Bewegung
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2],
    x = rollmean(x, k = 3, fill = "extend"),  # Weg glätten (mittelwert über ein fenster von 3 bilden)
    y = rollmean(y, k = 3, fill = "extend")
  ) |>
  st_drop_geometry() |>
  st_as_sf(coords = c("x", "y"), crs = 2056) |>
  summarise(geometry = st_cast(st_combine(geometry), "LINESTRING"))


Strasse <- alle_daten |>
  filter(quelle_layer == "06-27-M" & #Erste Kuh die diesen Weg genommen hat dient als grundlage
           Hour < 7&
           Rasse_ID == "HO01"
  ) |>
  ungroup() |>
  arrange(Time) |>
  filter(is.na(steplength) | steplength > 5) |> # mind. 5m Bewegung
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2],
    x = rollmean(x, k = 3, fill = "extend"),   # Weg glätten
    y = rollmean(y, k = 3, fill = "extend")
  ) |>
  st_drop_geometry() |>
  st_as_sf(coords = c("x", "y"), crs = 2056) |>
  summarise(geometry = st_cast(st_combine(geometry), "LINESTRING"))

# Hilfsfunktion für Senkrechte Linien: 
senkrechte_linie <- function(weg_coords, idx, laenge = 100, fenster = 3) {
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
S_E <- c(9.7895,9.8000)

#Nächster Punkt zu dieser Koordinate finden: 
S_E_idx  <- sapply(S_E, function(d) which.min(abs(str_coords_4326[, 1] - d)))

# Start und Endline genirieren: 
Start_Ende <- lapply(seq_along(S_E_idx), function(i) {
  senkrechte_linie(str_coords, S_E_idx[i], laenge = 800, fenster = 3)
}) %>%
  st_sfc(crs = st_crs(2056)) %>%
  st_sf(S_E = 1:2, geometry = .)

# Hilfs Funktion für Messlinien erstellen:
mess_linien_erstellen <- function(weg, lon_min = S_E[1], lon_max = S_E[2], n = 5, laenge = 100, fenster = 2) {
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
  mess_idx  <- sapply(ziel_dist, function(d) which.min(abs(kum_dist - d))) # Wählt das nächste Element aus. 
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

############################################################
############################################################
# Datenbereinigung:

# Unnötige Layer entfernen:
daten_ber <- alle_daten[!endsWith(alle_daten$quelle_layer, "lns"), ]

# Der datensatz enthält am 14.7 Morgens nur eine Kuh, weglassen: 
daten_ber <- daten_ber[daten_ber$quelle_layer != "07-14-M", ]

# Daten Gruppe 3 für die weitera Analyse auswählen: 
# daten_ber <- daten_ber [daten_ber$Grupe == "R1",]
# daten_ber <- daten_ber [daten_ber$Grupe == "R2",]
daten_ber <- daten_ber [daten_ber$Grupe == "R3",]

# Alle Daten die mit weniger alls 4 Sateliten bestimmt wurden Entfernen. (min 4 Sateliten sind für eine genaue position notwendig.) 
daten_ber  <- daten_ber [daten_ber $NSat >"3",]
# NA daten entfernen: 
daten_ber <- daten_ber[!is.na(daten_ber$NSat),]

# Hilfsfunktion definieren: 
difftime_secs <- function(later, now) {
  as.numeric(difftime(later, now, units = "secs"))
}

# Hilofsfunktion um ein segment aus 2 Punkten als Linestring zu definieren: (Um das auf den Punkt folgende segment zu bestimmen) 
segment <- function(p1, p2) {
  tryCatch(
    st_linestring(rbind(st_coordinates(p1), st_coordinates(p2))),
    error = function(e) st_linestring()  # falls Fehler → leere Linie zurückgeben
  )
}

# Zu hohe Geschwindikeiten raus filtern (Kuh nicht schneller als 25 Kmh im Alpinen gelände): 
repeat { #Repetiert das ganze fals ausreisser zu nahe aneinander liegen um erfasst zu werden. 
  n <- nrow(daten_ber)
  cat("Punkte:", n, "\n")
  
  daten_ber <- daten_ber |>
    group_by(quelle_layer, Rasse_ID) |>
    mutate(
      timelag    = difftime_secs(lead(Time), Time),
      segment = st_sfc(mapply( segment, geom, lead(geom), SIMPLIFY = FALSE), #Nach Punkt folgendes Segment generieeren und Länge davon berechnen. 
                       crs = st_crs(daten_ber)),
      steplength = as.numeric(st_length(segment)),
      speed      = steplength / timelag
    ) |>
    filter(
      (is.na(speed)      | speed      < 7) &
        (is.na(lag(speed)) | lag(speed) < 7)
    )
  if (nrow(daten_ber) == n) break
}

# Start und End der Wanderung pro Tier feststellen:  
S_E_Zeit_pro_Kuh <- daten_ber |>
  mutate(
    crosses_start = lengths(st_intersects(segment, Start_Ende[1, ])) > 0,
    crosses_end   = lengths(st_intersects(segment, Start_Ende[2, ])) > 0
  ) %>% 
  filter(crosses_start | crosses_end) %>% # Nur einträge behalten die mindestens 1 = True haben
  st_drop_geometry() %>% 
  group_by(quelle_layer, Rasse_ID) |>
  arrange(Time, .by_group = TRUE) |>
  mutate(
    start_hin_Weg = crosses_start & lead(crosses_end, default = FALSE),
    start_rück_Weg = crosses_end & lead(crosses_start, default = FALSE),
    end_hin_Weg  = lag(start_hin_Weg,  default = FALSE),
    end_rück_Weg = lag(start_rück_Weg, default = FALSE)
  )  

# Start und End Zeitpunkte pro Layer feststellen: 
S_E_Zeit <- S_E_Zeit_pro_Kuh |>
  group_by(quelle_layer) |>
  summarise(
    start_hin  = min(Time[start_hin_Weg],  na.rm = TRUE),
    ankunft_hin  = max(Time[end_hin_Weg],    na.rm = TRUE),
    start_rück = min(Time[start_rück_Weg], na.rm = TRUE),
    ankunft_rück = max(Time[end_rück_Weg],   na.rm = TRUE),
    .groups = "drop"
  )

# Filtern nach Zeit in der die Kühe auf dem Weg Wahren (mit 2Minuten Buffer)
daten_weg <- daten_ber |>
  left_join(S_E_Zeit, by = "quelle_layer") |>
  mutate(
    auf_hinweg  = Time >= (start_hin  - minutes(2)) & Time <= (ankunft_hin  + minutes(2)),
    auf_rückweg = Time >= (start_rück - minutes(2)) & Time <= (ankunft_rück + minutes(2))
  ) |>
  filter(auf_hinweg | auf_rückweg) |>
  mutate(
    richtung = case_when(
      auf_hinweg  & !auf_rückweg ~ "Hinweg",
      auf_rückweg & !auf_hinweg  ~ "Rückweg")
  )%>% 
  dplyr::select(-auf_hinweg,-auf_rückweg,-start_hin,-start_rück,-ankunft_hin,-ankunft_rück)

# Sauen welche Kuh wann den weg Vollständig absolviert hat: 
vollstaendige_wege <- S_E_Zeit_pro_Kuh |>
  group_by(quelle_layer, Rasse_ID) |>
  summarise(
    hin_komplett  = any(start_hin_Weg),
    rück_komplett = any(start_rück_Weg),
    .groups = "drop"
  )

# Nur Halb absolvierte Wege raus filtern: 
daten_weg <- daten_weg |>
  left_join(vollstaendige_wege, by = c("quelle_layer", "Rasse_ID")) |>
  filter(
    (richtung == "Hinweg"  & hin_komplett) |
      (richtung == "Rückweg" & rück_komplett)
  ) |>
  dplyr::select(-hin_komplett, -rück_komplett)



##############################################



# Herausfinden Wann welcher weg genommen wurde:

# # Pro Layer und Richtung die Route mit der geringeren mittlerer Distanz zum Routentrajektoer herausfinden und zuweisen:
route_pro_layer <- daten_weg |>
  filter( # Daten punkte auswählen welche auf dem Weg liegen: (zwoschen start unde ende)
    st_coordinates(st_transform(geom, 4326))[, 1] > S_E[1],
    st_coordinates(st_transform(geom, 4326))[, 1] < S_E[2]
  ) |>
  mutate( # Distanz jedes Punktes zu beiden Routen berechnen:
    dist_wanderweg = as.numeric(st_distance(geom, st_union(Wander_Weg))),
    dist_strasse   = as.numeric(st_distance(geom, st_union(Strasse)))
  ) |>
  st_drop_geometry() |>
  group_by(quelle_layer, richtung) |>
  summarise(
    mean_dist_ww  = mean(dist_wanderweg, na.rm = TRUE),
    mean_dist_str = mean(dist_strasse,   na.rm = TRUE),
    route         = if_else(mean_dist_ww < mean_dist_str, "Wanderweg", "Strasse"),
    .groups = "drop"
  )

# Route zurück in Hauptdatensatz joinen:
daten_weg <- daten_weg |>
  left_join(
    route_pro_layer |> dplyr::select(quelle_layer, richtung, route),
    by = c("quelle_layer", "richtung")
  )


###########################################



# Zitpunkte wann Linien überquert wurden Ermittlen: 

#Hilfsfunktion zum berechnen des wahrscheindlichsten ¨berschreitungs zeitpunkts erstellen. 
kreuzungszeiten_berechnen <- function(df, linien_liste) {
  crs_df <- st_crs(df)  
  
  imap(linien_liste, \(linie, name) {#üergibt der funktion den linien Nahmen mit, damit der später noch bekant ist / zur vwerfügung steht. 
    linie_sfc <- st_sfc(linie, crs = crs_df)
    
    df |>
      filter(lengths(st_intersects(segment, linie_sfc)) > 0) |>
      mutate(
        schnittpunkt = st_intersection(segment, linie_sfc),
        dist_zum_sp  = as.numeric(st_distance(geom, schnittpunkt, by_element = TRUE)),
        anteil       = dist_zum_sp / steplength, # Vergleicht distanz zum Schnitpunkt mit der gasamt dixtanz des segments. 
        Time_kreuzung     = round(Time + anteil * timelag), # fügt den entsprechenden zeitanteil zur Segmentstartzeit dazu. 
        linie_typ    = name
      ) |>
      st_drop_geometry() |>
      dplyr::select(quelle_layer, Rasse_ID, richtung, route, linie_typ, Time_kreuzung, Rasse, Tageszeit) |>
      group_by(quelle_layer, Rasse_ID, richtung) |>
      slice_min(Time_kreuzung, n = 1, with_ties = FALSE) #nimt inm falle mehrerer überschreitungen die erste. 
  }) |>
    bind_rows()
}

# Linien Namen hinzufügen: 
linien_ww <- setNames(
  c(st_geometry(Start_Ende[1, ]), st_geometry(ww_messline), st_geometry(Start_Ende[2, ])),
  seq(0,6,1)
)

# Wanderwegdaten auswählen und Kreuzungszeiten berechnen:
kreuzungs_zeiten_ww<- daten_weg |>
  filter(route == "Wanderweg") |>
  kreuzungszeiten_berechnen(linien_ww)

# Linien Namen hinzufügen: 
linien_str <- setNames(
  c(st_geometry(Start_Ende[1, ]), st_geometry(str_messline), st_geometry(Start_Ende[2, ])),
  seq(0,6,1)
)

# Strassen-Daten auswählen und Kreuzungszeiten berechnen:
kreuzungs_zeiten_str <- daten_weg |>
  filter(route == "Strasse") |>
  kreuzungszeiten_berechnen(linien_str)

# Zusammenführen:
kreuzungs_zeiten <- bind_rows(kreuzungs_zeiten_ww, kreuzungs_zeiten_str)

###############################################################################
# Rank Plot mit mittelwerten über den ganzen versuch: 

# Rang pro Messlinie 
kreuzung_sum <- kreuzungs_zeiten %>%
  group_by(route, quelle_layer, richtung, linie_typ) %>%
  mutate(
    rang      = rank(Time_kreuzung),
    rückstand = Time_kreuzung - min(Time_kreuzung),
    linie_typ = as.numeric(linie_typ)
  ) %>%
  group_by(route, Rasse_ID, linie_typ, richtung ) %>%
  summarise(
    mean_rang      = mean(rang),
    mean_rückstand = as.numeric(mean(rückstand)),
    .groups = "drop"
  )

# Library für höhenlagen lagen laden: 
library(elevatr)

# Funktion für Höhen Profile erstellen: 

hoehenprofil <- function(rute){
  # Punkte extrahieren und auf Bereich zwischen Start und Ende filtern
  weg_pts <- rute |>
    st_cast("POINT") |>
    st_transform(4326)
  
  weg_pts <- weg_pts[
    st_coordinates(weg_pts)[, 1] > S_E[1] &
      st_coordinates(weg_pts)[, 1] < S_E[2],]
  
  # Höhe pro Punkt abrufen
  weg_pts <- get_elev_point(weg_pts, src = "aws", z = 12)
  
  # Kumulative Distanz berechnen, auf 0–6 skalieren
  hoehen_profil <- weg_pts |>
    mutate(
      dist           = as.numeric(st_distance(geometry, lag(geometry), by_element = TRUE)),
      dist           = replace_na(dist, 0),
      kum_dist       = cumsum(dist)
    ) |>
    st_drop_geometry() |>
    transmute(
      hoehe_m        = elevation,
      linie_typ      = scales::rescale(kum_dist, to = c(0, 6)), #skalieren nach den Messlinen
    )
}

hoehen_profil_ww <- hoehenprofil(Wander_Weg)
hoehen_profil_str <- hoehenprofil(Strasse)

# interleave Funktion definieren (Hilfsfunktion, Geleiche hellikeit nicht nahe bei einander)
interleave_idx <- function(n) {
  c(seq(1, n, by = 2), seq(2, n, by = 2))
}

# Farbvektor (Hell Dunkel interleaved, nach mittlerem Rang) (jade Rasse bekommt Ihre Farbe)
rasse_farben <-kreuzungs_zeiten %>%
  group_by(route, quelle_layer, richtung, linie_typ) %>%
  mutate( rang = rank(Time_kreuzung)) %>% 
  ungroup() %>% 
  group_by(Rasse_ID, Rasse) %>%
  summarise(mean_rang = mean(rang, na.rm = TRUE), .groups = "drop") %>%
  group_by(Rasse) %>%
  arrange(mean_rang) %>%
  mutate(
    idx   = interleave_idx(n()),
    farbe = case_when(
      Rasse == "HO" ~ colorRampPalette(c("#7a1a1a", "#e6a8a8"))(n())[idx],
      Rasse == "OB" ~ colorRampPalette(c("#1a1a7a", "#a8a8e6"))(n())[idx],
      Rasse == "HW" ~ colorRampPalette(c("#1a6b1a", "#a8dba8"))(n())[idx]
    )
  ) %>%
  ungroup()

farb_vektor <- setNames(rasse_farben$farbe, rasse_farben$Rasse_ID)



#########################################################



# Zeitpunkt der ersten Kreuzung pro Layer + Richtung + Linie
erste_kreuzung <- kreuzungs_zeiten |>
  group_by(quelle_layer, richtung, linie_typ) |>
  slice_min(Time_kreuzung, n = 1, with_ties = FALSE) |>
  ungroup()

# 2. Nächster GPS-Punkt jeder Kuh zu diesem Zeitpunkt
positionen <- erste_kreuzung |>
  rowwise() |>
  mutate(pos = list({
    ql <- quelle_layer; ri <- richtung; tc <- Time_kreuzung
    daten_weg |>
      filter(quelle_layer == ql, richtung == ri) |>
      group_by(Rasse_ID) |>
      slice_min(abs(as.numeric(difftime(Time, tc, units = "secs"))),
                n = 1, with_ties = FALSE) |>
      ungroup() |>
      mutate(x = st_coordinates(geom)[, 1],
             y = st_coordinates(geom)[, 2]) |>
      st_drop_geometry() |>
      dplyr::select (Rasse_ID, x, y)
  })) |>
  ungroup()

# Hifs-Funktion welche Pro Kuh die position vor der Kreuzungszeit (Referenzzeit) berechent: 
positionen <- function(x){
  daten_weg |>
    filter(quelle_layer == erste_kreuzung$quelle_layer[x],
           richtung     == erste_kreuzung$richtung[x],
           Time         <= erste_kreuzung$Time_kreuzung[x]) |>  
    group_by(Rasse_ID) |>
    slice_max(Time, n = 1, with_ties = FALSE) |>                 
    ungroup() |>
    mutate(ref_linie = erste_kreuzung$linie_typ[x],
           ref_time  = erste_kreuzung$Time_kreuzung[x])
}

# Anzahl Zeilen zählen 
n_row = nrow(erste_kreuzung)

# funktion pro Zeile ausführen und alles zusammen fügen: 
liste_positionen <- map(1:n_row, positionen) |> 
  bind_rows()

str(liste_positionen)

# Hilfsfunktion um aus aus dem Segment und dem anteil die wahrschindlichste position der Kuh zu berechen: 
punkt_auf_segment <- function(seg, ant) {
  coords <- st_coordinates(seg)
  st_point(c(
    coords[1, 1] + ant * (coords[2, 1] - coords[1, 1]),
    coords[1, 2] + ant * (coords[2, 2] - coords[1, 2])
  ))
}

# Positionen der Kühe zur referenzzeit berechen: 
liste_positionen <- liste_positionen |>
  mutate(
    # Anteil des Segments der bis ref_time zurückgelegt wurde
    anteil = as.numeric(difftime(ref_time, Time, units = "secs")) / timelag,
    # Interpolierter Punkt entlang des Segments
    pos_ref_time = st_sfc(
      mapply(punkt_auf_segment, segment, anteil, SIMPLIFY = FALSE),
      crs = st_crs(liste_positionen)
    )
  )

# Seed Setzen (für wiederholbarkeit)
set.seed(73)

# Kluster Berechenen:
cluster_results <- liste_positionen |>
  st_drop_geometry() |>
  group_by(ref_time) |>
  group_modify(~ { # pro gruppe folgendes ausführen:
    coords   <- st_coordinates(st_sfc(.x$pos_ref_time)) 
    
    km_cascade  <- cascadeKM(coords, inf.gr = 2, # Bestimmen wie vile Gruppen zwischen 2 und 6
                             sup.gr    = min(nrow(coords) - 1, 6),
                             criterion = "calinski")
    
    k_idx    <- which.max(km_cascade$results[nrow(km_cascade$results), ]) # Sinvollste aufteilung wählen
    
    mutate(.x, cluster = km_cascade$partition[, k_idx]) #Die Gruppen als zahl in die liste hin zu fügen. 
  }) |>
  ungroup()


# Matrix welche kühe oft zusammen sind erstellen 
alle_kuehe <- sort(unique(cluster_results$Rasse_ID)) # Anzahl unterschidliche Kühe feststellen
clu_matrix  <- matrix(0L, nrow = length(alle_kuehe), ncol = length(alle_kuehe), # Lehre (gefüllt mit 0en) Matrix erstellen 
                      dimnames = list(alle_kuehe, alle_kuehe))


cluster_results |>
  group_by(ref_time) |>
  group_walk(~ { #wendet folgendes auf alle oben definierten gruppen an 
    idx <- .x$Rasse_ID  # gibt die RassenID zurück
    mat <- (outer(.x$cluster, .x$cluster, "==") + 0L) # Schaut welche matchen und wandelt True/Fals in 1 und 0 um
    dimnames(mat) <- list(idx, idx) # Beschriften der Zeilen und spalten mit den RassenIDs
    clu_matrix[idx, idx] <<- clu_matrix[idx, idx] + mat #Aktualisiert die matrix (in dem es die resultate der neusten Gruppe dazu adiert)
  })

# Kuh mit sich selbst entfernen
diag(clu_matrix) <- 0   




#####################################################################




# Gruppen in der Herde mit Euklidischerdistanz Analysiern
# Matrix mit st_is_within_distance
dist_matrix  <- matrix(0L, nrow = length(alle_kuehe), ncol = length(alle_kuehe), # Lehre (gefüllt mit 0en) Matrix erstellen 
                       dimnames = list(alle_kuehe, alle_kuehe))


liste_positionen |>
  st_drop_geometry() |>
  group_by(ref_time) |>
  group_walk(~ {
    idx    <- .x$Rasse_ID
    pos_sf <- st_sf(geometry = st_sfc(.x$pos_ref_time, crs = 2056))
    
    # Welche Kühe sind innerhalb 5m?
    nah <- st_is_within_distance(pos_sf, dist = 5, sparse = FALSE) + 0L
    dimnames(nah)  <- list(idx, idx)
    
    dist_matrix [idx, idx] <<- dist_matrix [idx, idx] + nah
  })

diag(dist_matrix ) <- 0




##########################################################################




