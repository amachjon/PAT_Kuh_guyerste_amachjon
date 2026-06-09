
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

