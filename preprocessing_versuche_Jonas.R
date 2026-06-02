
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

###############################################################################

# Daten verstehen: 
View(alle_daten)
unique(alle_daten$quelle_file)

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

# Missinien auf dem Weg anzeigen Lassen:
R1_HO01_06_25_A |> 
  ggplot() + 
  geom_sf()+
  geom_sf(data=messlinien)

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


#############################################################
#############################################################

# Datenbereinigung

alle_daten <- alle_daten[!endsWith(alle_daten$quelle_layer, "lns"), ]


# Schauen Wann wie viele Kühe unterwegs wahren: 

NKuh <- alle_daten |>
  st_drop_geometry() |>
  group_by(quelle_layer) |>
  summarise(n_kuehe = n_distinct(Rasse_ID))

# Plotten wann welche Kuh unterwegs wahr: 
alle_daten |>
  st_drop_geometry() |>
  ggplot(aes(quelle_layer,Rasse_ID))+
  geom_point()

# Es hat offensichtlich Drei Versuchs Gruppen gegeben. Ausserdem enthält der 14.7 Morgens nur eine Kuh

# 14.7 Morgens entfernen

alle_daten <- alle_daten[alle_daten$quelle_layer != "07-14-M", ]

# Spalte mit Versuchsgruppe hinzufügen:
alle_daten$Grupe <- substr(alle_daten$quelle_file, 1, 2)


# Geografisches Plotten aller daten
daten %>% 
  ggplot()+
  geom_sf()
# Es gibt offensichtliche Fehler in den Daten

# Alle Daten die mit weniger alls 2 Sateliten bestimmt wurden Entfernen. 
alle_daten <- alle_daten[alle_daten$NSat>"3",]

# Schauen ob esn noch NA werte hat. 
sum(is.na(alle_daten$NSat))

# NA daten entfernen: 
alle_daten <- alle_daten[!is.na(alle_daten$NSat),]
# das sieht schon besser aus

# Daten entfernen mit unrelistischen geshwindikeiten: 

daten <- alle_daten %>%
  arrange(quelle_layer,Rasse_ID,Time) %>%
  mutate(
    x = st_coordinates(geom)[, 1],
    y = st_coordinates(geom)[, 2],
    dist_prev = sqrt((x - lag(x))^2 + (y - lag(y))^2),
    dt_sec = as.numeric(difftime(Time, lag(Time), units="secs")),
    speed_ms = dist_prev / dt_sec
  )%>%
  filter(is.na(speed_ms) | speed_ms < 10)



library(scales)
library(hms)

daten$HMS <- as_hms(daten$HMS)

daten |>
  st_drop_geometry() |>
  ggplot(aes(x = HMS, y = quelle_layer, color = Gruppe)) +
  geom_point() +
  facet_wrap(~ endsWith(quelle_layer, "M"),
        labeller = labeller(`TRUE` = "Morgen", `FALSE` = "Abend"),
        scales = "free")+
  scale_x_time(breaks = breaks_width("30 min"), labels = label_time("%H:%M")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Auf annähernd realistische Zeiten kürzen
start_M <- as_hms("04:30:00")
end_M   <- as_hms("08:45:00")
start_A <- as_hms("15:00:00")
end_A   <- as_hms("19:15:00")

daten_clean <- daten |>
  filter(
    (endsWith(quelle_layer, "M") & HMS >= start_M & HMS <= end_M) |
    (endsWith(quelle_layer, "A") & HMS >= start_A & HMS <= end_A)
  )


# Mittleren Rang pro Individuum, Messline und Gruppe berechnen
rang_mean <- dist_time_ranked %>%
  filter(messlinie %in% c(1, 2)) %>%
  group_by(Grupe, Rasse,Rasse_ID, messlinie) %>%
  summarise(mean_rank = mean(rank, na.rm = TRUE), .groups = "drop")
  
# Hilfsfunktion: Farb-Indices verschachteln
interleave_idx <- function(n) {
  half <- ceiling(n / 2)
  idx <- c(rbind(1:half, (half + 1):n))[1:n]
  idx
}

# Farben Definieren:
rasse_farben <- rang_mean %>%
  group_by(Rasse_ID, Rasse) %>%
  summarise(mean_rank_gesamt = mean(mean_rank), .groups = "drop") %>%
  group_by(Rasse) %>%
  arrange(mean_rank_gesamt, .by_group = TRUE) %>%         # nach Rang sortieren
  mutate(
    idx  = interleave_idx(n()),          # verschachtelte Indices
    farbe = case_when(
      Rasse == "HW" ~ colorRampPalette(c("#1a6b1a", "#a8dba8"))(n())[idx],
      Rasse == "OB" ~ colorRampPalette(c("#1a1a7a", "#a8a8e6"))(n())[idx],
      Rasse == "HO" ~ colorRampPalette(c("#7a1a1a", "#e6a8a8"))(n())[idx]
    )) %>%
  ungroup()

farb_vektor <- setNames(rasse_farben$farbe, rasse_farben$Rasse_ID)

# Ein Plot pro Gruppe
gruppen <- unique(rang_mean$Grupe)

plots <- lapply(gruppen, function(g) {
  rang_mean %>%
    filter(Grupe == g) %>%
    ggplot(aes(x = factor(messlinie), y = mean_rank,
               group = Rasse_ID, color = Rasse_ID)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(aes(label = Rasse_ID),
              hjust = ifelse(rang_mean$messlinie[rang_mean$Grupe == g] == 1, 1.2, -0.2),
              size = 3) +
    scale_color_manual(values = farb_vektor) +
    scale_y_reverse() +       # Rang 1 oben
    labs(
      title = paste("Rangveränderung Gruppe:", g),
      x = "Messlinie",
      y = "Mittlerer Rang"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
})

# Alle 3 Plots anzeigen
library(patchwork)
plots[[1]] | plots[[2]] | plots[[3]]
