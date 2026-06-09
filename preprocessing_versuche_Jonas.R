
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


# Spalte mit Versuchsgruppe hinzufügen:
alle_daten$Grupe <- substr(alle_daten$quelle_file, 1, 2)


################################################################################
################################################################################
# Daten verstehen: 
View(alle_daten)
unique(alle_daten$quelle_file)

# Weg aufzeichnen von einer Kuh am 25.6. am Abend als erste Visualisierung: 

R1_HO01_06_25_A <-  alle_daten |>
  filter(quelle_file == "R1-HW08.gpkg",
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

################### Zeitlich eingränzen #########################
# Zeitliche Verteilung Plotten:

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


#########################################################
#########################################################
#########################################################
############ RankPlot V1 ################################

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


alle_daten <- map_dfr(gpkg_files, f_files)

######################################
######################################
# Neue Transektlinien ################

# Weg aufzeichnen von einer Kuh am 25.6. am Abend als erste Visualisierung: 

Tag1 <-  alle_daten |>
  filter(TimeSlice == "06-25-A")

Tag1 |> 
  ggplot() + 
  geom_sf()

# mit tmap 

tmap_mode("view")

tm_shape(Tag1) + 
  tm_dots()

# Layer lns Anschauen: 

Tag1_1Kuh <-  alle_daten |>
  filter(quelle_layer == "06-25-A_lns",quelle_file == "R1-HW08.gpkg")


Tag1_1Kuh  |> 
  ggplot() + 
  geom_sf()

# mit tmap 

tmap_mode("view")

tm_shape(Tag1) + 
  tm_dots()

# Farbig aufsteigend visualisieren:
# Weg aufzeichnen von einer Kuh am 25.6. am Abend als erste Visualisierung: 

R1_HO01_06_25_A <-  alle_daten |>
  filter(quelle_file == "R1-HW08.gpkg",
         TimeSlice == "06-25-A")

R1_HO01_06_25_A <- R1_HO01_06_25_A %>%
  mutate(reihenfolge = row_number())

ggplot(R1_HO01_06_25_A) +
  geom_sf(aes(color = reihenfolge)) +
  scale_color_gradient(low = "red", high = "blue") 
# mit tmap 

tmap_mode("view")

tm_shape(R1_HO01_06_25_A) +
  tm_dots(
    col = "reihenfolge",
    palette = c("red", "blue"),
    style = "cont"
  )

################ Filter2 Ohne Stall 6.25 (Klahr definierter Star und einde. #############

Filter_2 <-  alle_daten |>
  filter(quelle_layer == "06-25-A") %>% 
  st_transform(4326) %>%
  filter(st_coordinates(geom)[, 1] < 9.800, 
         st_coordinates(geom)[, 1] > 9.7885,) %>%
  st_transform(2056)

# Schauen wo Erster punkt pro Kuh ist:
erste_punkte <- Filter_2%>%
  arrange(Time) %>%
  group_by(quelle_file) %>%
  slice(1) %>%
  ungroup()

# Zeitliche Struktur anschauen: 
Filter_2$HMS <- as_hms(Filter_2$HMS)

Filter_2|>
  st_drop_geometry() |>
  ggplot(aes(x = HMS, y = quelle_layer)) +
  geom_point() +
  scale_x_time(breaks = breaks_width("30 min"), labels = label_time("%H:%M")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Filter_2 <- Filter_2[Filter_2$Hour<17,]

min(Filter_2$HMS)
max(Filter_2$HMS)


R1_HO01_06_25_A <- Filter_2 %>%
  mutate(reihenfolge = row_number())

ggplot(R1_HO01_06_25_A) +
  geom_sf(aes(color = reihenfolge)) +
  scale_color_gradient(low = "red", high = "blue") 
# mit tmap 

tmap_mode("view")

tm_shape(R1_HO01_06_25_A) +
  tm_dots(
    col = "reihenfolge",
    palette = c("red", "blue"),
    style = "cont"
  )

Filter_2$HMS <- as_hms(Filter_2$HMS)

Filter_2|>
  st_drop_geometry() |>
  ggplot(aes(x = HMS, y = quelle_layer)) +
  geom_point() +
  scale_x_time(breaks = breaks_width("30 min"), labels = label_time("%H:%M")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######## Versuch 2 ##########################################
######################################################

Tag1 <-  alle_daten |>
  filter(TimeSlice == "06-25-A")

Tag1 |> 
  ggplot() + 
  geom_sf()

# mit tmap 

tmap_mode("view")

tm_shape(Tag1) + 
  tm_dots()

# Layer lns Anschauen: 

Tag1_1Kuh <-  alle_daten |>
  filter(quelle_layer == "06-25-A_lns",quelle_file == "R1-HW08.gpkg")


Tag1_1Kuh  |> 
  ggplot() + 
  geom_sf()

# mit tmap 

tmap_mode("view")

tm_shape(Tag1) + 
  tm_dots()

# Farbig aufsteigend visualisieren:
# Weg aufzeichnen von einer Kuh am 25.6. am Abend als erste Visualisierung: 

R1_HO01_06_25_A <-  alle_daten |>
  filter(quelle_file == "R1-HW08.gpkg",
         TimeSlice == "06-25-A")

R1_HO01_06_25_A <- R1_HO01_06_25_A %>%
  mutate(reihenfolge = row_number())

ggplot(R1_HO01_06_25_A) +
  geom_sf(aes(color = reihenfolge)) +
  scale_color_gradient(low = "red", high = "blue") 
# mit tmap 

tmap_mode("view")

tm_shape(R1_HO01_06_25_A) +
  tm_dots(
    col = "reihenfolge",
    palette = c("red", "blue"),
    style = "cont"
  )

################ Filter2 Ohne Stall 6.25 (Klahr definierter Star und einde. #############

Filter_2 <-  alle_daten |>
  filter(quelle_layer == "06-25-A") %>% 
  st_transform(4326) %>%
  filter(st_coordinates(geom)[, 1] < 9.800, 
         st_coordinates(geom)[, 1] > 9.7885,) %>%
  st_transform(2056)

# Schauen wo Erster punkt pro Kuh ist:
erste_punkte <- Filter_2%>%
  arrange(Time) %>%
  group_by(quelle_file) %>%
  slice(1) %>%
  ungroup()

# Zeitliche Struktur anschauen: 
Filter_2$HMS <- as_hms(Filter_2$HMS)

Filter_2|>
  st_drop_geometry() |>
  ggplot(aes(x = HMS, y = quelle_layer)) +
  geom_point() +
  scale_x_time(breaks = breaks_width("30 min"), labels = label_time("%H:%M")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Filter_2 <- Filter_2[Filter_2$Hour<17,]

min(Filter_2$HMS)
max(Filter_2$HMS)


R1_HO01_06_25_A <- Filter_2 %>%
  mutate(reihenfolge = row_number())

ggplot(R1_HO01_06_25_A) +
  geom_sf(aes(color = reihenfolge)) +
  scale_color_gradient(low = "red", high = "blue") 
# mit tmap 

tmap_mode("view")

tm_shape(R1_HO01_06_25_A) +
  tm_dots(
    col = "reihenfolge",
    palette = c("red", "blue"),
    style = "cont"
  )

Filter_2$HMS <- as_hms(Filter_2$HMS)

Filter_2|>
  st_drop_geometry() |>
  ggplot(aes(x = HMS, y = quelle_layer)) +
  geom_point() +
  scale_x_time(breaks = breaks_width("30 min"), labels = label_time("%H:%M")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######## Versuch 2 Messlineen einfügen #


library(sf)
library(dplyr)
library(ggplot2)



gemeinsamer_weg <- Filter_2 %>%
  filter(Rasse_ID == "HO01") %>%
  arrange(Time) %>%
  summarise(geometry = st_cast(st_combine(geom), "LINESTRING"))

plot(gemeinsamer_weg$geometry)


weg_coords <- st_coordinates(gemeinsamer_weg$geometry[[1]])

# Kumulative Distanz entlang des Weges
kum_dist <- c(0, cumsum(sqrt(diff(weg_coords[,1])^2 + diff(weg_coords[,2])^2)))
gesamt_dist <- max(kum_dist)

# 5 Punkte gleichmässig nach Distanz (ohne Start/Ende)
ziel_dist <- seq(gesamt_dist / 6, gesamt_dist * 5/6, length.out = 5)
mess_idx  <- sapply(ziel_dist, function(d) which.min(abs(kum_dist - d)))

# Senkrechte Linie mit grösserem Fenster (fenster = Anzahl Punkte links/rechts)
senkrechte_linie <- function(weg_coords, idx, laenge = 100, fenster = 15) {
  idx1 <- max(1, idx - fenster)
  idx2 <- min(nrow(weg_coords), idx + fenster)
  dx <- weg_coords[idx2, 1] - weg_coords[idx1, 1]
  dy <- weg_coords[idx2, 2] - weg_coords[idx1, 2]
  len <- sqrt(dx^2 + dy^2)
  perp_x <- -dy / len
  perp_y <-  dx / len
  mx <- weg_coords[idx, 1]
  my <- weg_coords[idx, 2]
  st_linestring(rbind(
    c(mx - perp_x * laenge/2, my - perp_y * laenge/2),
    c(mx + perp_x * laenge/2, my + perp_y * laenge/2)
  ))
}

mess_linien <- lapply(seq_along(mess_idx), function(i) {
  senkrechte_linie(weg_coords, mess_idx[i], laenge = 100, fenster = 15)
}) %>%
  st_sfc(crs = st_crs(Filter_2)) %>%
  st_sf(messline = 1:5, geometry = .)

tmap_mode("view")

tm_shape(gemeinsamer_weg) + tm_lines(col = "grey40", lwd = 2) +
  tm_shape(mess_linien)     + tm_lines(col = "red", lwd = 2) +
  tm_shape(Filter_2)        + tm_dots(col = "Rasse_ID", alpha = 0.4, size = 0.05)

# Kreuzungszeit pro Tier und Messlinie 
kreuzungen <- Filter_2 %>%
  group_by(Rasse_ID) %>%
  group_split() %>%
  lapply(function(tier) {
    ziel_crs <- st_crs(Filter_2)
    
    track <- tier %>%
      arrange(Time) %>%
      summarise(geometry = st_cast(st_combine(geom), "LINESTRING")) %>%
      st_sf() %>%
      st_set_crs(ziel_crs)
    
    lapply(1:5, function(ml) {
      schnitt <- suppressWarnings(st_intersection(track, mess_linien[ml, ]))
      if (nrow(schnitt) > 0 && !st_is_empty(schnitt$geometry[[1]])) {
        coords <- st_coordinates(schnitt)[1, 1:2]       # Koordinaten extrahieren
        sp <- st_sfc(st_point(coords), crs = ziel_crs)  # sauber neu erstellen
        naechster <- which.min(st_distance(tier, sp))
        tibble(Rasse_ID = tier$Rasse_ID[1], messline = ml,
               kreuzung_time = tier$Time[naechster])
      } else {
        tibble(Rasse_ID = tier$Rasse_ID[1], messline = ml, kreuzung_time = NA)
      }
    }) %>% bind_rows()
  }) %>%
  bind_rows()

# Rang pro Messlinie 
kreuzungen_rang <- kreuzungen %>%
  filter(!is.na(kreuzung_time)) %>%
  group_by(messline) %>%
  mutate(rang = rank(kreuzung_time)) %>%
  ungroup()

# SCHRITT 5: Bump Chart
ggplot(kreuzungen_rang, aes(x = messline, y = rang,
                            group = Rasse_ID, color = Rasse_ID)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzungen_rang, messline == 1),
            hjust = 1.2, size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzungen_rang, messline == 5),
            hjust = -0.2, size = 3) +
  scale_y_reverse(breaks = 1:16) +
  scale_x_continuous(breaks = 1:5, labels = paste("ML", 1:5)) +
  labs(title = "Rangveränderung entlang des Weges",
       x = "Messlinie", y = "Rang (1 = Erster)") +
  theme_minimal() +
  theme(legend.position = "none")

#Und zur Kontrolle die Messlinien auf der Karte:
tmap_mode("view")
tm_shape(gemeinsamer_weg) + tm_lines(col = "grey40") +
  tm_shape(mess_linien) + tm_lines(col = "red", lwd = 2) +
  tm_shape(Filter_2) + tm_dots(alpha = 0.3)

rasse_lookup <- Filter_2 %>%
  st_drop_geometry() %>%
  distinct(Rasse_ID, Rasse)

start_ziel <- Filter_2 %>%
  st_drop_geometry() %>%
  group_by(Rasse_ID) %>%
  summarise(start_time = min(Time, na.rm = TRUE),
            ziel_time  = max(Time, na.rm = TRUE), .groups = "drop")

start_rang <- start_ziel %>%
  mutate(messline = 0, kreuzung_time = start_time,
         rang = rank(start_time)) %>%
  select(Rasse_ID, messline, kreuzung_time, rang)

ziel_rang <- start_ziel %>%
  mutate(messline = 6, kreuzung_time = ziel_time,
         rang = rank(ziel_time)) %>%
  select(Rasse_ID, messline, kreuzung_time, rang)

# Alles zusammenführen
kreuzungen_rang_komplett <- kreuzungen_rang %>%
  bind_rows(start_rang, ziel_rang) %>%
  left_join(rasse_lookup, by = "Rasse_ID") %>%
  arrange(messline)

# Farbvektor (interleaved, nach mittlerem Rang)
interleave_idx <- function(n) {
  half <- ceiling(n / 2)
  c(rbind(1:half, (half + 1):n))[1:n]
}

rasse_farben <- kreuzungen_rang_komplett %>%
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

# Bump Chart
ggplot(kreuzungen_rang_komplett,
       aes(x = messline, y = rang, group = Rasse_ID, color = Rasse_ID)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzungen_rang_komplett, messline == 0),
            hjust = 1.2, size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzungen_rang_komplett, messline == 6),
            hjust = -0.2, size = 3) +
  scale_color_manual(values = farb_vektor) +
  scale_y_reverse(breaks = 1:16) +
  scale_x_continuous(
    breaks = 0:6,
    labels = c("Start", paste("ML", 1:5), "Ziel")
  ) +
  labs(title = "Rangveränderung entlang des Weges",
       x = NULL, y = "Rang (1 = Erster)") +
  theme_minimal() +
  theme(legend.position = "none")

################# Plot Erste Kuh beimessline 4

# Zielzeit: Kreuzungszeit des ersten Tieres an Messlinie 4
zielzeit <- kreuzungen %>%
  filter(messline == 4, !is.na(kreuzung_time)) %>%
  slice_min(kreuzung_time, n = 1) %>%
  pull(kreuzung_time)

# Nächster Punkt pro Tier zu diesem Zeitpunkt
punkte_zeit <- Filter_2 %>%
  mutate(diff_sec = abs(as.numeric(Time - zielzeit))) %>%
  group_by(Rasse_ID) %>%
  slice_min(diff_sec, n = 1) %>%
  ungroup()

ggplot() +
  geom_sf(data = gemeinsamer_weg, color = "grey90", linewidth = 1) +
  geom_sf(data = mess_linien, color = "red", linewidth = 1.2) +
  geom_sf(data = punkte_zeit, aes(color = Rasse_ID), size = 3) +
  scale_color_manual(values = farb_vektor) +
  labs(title = paste("Positionen um", format(zielzeit, "%H:%M:%S")),
       subtitle = "Vorderste Kuh bei Messlinie 4") +
  theme_minimal()


