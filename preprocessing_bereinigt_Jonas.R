
library("sf")
library("tidyverse")
library("dplyr")
library("purrr")
library("stringr")
library("tmap")
library("lubridate")

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

# Wanderweg definieren: 
Wander_Weg <-  alle_daten |>
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
Strasse <-  alle_daten |>
  filter(quelle_layer == "06-27-M" & #Erste Kuh die diesen Weg genommen hat dient als grundlage
           Hour < 7&
           Rasse_ID == "HO01"
  ) %>% 
  ungroup() %>% 
  arrange(Time) %>%
  filter(is.na(steplength) | steplength > 5) %>% 
  summarise(geometry = st_cast(st_combine(geom), "LINESTRING"))

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

# Weg + Start und Ende und Messlinein Plotten. 
tmap_mode("view")

tm_shape(Wander_Weg)    + tm_lines(col = "black") +
  tm_shape(Strasse)       + tm_lines(col = "blue")  +
  tm_shape(Start_Ende[1, ]) + tm_lines(col = "green") +
  tm_shape(Start_Ende[2, ]) + tm_lines(col = "red")+
  tm_shape(ww_messline)   + tm_lines(col = "blue")   +
  tm_shape(str_messline)  + tm_lines(col = "grey10")


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
  select(-auf_hinweg,-auf_rückweg,-start_hin,-start_rück,-ankunft_hin,-ankunft_rück)

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
  select(-hin_komplett, -rück_komplett)

# Visuelisieren: 
tmap_mode("view")

tm_shape(daten_weg) +
  tm_dots(col = "richtung", palette = c("Hinweg" = "blue", "Rückweg" = "red"))



#############################################################################
#############################################################################
# Herausfinden Wann welcher weg genommen wurde:

# Pro Layer und Richtung: Route mit geringerer mittlerer Distanz zuweisen:
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
    route_pro_layer |> select(quelle_layer, richtung, route),
    by = c("quelle_layer", "richtung")
  )

# Visualisieren:
tmap_mode("view")
tm_shape(daten_weg) +
  tm_dots(col = "route", palette = c("Wanderweg" = "green", "Strasse" = "blue"))


##############################################################################
##############################################################################
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
         select(quelle_layer, Rasse_ID, richtung, route, linie_typ, Time_kreuzung, Rasse, Tageszeit) |>
         group_by(quelle_layer, Rasse_ID, richtung) #|>
         #slice_min(Time_kreuzung, n = 1, with_ties = FALSE) #nimt inm falle mehrerer überschreitungen die erste. 
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


########################################################################################
########################################################################################
# Rank Plot erstellen für einen Layer: 
kreuzung <- kreuzungs_zeiten %>%
  group_by(route, quelle_layer, richtung, linie_typ) %>%
  mutate(
    rang      = rank(Time_kreuzung),
    rückstand = Time_kreuzung - min(Time_kreuzung)) %>% 
  ungroup() %>% 
  filter(quelle_layer == "08-05-A", richtung == "Hinweg")

# interleave Funktion definieren (Hilfsfunktion, Geleiche hellikeit nicht nahe bei einander)
interleave_idx <- function(n) {
  c(seq(1, n, by = 2), seq(2, n, by = 2))
}

# Farbvektor (interleaved, nach mittlerem Rang) (jade Rasse bekommt Ihre Farbe)
rasse_farben <- kreuzung %>%
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


# Umwandeln des linienNamens in Numeric
kreuzung <- kreuzung |>
  mutate(
    linie_typ = as.numeric(linie_typ)
  )

# Raknplot erstellen: 
ggplot(kreuzung,
       aes(x = linie_typ, y = rang, group = Rasse_ID, color = Rasse_ID)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzung, linie_typ == 0),
            hjust = 1.2, size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzung, linie_typ == 6),
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

mean


###############################################################################
# Rank Plot mit mittelwerten über den ganzen versuch: 

# Rang pro Messlinie 
kreuzung_sum <- kreuzungs_zeiten %>%
  group_by(route, quelle_layer, richtung, linie_typ, Tageszeit) %>%
  mutate(
    rang      = rank(Time_kreuzung),
    rückstand = Time_kreuzung - min(Time_kreuzung),
    linie_typ = as.numeric(linie_typ)
  ) %>%
  group_by(route, Rasse_ID, linie_typ, Tageszeit, richtung ) %>%
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

#Funktion zum erstellen der Rankplots: 
rank_plot <- function(daten, y_var, hoehen_dat) {
  y_name     <- rlang::as_label(enquo(y_var)) #macht aus Variabelnnamen ein Teext 
  y_label    <- if (y_name == "mean_rang") "Mittlerer Rang (1 = Erster)" else "Mittlerer Rückstand [s]"
  route      <- unique(daten$route)
  y_spal     <- max(dplyr::pull(daten, {{ y_var }}), na.rm = TRUE) # die als y Variable definierte Spalte aus dem Datensatz ziehen. 
  y_max_plot <- y_spal * 1.4   # Platz für Höhenprofil unterhalb der Ränge
  elev_min   <- min(hoehen_dat$hoehe_m, na.rm = TRUE)
  elev_max   <- max(hoehen_dat$hoehe_m, na.rm = TRUE)
  
  #Höhendaten angepast an die Restlichen daten Skalieren 
  hoehen_dat <- hoehen_dat |>
      mutate(hoehe_skaliert = scales::rescale(hoehe_m, to = c(y_max_plot, y_spal )))
  
  # Plot drehen drehen fals Rückweg: 
  richtung   <- unique(daten$richtung)
  x_scale <- if (richtung == "Rückweg") {
    scale_x_reverse(breaks = 0:6, labels = c("Wiese", paste("ML", 1:5), "Melkstand"))
  } else {
    scale_x_continuous(breaks = 0:6, labels = c("Wiese", paste("ML", 1:5), "Melkstand"))
  }
  
  #Eigentlicher Plot ersten: 
  ggplot(daten, aes(x = linie_typ, y = {{ y_var }}, group = Rasse_ID, color = Rasse_ID))+
    geom_ribbon(
        data        = hoehen_dat,
        aes(x = linie_typ, ymin = hoehe_skaliert, ymax = y_max_plot, group = 1),
        fill        = "grey85", color = "grey60",
        inherit.aes = FALSE
    )+
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(aes(label = Rasse_ID),
              data = filter(daten, linie_typ == min(linie_typ)),
              hjust = 1.2, size = 3) +
    geom_text(aes(label = Rasse_ID),
              data = filter(daten, linie_typ == max(linie_typ)),
              hjust = -0.2, size = 3) +
    scale_color_manual(values = farb_vektor) +
    x_scale +
    labs(title = paste("Entwicklung entlang:", route), x = NULL, y = y_label) +
    theme_minimal() +
    theme(legend.position = "none")+
    scale_y_reverse(
      limits   = c(y_max_plot, 0),
      sec.axis = sec_axis(
        transform = ~ scales::rescale(., from = c(y_spal, y_max_plot), to = c(elev_max, elev_min)),
        name      = "Höhe (m ü. M.)",
        breaks    = seq(ceiling(elev_min / 20) * 20, floor(elev_max / 20) * 20, by = 20)
      )
    )
}


# nach Rang:
kreuzung_sum |>
  filter(route == "Wanderweg", Tageszeit == "A", richtung == "Rückweg") |>
  rank_plot( mean_rang, hoehen_profil_ww)

kreuzung_sum |>
  filter(route == "Wanderweg", Tageszeit == "A", richtung == "Hinweg") |>
  rank_plot( mean_rang, hoehen_profil_ww)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "M", richtung == "Hinweg") |>
  rank_plot( mean_rang, hoehen_profil_str)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "M", richtung == "Rückweg") |>
  rank_plot( mean_rang, hoehen_profil_str)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "A", richtung == "Hinweg") |>
  rank_plot( mean_rang, hoehen_profil_str)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "A", richtung == "Rückweg") |>
  rank_plot( mean_rang, hoehen_profil_str)


# Nach Zeit rückstand auf 1. Kuh
kreuzung_sum |>
  filter(route == "Wanderweg", Tageszeit == "A", richtung == "Rückweg") |>
  rank_plot( mean_rückstand, hoehen_profil_ww)

kreuzung_sum |>
  filter(route == "Wanderweg", Tageszeit == "A", richtung == "Hinweg") |>
  rank_plot( mean_rückstand, hoehen_profil_ww)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "M", richtung == "Hinweg") |>
  rank_plot( mean_rückstand, hoehen_profil_str)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "M", richtung == "Rückweg") |>
  rank_plot( mean_rückstand, hoehen_profil_str)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "A", richtung == "Hinweg") |>
  rank_plot( mean_rückstand, hoehen_profil_str)

kreuzung_sum |>
  filter(route == "Strasse", Tageszeit == "A", richtung == "Rückweg") |>
  rank_plot( mean_rückstand, hoehen_profil_str)

# HW meist forne mit dabei 
# HW06 und ev. HW13 Scheint das Leittier zu sein. 
# Gruppe ist generell beim Melkstand näher bei einander als bei der Weide. 

#################################################################################







#################################################################################

# Rankplot mit Höhenprofil (Wanderweg, Mittelwerte):
kreuzung_ww <- kreuzung_sum |>
  filter(route == "Wanderweg") |>
  mutate(linie_typ = as.numeric(linie_typ))

ggplot(kreuzung_ww, aes(x = linie_typ, y = mean_rang, group = Rasse_ID, color = Rasse_ID)) +
  # #geom_ribbon(data = hoehen_profil_ww,
  #             aes(x = linie_typ, ymin = hoehe_skaliert, ymax = 20, group = 1),
  #             fill = "grey85", color = "grey60",
  #             inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzung_ww, linie_typ == 0),
            hjust = 1.2, size = 3) +
  geom_text(aes(label = Rasse_ID),
            data = filter(kreuzung_ww, linie_typ == 6),
            hjust = -0.2, size = 3) +
  scale_color_manual(values = farb_vektor) +
  scale_y_reverse(breaks = 1:20) +
  scale_x_continuous(breaks = 0:6, labels = c("Weide", paste("ML", 1:5), "Melkstand")) +
  labs(title = "Rangveränderung entlang des Wanderwegs", x = NULL, y = "Mittlerer Rang (1 = Erster)") +
  theme_minimal() +
  theme(legend.position = "none")

# Höhenprofil aus Wanderweg-Punkten aufbereiten:
hoehen_profil <- daten_weg |>
  filter(route == "Wanderweg", richtung == "Hinweg") |>
  mutate(
    linie_typ = (st_coordinates(st_transform(geom, 4326))[, 1] - S_E[1]) / diff(S_E) * 6
  ) |>
  st_drop_geometry() |>
  filter(between(linie_typ, 0, 6)) |>
  mutate(linie_typ = round(linie_typ, 1)) |>
  group_by(linie_typ) |>
  summarise(Altitude = mean(Altitude, na.rm = TRUE)) |>
  arrange(linie_typ) |>
  mutate(hoehe_skaliert = scales::rescale(Altitude, to = c(15.5, 11)))


hoehen_profil <- Wander_Weg |>
  st_transform(4326) |>
  st_coordinates() |>
  as_tibble() |>
  mutate(linie_typ = (X - S_E[1]) / diff(S_E) * 6) |>
  filter(between(linie_typ, 0, 6)) |>
  mutate(linie_typ = round(linie_typ, 1)) |>
  group_by(linie_typ) |>
  summarise(Altitude = mean(Z, na.rm = TRUE)) |>
  arrange(linie_typ) |>
  mutate(hoehe_skaliert = scales::rescale(Altitude, to = c(15.5, 11)))

hoehen_profil <- alle_daten |>
  filter(quelle_layer == "06-25-A", Hour < 17, Rasse_ID == "HO01") |>
  filter(is.na(steplength) | steplength > 5) |>
  mutate(
    linie_typ = (st_coordinates(st_transform(geom, 4326))[, 1] - S_E[1]) / diff(S_E) * 6
  ) |>
  st_drop_geometry() |>
  filter(between(linie_typ, 0, 6)) |>
  mutate(linie_typ = round(linie_typ, 1)) |>
  group_by(linie_typ) |>
  summarise(Altitude = mean(Altitude, na.rm = TRUE)) |>
  arrange(linie_typ) |>
  mutate(hoehe_skaliert = scales::rescale(Altitude, to = c(15.5, 11)))

rank_plot <- function(daten, y_var, hoehen_dat) {
  y_name  <- rlang::as_label(enquo(y_var))
  y_label <- if (y_name == "mean_rang") "Mittlerer Rang (1 = Erster)" else "Mittlerer Rückstand [s]"
  route   <- unique(daten$route)
  y_ceil  <- max(dplyr::pull(daten, {{ y_var }}), na.rm = TRUE) * 1.1
  
  ggplot(daten, aes(x = linie_typ, y = {{ y_var }}, group = Rasse_ID, color = Rasse_ID)) +
    geom_ribbon(data = hoehen_dat,
                aes(x = linie_typ, ymin = hoehe_skaliert, ymax = y_ceil, group = 1),
                fill = "grey85", color = "grey60",
                inherit.aes = FALSE) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(aes(label = Rasse_ID),
              data = filter(daten, linie_typ == min(linie_typ)),
              hjust = 1.2, size = 3) +
    geom_text(aes(label = Rasse_ID),
              data = filter(daten, linie_typ == max(linie_typ)),
              hjust = -0.2, size = 3) +
    scale_color_manual(values = farb_vektor) +
    scale_y_reverse() +
    scale_x_continuous(breaks = 0:6, labels = c("Wiese", paste("ML", 1:5), "Melkstand")) +
    labs(title = paste("Entwicklung entlang:", route), x = NULL, y = y_label) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Aufruf:
plot_rankprofil(kreuzung_ww,  mean_rang, hoehen_profil_ww)
plot_rankprofil(kreuzung_str, mean_rang)
plot_rankprofil(kreuzung_ww,  mean_rückstand)


daten_weg$Tageszeit <- substr(daten_weg$quelle_layer,7,7)
