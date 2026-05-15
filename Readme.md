---
bibliography: references.bib
lang: de
---

# Proposal for Semester Project

```{=html}
<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github. 
Please do not Rename this file (Readme.md has a special meaning on GitHub).

quarto render Readme.md --to pdf
-->
```

**Patterns & Trends in Environmental Data / Computational Movement Analysis / Geo 880**


| Semester: | FS26 |
|:----------------------|:-----------------------------------------------|
| **Daten:** | GPS-Daten von Milchkühen sowie Temperaturdaten |
| **Titel:** | Einfluss der Temperatur auf das Bewegungsverhalten dreier Milchkuhrassen auf dem Weg von der Weide zum Melkstand |
| **Student 1:** | Jonas Amacher |
| **Student 2:** | Stefanie Guyer |

## Hintergrund

Milchkühe gelten als kältetolerant, reagieren jedoch vergleichsweise empfindlich auf Wärme. Ihr thermoneutraler Bereich liegt zwischen 0°C und 16°C. Ab 22°C wird bei Milchkühen von leichtem Hitzestress ausgegangen. Diese Schwellentemperatur ab der Hitzestress auftreten kann, ist allerdings nicht immer gleich und hängt von verschiedenen Faktoren ab. Bekannt ist, das Tiere mit einer hohen Milchleistung viel schneller Hitzestress verspüren [@teamtierhaltungmilchwirtschaft2021].

Hitzestress äussert sich bei Milchkühen durch Abweichungen verschiedener physiologischer Parameter vom Normalbereich. So konnten @pontiggia2023 in Untersuchungen mit weidenden Milchkühen zeigen, dass bereits moderate Wärmebelastung mit Veränderungen in Blut- und Milchparametern einhergeht. Bei Tieren mit erhöhter Körpertemperatur wurden unter anderem eine erhöhte Herzfrequenz, höhere Glukosewerte im Blut, höhere Cortisolwerte in der Milch, tiefere Konzentrationen der Schilddrüsenhormone T3 und T4 sowie veränderte Elektrolytkonzentrationen in der Milch festgestellt.

Während der Einfluss der Temperatur auf physiologische Parameter von Milchkühen relativ gut untersucht ist, gibt es bislang nur wenige Studien zum Einfluss der Temperatur auf das Bewegungsverhalten. Da Verhaltensänderungen häufig bereits vor messbaren physiologischen Veränderungen auftreten, könnte das Bewegungsverhalten potenziell als sensitiver Indikator zur frühzeitigen Identifikation von Hitzestress bei Milchkühen herangezogen werden [@abeni2017]. Darüber hinaus kann die Analyse temperaturabhängiger Bewegungsmuster wichtige Hinweise für das räumliche Management von Milchkuhherden liefern, insbesondere im Hinblick auf Weideführung, Stallmanagement, Schattenangebot und Zugang zu Tränken [@paixão2026].

Bisherige Untersuchungen zum Einfluss der Temperatur auf das Bewegungsverhalten der Tiere deuten darauf hin, dass Wärmebelastung bei Kühen mit Veränderungen des Aktivitäts- und Wiederkauverhaltens verbunden ist. So zeigten @abeni2017, @holinger2024, @paixão2026, dass erhöhte Temperatur-Feuchte-Indizes (THI) mit einer reduzierten Wiederkau- und Ruhezeit sowie einer erhöhten Bewegungsaktivität der Tiere einhergehen können. Zudem berichteten @holinger2024, dass sich bei moderater Wärmebelastung auch die räumliche Verteilung innerhalb der Herde veränderte, indem die individuellen Abstände zwischen den Kühen abnahmen.

Ein aktuelles Projekt von Agroscope und AgroVet Strickhof mit dem Titel PeaMaps untersucht das Bewegungsverhalten dreier Kuhrassen mit unterschiedlichen Leistungsniveaus (Holstein, Original Braunvieh, Hinterwälder) auf der Alp Weissenstein (GR). Ziel des Projekts ist es insbesondere zu untersuchen, ob die verschiedenen Kuhrassen den alpinen Weideraum unterschiedlich nutzen und dadurch in unterschiedlichem Ausmass zur Offenhaltung der Alpflächen beziehungsweise zur Reduktion der Verbuschung beitragen können [@pauler2024]. Erste Ergebnisse aus dem Projekt zeigen, dass sich das Bewegungs- und Raumnutzungsverhalten zwischen den untersuchten Kuhrassen tatsächlich unterscheidet. Gemäss @pauler2025 scheinen Holstein-Kühe eher flachere Bereiche zu bevorzugen und ein selektiveres Fressverhalten zu zeigen als die beiden anderen Kuhassen. Original Braunvieh und insbesondere Hinterwälder scheinen besser an alpines Gelände angepasst zu sein und nutzen häufiger höher gelegene oder steilere Bereiche. Vor allem Hinterwälder gelten als besonders trittsicher im steilen Gelände und scheinen zudem besser mit nährstoffärmerem Futter zurechtzukommen [@srf.ch2025].

Im Rahmen des PeaMaps-Projekts wurde der Einfluss der Temperatur auf das Bewegungsverhalten der Kühe sowie mögliche Unterschiede zwischen den Rassen bislang noch nicht untersucht. Untersuchungen zur temperaturabhängigen Veränderung des Bewegungsverhaltens verschiedener Kuhrassen könnten zusätzliche Hinweise geben, welche Rassen für alpine Weidesysteme besonders geeignet sind. Zeigt eine Kuhrasse beispielsweise eine besonders hohe Temperatursensibilität, indem sich ihr Bewegungsverhalten bei steigenden Temperaturen stark verändert, könnte dies die bisherige Einschätzung ihrer Eignung für die Alpung beeinflussen.

In Anbetracht des Klimawandels und dem damit verbundenen Temperaturanstieg gewinnt diese Fragestellung zusätzlich an Relevanz.

## Fragestellung

Beeinflusst die Temperatur das Bewegungsverhalten von Milchkühen auf dem Weg zum Melkstand und unterscheidet sich dieser Einflussfaktor zwischen den verschiedenen Milchkuhrassen?

#### Unterfragen

-   U1: Verändert sich die Wegdauer von der Weide zum Melkstand bei unterschiedlichen Temperaturen?

-   U2: Zeigt sich bei unterschiedlichen Temperaturen ein verändertes Stop-and-Move-Muster und gibt es hierbei Unterschiede zwischen den Rassen?

-   U3: Verändert sich bei unterschiedlichen Temperaturen die Position einzelner Tiere oder Rassen innerhalb der Gruppe auf dem Weg zum Melkstand?

-   U4: Zeigt sich bei unterschiedlichen Temperaturen ein verändertes Gruppierungsverhalten, z. B. durch geringere Abstände zwischen den Tieren?

## Resultate

#### Erwartungen:

Verschiedene wissenschaftliche Studien zur Reaktion des Bewegungsverhaltens auf die Wärmebelastung bei Kühen zeigen ein weitgehend konsistentes Bild. Unter Hitzestress passen Kühe ihr Bewegung- und Aktivitätsverhalten als thermoregulatorische Reaktion an. Gemäss @cook2007, @nordlund2019, @becker2020, @corazzin2021, @holinger2024 und @schütz2010 zeigen sich insbesondere Veränderungen im Liege-, Steh-, Fress-, Wiederkau-, Trink-, und Raumverhalten.

Bei zunehmender Wärmebelastung ist eine Abnahme folgender Verhaltensweisen zu erwarten:

-   Liegezeit

-   Futteraufnahme

-   Wiederkauzeit

-   Distanz zu anderen Individuen

Demgegenüber ist eine Zunahme folgender Verhaltensweisen zu erwarten:

-   Stehzeit

-   Trinkverhalten

-   räumliche Konzentration der Tiere an thermisch günstigeren Standorten

Durch unsere Datenanalyse erwarten wir Ergebnisse, welche den Beobachtungen in der Literatur nicht widersprechen sollen.

Für einige Unterfragen haben wir folglich konkrete Erwartungen:

-   U2: Aktiveres Stop-and Move-Muster bei steigenden Temperaturen
-   U4: Reduktion der Distanz zwischen den Tieren bei steigenen Temperaturen

#### Visualisierung

Die Unterfragen möchten wir wie folgt Visualisieren:

| Unterfrage | Mögliche Visualisierung (Parameter) | Ziel der Darstellung |
|----------------|---------------------|-----------------------------------|
| U1 | scatter plot (temp\~Wegdauer) | Identifikation von Abhängigkeiten, Linerarität? |
| U2 | boxplots (Anzahl Stops, Dauer der Stops, Zeit zwischen den Stops)  | Identifikation der in der Literatur beschriebenen Zunahme des Aktivitätsmuster, z.B. kürzere Stop-Move-Stop-Intervalle |
| U3 | tabellarisch (Nummerierung der Tiere nach Position), boxplots (Veränderung der Position, z.B. von vorne nach hintern) | Prüfung, ob Tiere bei steigenden Temperaturen häufiger ihre relative Position innerhalb der Herde verändern, beispielsweise von vorne nach hinten, und ob sich dabei Unterschiede zwischen den Rassen zeigen |
| U4 | Boxplots der durchschnittlichen Grösse der Convex Hulls od. Distanzen zwischen den Tieren | Identifikation der in der Literatur beschriebenen Abnahme der individuellen Distanzen bei steigenden Temperaturen |


## Data

Für die Analyse verwenden wir Daten aus dem "PeaMaps"-Projekt, welche wir von der Agroscope (Manuel Schneider) erhalten haben. Zusätzlich verwenden wir Temperaturdaten der nächsten Wetterstation zur Alp Weissenstein. Die nächste Wetterstation zur Alp Weissenstein ist die Wetterstation in Samedan. 

<!-- (100-150 words) -->

<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->

## Analytical concepts

<!-- (100-200 words) -->

<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->

## R concepts

<!-- (50-100 words) -->

<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->

## Risk analysis

<!-- (100-150 words) -->

<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->

## Questions?

1) Wir haben ca. 50 gpkg-Files, welche jeweils noch ca. 22 Layers enthalten. Wie sollen wir am besten damit umgehen? Einlesen, Zusammenfügen? 



<!-- (100-150 words) -->

<!-- Which questions would you like to discuss at the coaching session? -->
