---
output:
  word_document: default
  pdf_document: default
  html_document: default
---
#### Geospatial Analysis for smart communities
MSCIDS FS 2019

 
# Visualisierung des Passagieraufkommens am Flughafen Zürich

## 1. Themenwahl
Der Flughafen Zürich (ZRH) hat für die Schweizer Verkehrsinfrastruktur bezüglich der internationalen Erreichbarkeit des Landes eine nationale Bedeutung. Im Jahr 2018 hatte der Flughafen Zürich einen Anteil von rund 54% der an Schweizer Flughäfen beförderten Passagiere (Bundesamt für Statistik, 2019).

Das Hauptziel dieser Arbeit ist die kartografische Darstellung des Passagieraufkommens am Flughafen Zürich. Zusätzlich soll die Arbeit saisonale Schwankungen untersuchen. Dafür soll das Passagieraufkommen an einem Spitzentag im Sommer mit je einem durchschnittlichen Tag im Herbst und Winter verglichen werden und die Unterschiede kartografisch abgebildet werden.
Der Jahresverlauf der beförderten Passagiere wie auch der Flugbewegungen kann starke Schwankungen aufweisen. So wurden am Spitzentag im Jahr 2018 am Flughafen Zürich 114'547 Passagiere befördert, während durchschnittlich nur 85'242 Passagiere befördert wurden (Flughafen Zürich AG, 2018).

Die Arbeit soll folgende Forschungsfragen beantworten:
1. Wie sieht das Passagieraufkommen am Flughafen Zürich an spezifisch gewählten Tagen aus?
2. In welche Destinationen bzw. Länder fliegen am meisten Passagiere?
3. Welche saisonalen Unterschiede sind in den Passagierzahlen zu erkennen?

# 2. Datengrundlagen
Für die Analyse werden voraussichtlich folgende Datengrundlagen verwendet:

| Datensatz                             | Schlüsselmerkmale                                                  | Format | Quellen                                                                               |
|---------------------------------------|--------------------------------------------------------------------|--------|---------------------------------------------------------------------------------------|
| Flugbewegungen von spezifischen Tagen | - Flugdatum - Flugnummer - IATA Code Destination - Passagieranzahl | .xlsx  | Flughafen Zürich Frau Franziska Platten Head Datawarehouse & Statistics               |
| Koordinaten der Flughäfen             | - IATA Code Flughafen - Flughafen Name - Latitude - Longitude      | .xlsx  | OpenFlights https://openflights.org/data.html                                         |
| Landesgrenzen für Hintergrundkarte    | - Country Code - Name                                              | .shp   | Eurostat https://ec.europa.eu/eurostat/web/main/home GADM https://gadm.org/index.html |

