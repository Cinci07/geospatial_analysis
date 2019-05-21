#### Notes ####

# Flughafen Doha (DOH) fehlt noch! Muss noch ergänzt werden

# Tanzania Continent ergänzen

# Egypt zu Middle East 

# End

#### Packages ####

library("rgdal")
library("ggplot2")
library("ggspatial")
library("ggsn")
library("maps")
library("maptools")
library("ggplot2")
library("grid")

# End


#### Funktionen Scale and Arrow ####

  # Load the 3 functions for designing the north arrow and the scale bar!

  # Source: http://egallic.fr/en/scale-bar-and-north-arrow-on-a-ggplot2-map/

create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}

scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}

## End

#### Load Data + Transformations ####

setwd("C:/Users/lucac/OneDrive/Dokumente/Schule/HSLU/1_Semester/GEO_Geospacial_Analysis/Projekt/Datasets")

## Load airport data
airports <- read.csv("Airports.csv", header = FALSE)
airports <- subset(airports[ airports$V5 != "\\N", c("V4","V5","V7","V8")] )
colnames(airports) <- c("Country","IATA","Latitude_airport","Longitude_airport")

## Load country center data
country_centers <- read.csv("Country_Centers.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
colnames(country_centers) <- c("Latitude_country","Longitude_country","Country_name","Continent")

## Load flight data
flights_raw <- read.csv("20180728_flights_zrh.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)     # summer
#flights_raw <- read.csv("20190217_flights_zrh.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)     # winter
flights_raw <- subset(flights_raw[flights_raw$Bewegungsart.LSV == "S",c(3:5,7,8,12)])                   # Cleaning + Only departures
colnames(flights_raw) <- c("IATA_Airline_Code","Airline_name","Flugnummer","IATA_Destination_Airport","Destination_City","Passagiere")

## Transformation flights
flights_raw[flights_raw$IATA_Destination_Airport %in% c("LGW","LCY","LTN"), 4]  <- "LHR" # London Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "SAW", 4]                   <- "IST" # Instanbul Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "EWR", 4]                   <- "JFK" # New York Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "SXF", 4]                   <- "TXL" # Berlin Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "SXF", 4]                   <- "TXL" # Berlin Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "DME", 4]                   <- "SVO" # Moskau Flughäfen zusammenfassen

## Mergeing
flights_raw <- merge(flights_raw, airports, by.x="IATA_Destination_Airport", by.y = "IATA", sort=TRUE)  # Add airport data (long/lat)
flights_raw <- merge(flights_raw, country_centers, by.x="Country", by.y="Country_name", sort=TRUE)      # Add country data (long/lat)

flights_raw$IATA_Start_Airport <- "ZRH"   # Add ZRH information
flights_raw$Latitude_ZRH <- 47.46470
flights_raw$Longitude_ZRH <- 8.549171

# End


#### Aggregations ####

flights_eu <- subset(flights_raw[flights_raw$Continent == "Europe",])
flights_usa <- subset(flights_raw[flights_raw$Continent == "North America",])
flights_south <- subset(flights_raw[flights_raw$Continent == "South America",])
flights_middle <- subset(flights_raw[flights_raw$Continent == "Middle East",])
flights_asia <- subset(flights_raw[flights_raw$Continent == "Asia",])
flights_africa <- subset(flights_raw[flights_raw$Continent == "Africa",])

flights_non_eu <- subset(flights_raw[flights_raw$Continent != "Europe",])
flights_ger <- subset(flights_raw[flights_raw$Country == "Germany",])

agg_eu <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_eu, FUN=sum)
agg_usa <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_usa, FUN=sum)
agg_south <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_south, FUN=sum)
agg_middle <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_middle, FUN=sum)
agg_asia <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_asia, FUN=sum)
agg_africa <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_africa, FUN=sum)

agg_non_eu <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_non_eu, FUN=sum)
agg_ger <- aggregate(Passagiere ~ IATA_Destination_Airport + Country + Longitude_airport + Latitude_airport + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_ger, FUN=sum)

agg_country_world <- aggregate(Passagiere ~ Country + Longitude_country + Latitude_country + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_raw, FUN=sum)
agg_country_eu <- aggregate(Passagiere ~ Country + Longitude_country + Latitude_country + Longitude_ZRH + Latitude_ZRH + IATA_Start_Airport, data=flights_eu, FUN=sum)


#### Build Polygons GGplot + Load Map ####

world <- readOGR(dsn = "C:/Users/lucac/OneDrive/Dokumente/Schule/HSLU/1_Semester/GEO_Geospacial_Analysis/Projekt/Maps/World/CNTR_RG_10M_2016_4326",layer = "CNTR_RG_10M_2016_4326")

eu <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_eu, aes(Longitude_airport, Latitude_airport, group = FALSE ), col="Red") +                                            # Airport dots (foreign)
  geom_text(data = agg_eu, aes(Longitude_airport, Latitude_airport, label=IATA_Destination_Airport), hjust=1.25, vjust=0.25) +                # Airport text (foreign)
  geom_curve(data = agg_eu, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_airport, yend = Latitude_airport), col="red", curvature = -0.3, size = agg_eu$Passagiere/1000, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

usa <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_usa, aes(Longitude_airport, Latitude_airport, group = FALSE ), col="Red") +                                           # Airport dots (foreign)
  geom_text(data = agg_usa, aes(Longitude_airport, Latitude_airport, label=IATA_Destination_Airport), hjust=1.25, vjust=0.25) +               # Airport text (foreign)
  geom_curve(data = agg_usa, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_airport, yend = Latitude_airport), col="red", curvature = -0.3, size = agg_usa$Passagiere/300, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

asia <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_asia, aes(Longitude_airport, Latitude_airport, group = FALSE ), col="Red") +                                          # Airport dots (foreign)
  geom_text(data = agg_asia, aes(Longitude_airport, Latitude_airport, label=IATA_Destination_Airport), hjust=1.25, vjust=0.25) +              # Airport text (foreign)
  geom_curve(data = agg_asia, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_airport, yend = Latitude_airport), col="red", curvature = -0.3, size = agg_asia$Passagiere/300, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

middle <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_middle, aes(Longitude_airport, Latitude_airport, group = FALSE ), col="Red") +                                        # Airport dots (foreign)
  geom_text(data = agg_middle, aes(Longitude_airport, Latitude_airport, label=IATA_Destination_Airport), hjust=1.25, vjust=0.25) +            # Airport text (foreign)
  geom_curve(data = agg_middle, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_airport, yend = Latitude_airport), col="red", curvature = -0.3, size = agg_middle$Passagiere/300, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

non_eu <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_non_eu, aes(Longitude_airport, Latitude_airport, group = FALSE ), col="Red") +                                        # Airport dots (foreign)
  geom_text(data = agg_non_eu, aes(Longitude_airport, Latitude_airport, label=IATA_Destination_Airport), hjust=1.25, vjust=0.25) +            # Airport text (foreign)
  geom_curve(data = agg_non_eu, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_airport, yend = Latitude_airport), col="red", curvature = -0.3, size = agg_non_eu$Passagiere/300, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

ger <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_ger, aes(Longitude_airport, Latitude_airport, group = FALSE ), col="Red") +                                           # Airport dots (foreign)
  geom_text(data = agg_ger, aes(Longitude_airport, Latitude_airport, label=IATA_Destination_Airport), hjust=1.25, vjust=0.25) +               # Airport text (foreign)
  geom_curve(data = agg_ger, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_airport, yend = Latitude_airport), col="red", curvature = -0.3, size = agg_ger$Passagiere/1000, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

countries <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_country_world, aes(Longitude_country, Latitude_country, group = FALSE ), col="Red") +                                           # Airport dots (foreign)
  geom_text(data = agg_country_world, aes(Longitude_country, Latitude_country, label=Country), hjust=1.25, vjust=0.25) +               # Airport text (foreign)
  geom_curve(data = agg_country_world, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_country, yend = Latitude_country), col="red", curvature = -0.3, size = agg_country_world$Passagiere/1000, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

countries_eu <-  ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
  geom_point(data = agg_country_eu, aes(Longitude_country, Latitude_country, group = FALSE ), col="Red") +                                           # Airport dots (foreign)
  geom_text(data = agg_country_eu, aes(Longitude_country, Latitude_country, label=Country), hjust=1.25, vjust=0.25) +               # Airport text (foreign)
  geom_curve(data = agg_country_eu, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = Longitude_country, yend = Latitude_country), col="red", curvature = -0.3, size = agg_country_eu$Passagiere/1000, ncp = 50) + # Connections!
  theme_test() +
  theme( panel.background = element_rect(fill = "aliceblue")) +
  coord_equal()

## End


#### Plot Polygons + Adjustments ####

season = "summer"

  # Destinations

setwd("C:/Users/lucac/OneDrive/Dokumente/Schule/HSLU/1_Semester/GEO_Geospacial_Analysis/Projekt/Datasets")

eu + coord_quickmap(xlim=c(-15,30), ylim=c(25,62))  # Europe
ggsave(paste("eu_",season,".png",sep=""), width = 20, height = 20, units = "cm")

usa + coord_quickmap(xlim=c(-130,-50), ylim=c(25,55)) # USA 
ggsave(paste("usa_",season,".png",sep=""), width = 20, height = 20, units = "cm")

asia + coord_quickmap(xlim=c(65,140), ylim=c(-10,65)) # Asia
ggsave(paste("asia_",season,".png",sep=""), width = 20, height = 20, units = "cm")

middle + coord_quickmap(xlim=c(30,62), ylim=c(20,40)) # Golfregion
ggsave(paste("middle_",season,".png",sep=""), width = 20, height = 20, units = "cm")

non_eu + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # World
ggsave(paste("world_",season,".png",sep=""), width = 30, height = 20, units = "cm")

ger + coord_quickmap(xlim=c(5,17), ylim=c(47,57)) # Germany
ggsave(paste("germany_",season,".png",sep=""), width = 20, height = 20, units = "cm")

  # Countries

countries + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # Countries World
ggsave(paste("countries_",season,".png",sep=""), width = 30, height = 20, units = "cm")

countries_eu + coord_quickmap(xlim=c(-15,30), ylim=c(25,62))  # Countries Europe
ggsave(paste("countries_",season,".png",sep=""), width = 30, height = 20, units = "cm")

  # Plot with Scalebar --- ?!?!!

library("ggspatial")
library("ggsn")

usa = usa 
      + scale_bar(lon = -130, lat = 26, distance_lon = 500, distance_lat = 100, distance_legend = 200, dist_unit = "km", orientation = FALSE)
      + annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in") )
      #+ scale_bar(lon = -130, lat = 26, distance_lon = 500, distance_lat = 100,  distance_legend = 200, dist_unit = "km", oriental =TRUE, arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)
usa + coord_quickmap(xlim=c(-130,-50), ylim=c(25,55)) # USA 

dev.off()


