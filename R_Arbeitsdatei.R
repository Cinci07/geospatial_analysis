#### Notes ####

# Fix plot function for countries --> Longitude_country instead of Longitude_airport --> erledigt

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
library("dplyr")
library("tidyr")

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

#### Dynamic working directory ####

## this allows multiple persons to use the same R-File without adjusting the working directory by themselves all the time
source("scripts/csf.R")
path_to_wd <- csf() # if this - for some reason - does not work, replace with a hardcoded path, like: "~/projects/rddj-template/analysis/"
if ( is.null(path_to_wd) | !dir.exists(path_to_wd)) {
  print("WARNING: No working directory specified for current user")
} else {
  setwd(path_to_wd)
}

# End


#### Load Data + Transformations ####

## Load airport data
airports <- read.csv("data/Airports.csv", header = FALSE)
airports <- subset(airports[ airports$V5 != "\\N", c("V4","V5","V7","V8")] )
colnames(airports) <- c("Country","IATA","Latitude_airport","Longitude_airport")

  # Manually add DOH (missing in raw airports df)
  DOH <- data.frame(Country = 'Qatar', IATA = 'DOH', Latitude_airport =25.261125, Longitude_airport =51.565056)
  airports <- rbind(airports, DOH)
  

## Load country center data
country_centers <- read.csv("data/Country_Centers.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
colnames(country_centers) <- c("Latitude_country","Longitude_country","Country_name","Continent")

  # Manual transformations of Continent for Tanzania and Egypt
  country_centers[country_centers$Country_name == 'Tanzania', 4] <- 'Africa'
  country_centers[country_centers$Country_name == 'Egypt', 4] <- 'Middle East'
  
  # Cleaning country name for Macedonia
  country_centers[country_centers$Country_name == 'Macedonia [FYROM]', 3] <- 'Macedonia'

## Load flight data
flights_raw_summer <- read.csv("data/20180728_flights_zrh.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)     # summer
flights_raw_winter <- read.csv("data/20190217_flights_zrh.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)     # winter
flights_raw <- rbind(flights_raw_winter, flights_raw_summer)      # combine summer + winter
flights_raw <- subset(flights_raw[flights_raw$Bewegungsart.LSV == "S",c(1,3:5,7,8,12)])                   # Cleaning + Only departures
colnames(flights_raw) <- c("Season", "IATA_Airline_Code","Airline_name","Flugnummer","IATA_Destination_Airport","Destination_City","Passagiere")

## Tidying: Create column for passengers in summer and winter for each airport
flights_raw[flights_raw$Season == '17.02.2019', 1] <- 'winter'
flights_raw[flights_raw$Season == '29.07.2018', 1] <- 'summer'
flights_raw <- aggregate(Passagiere ~ Season + IATA_Destination_Airport + Destination_City, data=flights_raw, FUN=sum)
flights_raw <- spread(flights_raw, Season, Passagiere)

## Calculate difference between summer and winter
flights_raw[is.na(flights_raw)] <- 0
flights_raw$difference <- flights_raw$summer - flights_raw$winter

## Transformation flights
flights_raw[flights_raw$IATA_Destination_Airport %in% c("LGW","LCY","LTN"), 1]  <- "LHR" # London Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "SAW", 1]                   <- "IST" # Instanbul Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "EWR", 1]                   <- "JFK" # New York Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "SXF", 1]                   <- "TXL" # Berlin Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "SXF", 1]                   <- "TXL" # Berlin Flughäfen zusammenfassen
flights_raw[flights_raw$IATA_Destination_Airport == "DME", 1]                   <- "SVO" # Moskau Flughäfen zusammenfassen

## Merging
flights_raw <- merge(flights_raw, airports, by.x="IATA_Destination_Airport", by.y = "IATA", sort = TRUE)  # Add airport data (long/lat)
flights_raw <- merge(flights_raw, country_centers, by.x="Country", by.y="Country_name", sort = TRUE, all.x = TRUE)      # Add country data (long/lat)


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
flights_germany <- subset(flights_raw[flights_raw$Country == "Germany",])

flights_agg_countries <- aggregate( cbind(summer, winter, difference) ~ Country + Latitude_country + Longitude_country + IATA_Start_Airport 
                                    + Longitude_ZRH + Latitude_ZRH, data=flights_raw, FUN=sum)

# Destinations in Europe with difference > 400
flights_eu_difference <- subset(flights_eu[flights_eu$difference > 400,])

#### Build Polygons GGplot + Load Map ####

# Load shapefile
world <- readOGR(dsn = "data",layer = "CNTR_RG_10M_2016_4326")

# Define functions for summer, winter and difference (summer - winter) - different functions to change colors
plot_winter <- function(region, longitude = Longitude_airport, latitude = Latitude_airport,label = IATA_Destination_Airport, scale = 1000){
  ggplot() +
    geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
    geom_point(data = region, aes(longitude, latitude, group = FALSE ), col="#0059b3") +                                        # Airport dots (foreign)
    geom_text(data = region, aes(longitude, latitude, label= label), hjust=1.25, vjust=0.25) +                # Airport text (foreign)
    geom_curve(data = region, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = longitude, yend = latitude), col="#0059b3", 
               curvature = -0.3, size = region$winter/scale, ncp = 50) + # Connections!
    theme_test() +
    theme( panel.background = element_rect(fill = "aliceblue")) +
    coord_equal()
  
}


plot_summer <- function(region, longitude = Longitude_airport, latitude = Latitude_airport,label = IATA_Destination_Airport, scale = 1000){
  ggplot() +
    geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
    geom_point(data = region, aes(longitude, latitude, group = FALSE ), col="#ff7733") +                                        # Airport dots (foreign)
    geom_text(data = region, aes(longitude, latitude, label= label), hjust=1.25, vjust=0.25) +                # Airport text (foreign)
    geom_curve(data = region, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = longitude, yend = latitude), col="#ff7733", 
               curvature = -0.3, size = region$summer/scale, ncp = 50) + # Connections!
    theme_test() +
    theme( panel.background = element_rect(fill = "aliceblue")) +
    coord_equal()
  
}
  


plot_difference <- function(region, longitude = Longitude_airport, latitude = Latitude_airport,label = IATA_Destination_Airport, scale = 1000){
  ggplot() +
    geom_polygon(data = world, aes(long, lat, group = group), fill="papayawhip", size=0.3, linetype=1, color = "black") +                       # Plotting map
    geom_point(data = region, aes(longitude, latitude, group = FALSE ), col="#009933") +                                        # Airport dots (foreign)
    geom_text(data = region, aes(longitude, latitude, label= label), hjust=1.25, vjust=0.25) +                # Airport text (foreign)
    geom_curve(data = region, aes(x = Longitude_ZRH, y = Latitude_ZRH, xend = longitude, yend = latitude), col="#009933", 
               curvature = -0.3, size = region$difference/scale, ncp = 50) + # Connections!
    theme_test() +
    theme( panel.background = element_rect(fill = "aliceblue")) +
    coord_equal()
  
}



# Create plots for each region
# Europe
eu_winter <- plot_winter(flights_eu)
eu_summer <- plot_summer(flights_eu)
eu_difference <- plot_difference(flights_eu)
eu_big_difference <- plot_difference(flights_eu_difference, scale = 500)

# USA
usa_winter <- plot_winter(flights_usa)
usa_summer <- plot_summer(flights_usa)
usa_difference <- plot_difference(flights_usa)

# Asia
asia_winter <- plot_winter(flights_asia)
asia_summer <- plot_summer(flights_asia)
asia_difference <- plot_difference(flights_asia)

# Middle East
middle_winter <- plot_winter(flights_middle)
middle_summer <- plot_summer(flights_middle)
middle_difference <- plot_difference(flights_middle)

# Non-EU
non_eu_winter <- plot_winter(flights_non_eu)
non_eu_summer <- plot_summer(flights_non_eu)
non_eu_difference <- plot_difference(flights_non_eu)

# Germany
germany_winter <- plot_winter(flights_germany)
germany_summer <- plot_summer(flights_germany)
germany_difference <- plot_difference(flights_germany)

# Aggregated on countries (world)
agg_countries_winter <- plot_winter(flights_agg_countries, longitude = flights_agg_countries$Longitude_country, latitude = flights_agg_countries$Latitude_country, label = flights_agg_countries$Country)
agg_countries_summer <- plot_summer(flights_agg_countries)
agg_countries_difference <- plot_difference(flights_agg_countries)


## End


#### Plot Polygons + Adjustments ####


# Europe
eu_winter + coord_quickmap(xlim=c(-15,30), ylim=c(25,62))
ggsave(paste("output/eu_winter.png",sep=""), width = 20, height = 20, units = "cm")

eu_summer + coord_quickmap(xlim=c(-15,30), ylim=c(25,62))
ggsave(paste("output/eu_summer.png",sep=""), width = 20, height = 20, units = "cm")

eu_difference + coord_quickmap(xlim=c(-15,30), ylim=c(25,62))
ggsave(paste("output/eu_difference.png",sep=""), width = 20, height = 20, units = "cm")

eu_big_difference + coord_quickmap(xlim=c(-15,30), ylim=c(25,62))
ggsave(paste("output/eu_big_difference.png",sep=""), width = 20, height = 20, units = "cm")

# USA
usa_summer + coord_quickmap(xlim=c(-130,-50), ylim=c(25,55))  
ggsave(paste("output/usa_summer.png",sep=""), width = 20, height = 20, units = "cm")

usa_winter + coord_quickmap(xlim=c(-130,-50), ylim=c(25,55))  
ggsave(paste("output/usa_winter.png",sep=""), width = 20, height = 20, units = "cm")

usa_difference + coord_quickmap(xlim=c(-130,-50), ylim=c(25,55))
ggsave(paste("output/usa_difference.png",sep=""), width = 20, height = 20, units = "cm")

# Asia
asia_summer + coord_quickmap(xlim=c(65,140), ylim=c(-10,65)) # Asia
ggsave(paste("output/asia_summer.png",sep=""), width = 20, height = 20, units = "cm")

asia_winter + coord_quickmap(xlim=c(65,140), ylim=c(-10,65)) # Asia
ggsave(paste("output/asia_winter.png",sep=""), width = 20, height = 20, units = "cm")

asia_difference + coord_quickmap(xlim=c(65,140), ylim=c(-10,65)) # Asia
ggsave(paste("output/asia_difference.png",sep=""), width = 20, height = 20, units = "cm")

# Middle East
middle_summer + coord_quickmap(xlim=c(30,62), ylim=c(20,40)) # Golfregion
ggsave(paste("output/middle_summer.png",sep=""), width = 20, height = 20, units = "cm")

middle_winter + coord_quickmap(xlim=c(30,62), ylim=c(20,40)) # Golfregion
ggsave(paste("output/middle_winter.png",sep=""), width = 20, height = 20, units = "cm")

middle_difference + coord_quickmap(xlim=c(30,62), ylim=c(20,40)) # Golfregion
ggsave(paste("output/middle_difference.png",sep=""), width = 20, height = 20, units = "cm")


# Non-EU
non_eu_summer + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # World
ggsave(paste("output/non_eu_summer.png",sep=""), width = 30, height = 20, units = "cm")

non_eu_winter + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # World
ggsave(paste("output/non_eu_winter.png",sep=""), width = 30, height = 20, units = "cm")

non_eu_difference + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # World
ggsave(paste("output/non_eu_difference.png",sep=""), width = 30, height = 20, units = "cm")


# Germany
germany_summer + coord_quickmap(xlim=c(5,17), ylim=c(47,57)) # Germany
ggsave(paste("output/germany_summer.png",sep=""), width = 20, height = 20, units = "cm")

germany_winter + coord_quickmap(xlim=c(5,17), ylim=c(47,57)) # Germany
ggsave(paste("output/germany_winter.png",sep=""), width = 20, height = 20, units = "cm")

germany_difference + coord_quickmap(xlim=c(5,17), ylim=c(47,57)) # Germany
ggsave(paste("output/germany_difference.png",sep=""), width = 20, height = 20, units = "cm")


# Countries World
agg_countries_summer + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # Countries World
ggsave(paste("output/countries_summer.png",sep=""), width = 30, height = 20, units = "cm")

agg_countries_winter + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # Countries World
ggsave(paste("output/countries_winter.png",sep=""), width = 30, height = 20, units = "cm")

agg_countries_difference + coord_quickmap(xlim=c(-135,145), ylim=c(-55,65)) # Countries World
ggsave(paste("output/countries_difference.png",sep=""), width = 30, height = 20, units = "cm")


# Countries Europe
agg_countries_summer + coord_quickmap(xlim=c(-15,30), ylim=c(25,62))  # Countries Europe
ggsave(paste("output/countries_eu_summer.png",sep=""), width = 30, height = 20, units = "cm")

  # Plot with Scalebar --- ?!?!!

library("ggspatial")
library("ggsn")

usa = usa_summer 
      + scale_bar(lon = -120, lat = 30, distance_lon = 50, distance_lat = 100, distance_legend = 200, dist_unit = "km", orientation = FALSE)
      + annotation_north_arrow(location = "bl", which_north = "true")
      #+ scale_bar(lon = -130, lat = 26, distance_lon = 500, distance_lat = 100,  distance_legend = 200, dist_unit = "km", oriental =TRUE, arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)
usa + coord_quickmap(xlim=c(-130,-50), ylim=c(25,55)) # USA 

dev.off()
