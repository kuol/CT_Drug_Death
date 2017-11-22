library(data.table)
library(dplyr)
library(rgdal)
library(leaflet)
library(crosstalk)
library(DT)


# Data source:
#      https://catalog.data.gov/dataset/accidental-drug-related-deaths-january-2012-sept-2015

setwd("~/Documents/Fun/R/Leaflet")
df <- data.frame(fread("Accidental_Drug_Related_Deaths__2012-June_2017.csv", stringsAsFactors = TRUE))
df <- as_data_frame(df)
death_loc <- sort(table(df$DeathLoc), decreasing = TRUE)
top10 <- death_loc[1:10]
# top10_city <- names(top10)
# top10_cities <- unname(sapply(top10_city, function(x){gsub("^(.*), CT.*", "\\1", x)}))
# top10_lat <- unname(sapply(top10_city, function(x){as.numeric(gsub(".*\\((.*),(.*)\\).*", "\\1", x))}))
# top10_long <- unname(sapply(top10_city, function(x){as.numeric(gsub(".*\\((.*),(.*)\\).*", "\\2", x))}))
# top10_loc_df <- data_frame(city = top10_cities, long = top10_long, lat = top10_lat)

extract_city_lon_lat <- function(df) {
  city_long_lat <- names(df)
  city <- unname(sapply(city_long_lat, function(x){gsub("^(.*), CT.*", "\\1", x)}))
  lat <- unname(sapply(city_long_lat, function(x){as.numeric(gsub(".*\\((.*),(.*)\\).*", "\\1", x))}))
  long <- unname(sapply(city_long_lat, function(x){as.numeric(gsub(".*\\((.*),(.*)\\).*", "\\2", x))}))
  data_frame(city = city, death_count = as.vector(unname(df)), long = long, lat = lat)
}

dff <- extract_city_lon_lat(death_loc)



simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(substring(s, 1,1), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

# Death Count by county ===============
df_by_county <- df %>% 
  select(Death.County) %>%
  group_by(Death.County) %>%
  summarise(death_count = n()) %>%
  filter(!Death.County %in% c("", "USA")) 

levels(df_by_county$Death.County) <- sapply(levels(df_by_county$Death.County), simpleCap)



# Read county data as a map object =========
conn <- readOGR("countyct.kml", encoding = "UTF-8")
conn@data <- inner_join(conn@data, df_by_county, c("Name" = "Death.County"))


pal <- colorNumeric("YlGn", c(0,1000))

county_popup <- paste0("<strong>", conn$Name, " </strong>","<strong>death count: </strong>", 
                       conn$death_count)

# Wrap the data_frame of top 100 death cities in SharedData
sd <- SharedData$new(dff[1:100, ])

# Create a filter input
filter_slider("city_death", "Death Count", sd, column=~death_count, step=5, width=250)

# Create Crosstalk widget
bscols(
  list(
    filter_slider("city_death", "Death Count", sd, column=~death_count, step=5, width=250),
    leaflet(conn) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "#BDBDC3", weight = 1, 
                  fillColor = ~pal(death_count), fillOpacity = 0.5,
                  popup = county_popup) %>%
      addMarkers(lng = sd$data()$long, lat = sd$data()$lat, popup = sd$data()$city)
  ),
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)


leaflet(conn) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "#BDBDC3", weight = 1, 
              fillColor = ~pal(death_count), fillOpacity = 0.5,
              popup = county_popup) %>%
  addMarkers(lng = top10_loc_df$long, lat = top10_loc_df$lat, popup = top10_loc_df$city)














