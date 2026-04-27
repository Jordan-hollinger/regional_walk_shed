library(mapboxapi)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)

#load data
food_retailers <- st_read("data/raw/Food_Retailers_Points.geojson")
libraries      <- st_read("data/raw/Library_Points.geojson")
parks          <- st_read("data/raw/Parks_Points.geojson")
schools        <- st_read("data/raw/Schools_Points.geojson")
town_halls     <- st_read("data/raw/TownHall_Points.geojson")

#extra files to test function
post_offices   <- st_read("data/raw/post_office.geojson")
healthcare      <- st_read("data/raw/Health_Facilities.geojson")

#point layers
destinations <- list(
  healthcare   = healthcare ,
  post_offices = post_offices
)

#time intervals for isochrones
time_intervals <- c(5, 10, 15)

#profile for isochrones
profile <- "walking"


#function to create isochrones for a given point layer and save as geojson and shapefile
create_iso <- function(point_layer, name, time, profile) {
  #convert to WGS84
  point_layer <- st_transform(point_layer, 4326)

  #create isochrones
  iso <- mb_isochrone(
    point_layer, 
    time = time, 
    profile = profile,
    access_token = Sys.getenv("MAPBOX_TOKEN")
  )
  
  #convert to state plane for saving as shapefile and geojson
  iso <- st_transform(iso, 26986)
    
  #save as geojson and shapefile
  st_write(iso, paste0("data/processed/geojson/", name, "_iso.geojson"), delete_dsn = TRUE)
  st_write(iso, paste0("data/processed/shp/", name, "_iso.shp"), delete_dsn = TRUE)

  #merged/dissolved isochrones
  iso_dissolved <- iso |> 
    group_by(time) |> 
    summarise(geometry = st_union(geometry))

  #save dissolved isochrones as geojson and shapefile
  st_write(iso_dissolved, paste0("data/processed/geojson/", name, "_iso_dissolved.geojson"), delete_dsn = TRUE)
  st_write(iso_dissolved, paste0("data/processed/shp/", name, "_iso_dissolved.shp"), delete_dsn = TRUE)
}

#apply the function to each point layer
imap(destinations, ~ create_iso(.x, .y, time_intervals, profile))







#Check the difference between the dissolved and undissolved isochrones

#load isos
post_offices_iso_wgs <- st_read("data/processed/geojson/post_offices_iso.geojson") |> st_transform(4326)
post_offices_wgs <- st_transform(post_offices, 4326)

leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(data = post_offices_iso_wgs |> filter(time == 10),
              color = "red",  fillOpacity = 0.5, group = "10 Min") |>
    addPolygons(data = post_offices_iso_wgs |> filter(time == 5),
              color = "red",  fillOpacity = 0.5, group = "5 Min") |>
  addCircles(data = post_offices_wgs, group = "post office") |> 
  addLayersControl(
    overlayGroups = c("10 Min","5 Min", "post office"),
    options = layersControlOptions(collapsed = FALSE)
  )
