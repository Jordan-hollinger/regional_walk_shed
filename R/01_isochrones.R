library(mapboxapi)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)

#load data and transform to WGS84
food_retailers <- st_read("data/raw/Food_Retailers_Points.geojson")
libraries      <- st_read("data/raw/Library_Points.geojson")
parks          <- st_read("data/raw/Parks_Points.geojson")
schools        <- st_read("data/raw/Schools_Points.geojson")
town_halls     <- st_read("data/raw/TownHall_Points.geojson")

st_crs(food_retailers)

food_retailers <- st_transform(food_retailers, 4326)
libraries      <- st_transform(libraries, 4326)
parks          <- st_transform(parks, 4326)
schools        <- st_transform(schools, 4326)
town_halls     <- st_transform(town_halls, 4326)

st_crs(food_retailers)


#------------------------
#food retailer isochrones
#------------------------
food_iso <- mb_isochrone(
  food_retailers, 
  time = c(5, 10, 15), 
  profile = "walking",
  access_token = Sys.getenv("MAPBOX_TOKEN")
)

#if you want to visualize the isochrones
food_iso$time <- factor(food_iso$time, levels = c(5, 10, 15))
mapview(food_iso, zcol = "time", legend = TRUE)

#make sure to change the time column to numeric before saving as geojson
food_iso$time <- as.numeric(as.character(food_iso$time))

#convert to state plane for saving as shapefile and geojson
food_iso <- st_transform(food_iso, 26986)
st_crs(food_iso)

st_write(food_iso, "data/processed/geojson/food_iso.geojson", delete_dsn = TRUE)
st_write(food_iso, "data/processed/shp/food_iso.shp", delete_dsn = TRUE)

#merged/dissolved isochrones
food_iso_dissolved <- food_iso |> 
  group_by(time) |> 
  summarise(geometry = st_union(geometry))

#dissolved isochrones
st_write(food_iso_dissolved, "data/processed/geojson/food_iso_dissolved.geojson", delete_dsn = TRUE)
st_write(food_iso_dissolved, "data/processed/shp/food_iso_dissolved.shp", delete_dsn = TRUE)


#---------------------
#library isochrones
#---------------------
library_iso <- mb_isochrone(
  libraries, 
  time = c(5, 10, 15), 
  profile = "walking",
  access_token = Sys.getenv("MAPBOX_TOKEN")
)

mapview(library_iso, zcol = "time", legend = TRUE)

#default isochrones
st_write(library_iso, "data/processed/geojson/library_iso.geojson", delete_dsn = TRUE)
st_write(library_iso, "data/processed/shp/library_iso.shp", delete_dsn = TRUE)

#merged/dissolved isochrones
library_iso_dissolved <- library_iso |> 
  group_by(time) |> 
  summarise(geometry = st_union(geometry))

#dissolved isochrones
st_write(library_iso_dissolved, "data/processed/geojson/library_iso_dissolved.geojson", delete_dsn = TRUE)
st_write(library_iso_dissolved, "data/processed/shp/library_iso_dissolved.shp", delete_dsn = TRUE)


#-----------------
#parks isochrones
#-----------------
parks_iso <- mb_isochrone(
  parks, 
  time = c(5, 10, 15), 
  profile = "walking",
  access_token = Sys.getenv("MAPBOX_TOKEN")
)

mapview(parks_iso, zcol = "time", legend = TRUE) + mapview(parks, color = "red", cex = 2)

st_write(parks_iso, "data/processed/geojson/parks_iso.geojson", delete_dsn = TRUE)
st_write(parks_iso, "data/processed/shp/parks_iso.shp", delete_dsn = TRUE)

#merged/dissolved isochrones
parks_iso_dissolved <- parks_iso |> 
  group_by(time) |> 
  summarise(geometry = st_union(geometry))

#Check the difference between the dissolved and undissolved isochrones
leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(data = parks_iso_dissolved |> filter(time == 5),
              color = "blue", fillOpacity = 0.5, group = "5 Min Dissolved") |>
  addPolygons(data = parks_iso |> filter(time == 5),
              color = "red",  fillOpacity = 0.5, group = "5 Min Undissolved") |>
  addCircles(data = parks, group = "parks") |> 
  addLayersControl(
    overlayGroups = c("5 Min Dissolved", "5 Min Undissolved"),
    options = layersControlOptions(collapsed = FALSE)
  )

#dissolved isochrones
st_write(parks_iso_dissolved, "data/processed/geojson/parks_iso_dissolved.geojson", delete_dsn = TRUE)
st_write(parks_iso_dissolved, "data/processed/shp/parks_iso_dissolved.shp", delete_dsn = TRUE)


#------------------
#schools isochrones
#------------------
schools_iso <- mb_isochrone(
  schools, 
  time = c(5, 10, 15), 
  profile = "walking",
  access_token = Sys.getenv("MAPBOX_TOKEN")
)

st_write(schools_iso, "data/processed/geojson/schools_iso.geojson", delete_dsn = TRUE)
st_write(schools_iso, "data/processed/shp/schools_iso.shp", delete_dsn = TRUE)

#merged/dissolved isochrones
schools_iso_dissolved <- schools_iso |> 
  group_by(time) |> 
  summarise(geometry = st_union(geometry))

#dissolved isochrones
st_write(schools_iso_dissolved, "data/processed/geojson/schools_iso_dissolved.geojson", delete_dsn = TRUE)
st_write(schools_iso_dissolved, "data/processed/shp/schools_iso_dissolved.shp", delete_dsn = TRUE)


#------------------
#town_halls isochrones
#------------------
town_halls_iso <- mb_isochrone(
  town_halls, 
  time = c(5, 10, 15), 
  profile = "walking",
  access_token = Sys.getenv("MAPBOX_TOKEN")
)

st_write(town_halls_iso, "data/processed/geojson/town_halls_iso.geojson", delete_dsn = TRUE)
st_write(town_halls_iso, "data/processed/shp/town_halls_iso.shp", delete_dsn = TRUE)

#merged/dissolved isochrones
town_halls_iso_dissolved <- town_halls_iso |> 
  group_by(time) |> 
  summarise(geometry = st_union(geometry))

#dissolved isochrones
st_write(town_halls_iso_dissolved, "data/processed/geojson/town_halls_iso_dissolved.geojson", delete_dsn = TRUE)
st_write(town_halls_iso_dissolved, "data/processed/shp/town_halls_iso_dissolved.shp", delete_dsn = TRUE)
