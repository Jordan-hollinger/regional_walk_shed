library(tidyverse)
library(tidycensus)
library(mapview)
library(sf)

load_variables(2020, "pl")

####-----
#pull in all census blocks in worcester county
####
census_blocks <- get_decennial(
  geography = "block",
  variable = "P1_001N",
  state = "MA",
  county = "Worcester",
  geometry = TRUE,
  year = 2020
)

mapview(census_blocks)

####-----
#pull in all county subdivisions in CMRPC region
####
towns <- c(
  "Auburn", "Barre", "Berlin", "Blackstone", "Boylston", "Brookfield", "Charlton",
  "Douglas", "Dudley", "East Brookfield", "Grafton", "Hardwick", "Holden", "Hopedale",
  "Leicester", "Mendon", "Millbury", "Millville", "New Braintree", "North Brookfield",
  "Northborough", "Northbridge", "Oakham", "Oxford", "Paxton", "Princeton", "Rutland",
  "Shrewsbury", "Southbridge", "Spencer", "Sturbridge", "Sutton", "Upton", "Uxbridge",
  "Warren", "Webster", "West Boylston", "West Brookfield", "Westborough", "Worcester"
)

town_boundaries <- get_decennial(
  geography = "county subdivision",
  state = "MA",
  variable = "P1_001N",
  county = c("Worcester"),
  geometry = TRUE,
  year = 2020
) |> 
  mutate(NAME = NAME |> 
           str_extract("^[^,]+") |>
           str_remove("\\s+(town|city|Town city)$") |>
           str_trim()) |> 
  filter(NAME %in% towns)

sf::st_crs(town_boundaries) == sf::st_crs(census_blocks)

mapview(town_boundaries)

####-----
#only keep census blocks that are within the town boundaries
####
census_block_centroids <- st_centroid(census_blocks)

mapview(census_block_centroids)

cmrpc_census_block_centroids <- st_join(
  census_block_centroids,
  town_boundaries,
  suffix = c("_blocks", "_towns"),
  join = st_within) |> 
  filter(!is.na(NAME_towns))

mapview(cmrpc_census_block_centroids)

cmrpc_census_blocks <- census_blocks |>
  filter(GEOID %in% cmrpc_census_block_centroids$GEOID_blocks) |>
  left_join(st_drop_geometry(cmrpc_census_block_centroids) |> select(GEOID_blocks, NAME_towns), join_by(GEOID == GEOID_blocks)) |> 
  rename(
    town = NAME_towns,
    block = NAME,
    population = value
  )

mapview(cmrpc_census_blocks)

#check population rollups by town
cmrpc_census_blocks |> 
  group_by(town) |> 
  summarise(total_pop = sum(population)) |> 
  arrange(desc(total_pop))

st_crs(cmrpc_census_blocks)

cmrpc_census_blocks <- st_transform(cmrpc_census_blocks, 26986)

st_write(cmrpc_census_blocks, "data/processed/geojson/cmrpc_census_blocks.geojson", delete_dsn = TRUE)


