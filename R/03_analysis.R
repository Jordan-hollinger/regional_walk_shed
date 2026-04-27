library(tidyverse)
library(sf)

# Paths
geojson_dir <- "data/processed/geojson"
out_dir     <- "data/processed"

# Destination categories: display label -> file prefix
destinations <- list(
  "Food Retailers" = "food",
  "Libraries"      = "library",
  "Parks"          = "parks",
  "Schools"        = "schools",
  "Town Halls"     = "town_halls",
  "Healthcare"     = "healthcare",
  "Post Offices"   = "post_offices"
)

# --- Load census blocks ---------------------------------------------------

census_blocks <- st_read(file.path(geojson_dir, "cmrpc_census_blocks.geojson"),
                         quiet = TRUE) |>
  mutate(block_area = st_area(geometry))

regional_total_pop <- sum(census_blocks$population, na.rm = TRUE)

town_totals <- census_blocks |>
  st_drop_geometry() |>
  group_by(town) |>
  summarise(town_total_pop = sum(population, na.rm = TRUE), .groups = "drop")

# --- Area-weighted population function ------------------------------------

# Returns a long tibble with columns: time, pop_within, [town]
calc_walkshed_pop <- function(census_blocks, iso_dissolved, by_town = FALSE) {
  fragments <- st_intersection(census_blocks, iso_dissolved) |>
    mutate(frag_area   = st_area(geometry),
           weighted_pop = as.numeric(frag_area / block_area) * population) |>
    st_drop_geometry()

  group_cols <- if (by_town) c("time", "town") else "time"

  fragments |>
    group_by(across(all_of(group_cols))) |>
    summarise(pop_within = sum(weighted_pop, na.rm = TRUE), .groups = "drop")
}

# --- Regional summary -----------------------------------------------------

regional_results <- imap(destinations, function(prefix, label) {
  iso <- st_read(file.path(geojson_dir, paste0(prefix, "_iso_dissolved.geojson")),
                 quiet = TRUE) |>
    st_transform(st_crs(census_blocks))
  calc_walkshed_pop(iso, census_blocks, by_town = FALSE) |>
    mutate(destination = label)
}) |>
  bind_rows() |>
  mutate(
    walk_time_min     = time,
    total_regional_pop = regional_total_pop,
    pop_within        = round(pop_within),
    pct_within        = round(pop_within / total_regional_pop * 100, 1)
  ) |>
  select(destination, walk_time_min, total_regional_pop, pop_within, pct_within)

# --- Town-level summary ---------------------------------------------------


town_results <- imap(destinations, function(prefix, label) {
  iso <- st_read(file.path(geojson_dir, paste0(prefix, "_iso_dissolved.geojson")),
                 quiet = TRUE) |>
    st_transform(st_crs(census_blocks))
  calc_walkshed_pop(iso, census_blocks, by_town = TRUE) |>
    mutate(destination = label)
}) |>
  bind_rows() |>
  left_join(town_totals, by = "town") |>
  mutate(
    walk_time_min  = time,
    pop_within     = round(pop_within),
    pct_within     = round(pop_within / town_total_pop * 100, 1)
  ) |>
  select(town, destination, walk_time_min, town_total_pop, pop_within, pct_within) |>
  arrange(town, destination, walk_time_min)

# --- Write outputs --------------------------------------------------------

write_csv(regional_results, file.path(out_dir, "regional_walkshed_summary.csv"))
write_csv(town_results,     file.path(out_dir, "town_walkshed_summary.csv"))

message("Done. Outputs written to ", out_dir)
