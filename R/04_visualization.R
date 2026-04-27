library(tidyverse)

town_totals <- read_csv("data/processed/town_walkshed_summary.csv", col_types = cols(walk_time_min = col_factor()))
region_totals <- read_csv("data/processed/regional_walkshed_summary.csv", col_types = cols(walk_time_min = col_factor()))

town_totals <- town_totals |>
  mutate(walk_time_min = factor(walk_time_min, levels = c(5, 10, 15)),
         pct_within = pct_within / 100)


town_order <- town_totals |>
  filter(destination == "Food Retailers") |>
  filter(as.numeric(as.character(walk_time_min)) == 15) |>
  arrange(desc(pct_within)) |>
  pull(town) |>
  unique()

town_totals |>
  mutate(town = factor(town, levels = town_order)) |>
  filter(destination == "Food Retailers") |>
  ggplot(aes(x = town)) +
  geom_col(data = ~filter(.x, walk_time_min == 15), 
           aes(y = pct_within), fill = "salmon",
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 10), 
           aes(y = pct_within), fill = "darkseagreen3", 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 5), 
           aes(y = pct_within), fill = "steelblue",
           position = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Percentage of Population within Walkshed of Food Retailers by Town",
       x = "Town",
       y = "Percentage of Population") +
  scale_y_continuous(labels = scales::percent)
  
