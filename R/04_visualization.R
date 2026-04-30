library(tidyverse)

town_totals <- read_csv("data/processed/town_walkshed_summary.csv", col_types = cols(walk_time_min = col_factor()))
region_totals <- read_csv("data/processed/regional_walkshed_summary.csv", col_types = cols(walk_time_min = col_factor()))

town_totals <- town_totals |>
  mutate(walk_time_min = factor(walk_time_min, levels = c(5, 10, 15)),
         pct_within_fr = pct_within / 100)


#---------
#Food Retailers Walkshed by Town
#---------

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
           aes(y = pct_within_fr, fill = "15 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 10), 
           aes(y = pct_within_fr, fill = "10 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 5), 
           aes(y = pct_within_fr, fill = "5 minutes"), 
           position = "identity") +
  theme_minimal() +
  scale_fill_manual(name = "Walk Time",
                    values = c("5 minutes" = "steelblue", 
                               "10 minutes" = "darkseagreen3", 
                               "15 minutes" = "salmon"),
                    breaks = c("15 minutes", "10 minutes", "5 minutes")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = c(0.99,0.99),
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Percentage of Population within Walkshed of Food Retailers by Town",
       x = "Town",
       y = "Percentage of Population") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

ggsave("outputs/food_retailers_walkshed_by_town.png", width = 10, height = 6, dpi = 300)
  

#---------
#Healthcare Walkshed by Town
#---------

 town_order <- town_totals |>
  filter(destination == "Healthcare") |>
  filter(as.numeric(as.character(walk_time_min)) == 15) |>
  arrange(desc(pct_within)) |>
  pull(town) |>
  unique()

town_totals |>
  mutate(town = factor(town, levels = town_order)) |>
  filter(destination == "Healthcare") |>
  ggplot(aes(x = town)) +
  geom_col(data = ~filter(.x, walk_time_min == 15), 
           aes(y = pct_within_fr, fill = "15 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 10), 
           aes(y = pct_within_fr, fill = "10 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 5), 
           aes(y = pct_within_fr, fill = "5 minutes"), 
           position = "identity") +
  theme_minimal() +
  scale_fill_manual(name = "Walk Time",
                    values = c("5 minutes" = "steelblue", 
                               "10 minutes" = "darkseagreen3", 
                               "15 minutes" = "salmon"),
                    breaks = c("15 minutes", "10 minutes", "5 minutes")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = c(0.99,0.99),
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Percentage of Population within Walkshed of Healthcare Provider by Town",
       x = "Town",
       y = "Percentage of Population") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))
  
ggsave("outputs/healthcare_walkshed_by_town.png", width = 10, height = 6, dpi = 300)


#---------
#Libraries Walkshed by Town
#---------
distinct(town_totals, destination)

 town_order <- town_totals |>
  filter(destination == "Libraries") |>
  filter(as.numeric(as.character(walk_time_min)) == 15) |>
  arrange(desc(pct_within)) |>
  pull(town) |>
  unique()

town_totals |>
  mutate(town = factor(town, levels = town_order)) |>
  filter(destination == "Libraries") |>
  ggplot(aes(x = town)) +
  geom_col(data = ~filter(.x, walk_time_min == 15), 
           aes(y = pct_within_fr, fill = "15 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 10), 
           aes(y = pct_within_fr, fill = "10 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 5), 
           aes(y = pct_within_fr, fill = "5 minutes"), 
           position = "identity") +
  theme_minimal() +
  scale_fill_manual(name = "Walk Time",
                    values = c("5 minutes" = "steelblue", 
                               "10 minutes" = "darkseagreen3", 
                               "15 minutes" = "salmon"),
                    breaks = c("15 minutes", "10 minutes", "5 minutes")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = c(0.99,0.99),
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Percentage of Population within Walkshed of Library by Town",
       x = "Town",
       y = "Percentage of Population") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))
  
ggsave("outputs/library_walkshed_by_town.png", width = 10, height = 6, dpi = 300)


#---------
#Parks Walkshed by Town
#---------

town_order <- town_totals |>
  filter(destination == "Parks") |>
  filter(as.numeric(as.character(walk_time_min)) == 15) |>
  arrange(desc(pct_within)) |>
  pull(town) |>
  unique()

town_totals |>
  mutate(town = factor(town, levels = town_order)) |>
  filter(destination == "Parks") |>
  ggplot(aes(x = town)) +
  geom_col(data = ~filter(.x, walk_time_min == 15), 
           aes(y = pct_within_fr, fill = "15 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 10), 
           aes(y = pct_within_fr, fill = "10 minutes"), 
           position = "identity") +
  geom_col(data = ~filter(.x, walk_time_min == 5), 
           aes(y = pct_within_fr, fill = "5 minutes"), 
           position = "identity") +
  theme_minimal() +
  scale_fill_manual(name = "Walk Time",
                    values = c("5 minutes" = "steelblue", 
                               "10 minutes" = "darkseagreen3", 
                               "15 minutes" = "salmon"),
                    breaks = c("15 minutes", "10 minutes", "5 minutes")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = c(0.99,0.99),
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Percentage of Population within Walkshed of Park by Town",
       x = "Town",
       y = "Percentage of Population") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

ggsave("outputs/park_walkshed_by_town.png", width = 10, height = 6, dpi = 300)
  
