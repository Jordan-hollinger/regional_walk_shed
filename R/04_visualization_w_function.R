library(tidyverse)

town_totals <- read_csv("data/processed/town_walkshed_summary.csv", col_types = cols(walk_time_min = col_factor()))
region_totals <- read_csv("data/processed/regional_walkshed_summary.csv", col_types = cols(walk_time_min = col_factor()))

town_totals <- town_totals |>
  mutate(walk_time_min = factor(walk_time_min, levels = c(5, 10, 15)),
         pct_within_fr = pct_within / 100)

destinations <- unique(town_totals$destination)

#---------
#town graph function
#---------

town_graph_func <- function(dfr, dest)
{
town_order <- dfr |>
  filter(destination == dest) |>
  filter(as.numeric(as.character(walk_time_min)) == 15) |>
  arrange(desc(pct_within)) |>
  pull(town) |>
  unique()

dfr |>
  mutate(town = factor(town, levels = town_order)) |>
  filter(destination == dest) |>
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
  labs(title = paste("Percentage of Population within Walkshed of ", dest, " by Town"),
       x = "Town",
       y = "Percentage of Population") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

ggsave(paste("outputs/",dest,"_walkshed_by_town.png"), width = 10, height = 6, dpi = 300)
  
}

map(destinations, ~ town_graph_func(town_totals, .x))


#---------
#Region graph 
#---------

region_totals <- region_totals |>
  mutate(walk_time_min = factor(walk_time_min, levels = c(15, 10, 5)),
         pct_within_fr = pct_within / 100)

dest_order <- region_totals |>
  filter(as.numeric(as.character(walk_time_min)) == 15) |>
  arrange(desc(pct_within)) |>
  pull(destination) |>
  unique()

region_totals |>
  mutate(destination = factor(destination, levels = dest_order)) |>
  ggplot(aes(x = destination)) +
  geom_col(position = "dodge", aes(y = pct_within_fr, fill = walk_time_min)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = c(0.99,0.99),
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = paste("Percentage of Regional Population within Walkshed of Destinations"),
       x = "Destination",
       y = "Percentage of Population") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(name = "Walk Time",
                    values = c("5" = "steelblue", 
                               "10" = "darkseagreen3", 
                               "15" = "salmon"),
                    breaks = c("15", "10", "5"),
                    labels = c("15 minutes", "10 minutes", "5 minutes"))

ggsave(paste("outputs/region_walkshed_by_dest.png"), width = 10, height = 6, dpi = 300)
