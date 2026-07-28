library(readxl)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(countrycode)


df <- read_excel("Project List.xlsx", skip = 1)
region_rows <- c(
  "Caribbean", "Central Asia", "East Asia and Pacific",
  "Eastern and Southern Africa", "Pacific 1",
  "South Asia", "Western and Central Africa", "Western Balkans"
)

country_counts <- df |>
  count(Country, name = "num_projects") |>
  filter(!Country %in% region_rows) |>
  mutate(
    iso3 = countrycode(
      Country,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Co-operative Republic of Guyana" = "GUY",
        "Lebanese Republic" = "LBN",
        "Republic of Kosovo" = "XKX"
      )
    )
  )

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent != "Antarctica")

map_data <- world |>
  left_join(country_counts, by = c("iso_a3" = "iso3"))

plot_map <- ggplot(map_data) +
  geom_sf(aes(fill = num_projects)) +
  scale_fill_gradientn(
    colours = c("green", "yellow", "red"),
    na.value = "#dfedd5"
  ) +
  labs(
    title = "Global Distribution of Digital Technology Projects",
    fill = "Projects"
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("digitization_heatmap.png", plot_map, width = 12, height = 6)
