library(dplyr)
library(ggplot2)
library(readxl)
library(scales)

# Load data
data <- read_excel("PLACEHOLDER.xlsx")

# Rename and clean
data_clean <- data %>%
  rename(
    Red = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Nombre de su Red:_1 [6406014]`,
    Servicio = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]`,
    Hours = `[Información Servicio Diagnóstico] Horas de atención_1 [6406166]`,
    Region = `[Persona Jurídica Localización] Región_1 [6406076]`,
    Comuna = `[Persona Jurídica Localización] Comuna_1 [6406078]`
  ) %>%
  mutate(
    Hours = gsub(",", ".", Hours),  # Replace comma with dot
    Hours = as.numeric(Hours),       # Convert to numeric
    Region = gsub("\\s*(Regi[oó]n)\\s*$", "", Region, perl = TRUE),
    Region = gsub("^Regi[oó]n\\s+de\\s+", "", Region, perl = TRUE)
  ) %>%
  filter(!is.na(Hours), !is.na(Comuna), !is.na(Region), Servicio == "Diagnóstico")

# Summarise total hours per Comuna
comuna_summary <- data_clean %>%
  group_by(Comuna) %>%
  summarise(Total_Hours = sum(Hours, na.rm = TRUE), .groups = "drop") %>%
  mutate(prop = Total_Hours / sum(Total_Hours),
         cumprop = cumsum(prop))
top_comunas <- comuna_summary %>%
  filter(cumprop <= 0.85) %>%
  pull(Comuna)

data_clean <- data_clean %>%
  mutate(
    Comuna_grouped = ifelse(Comuna %in% top_comunas, Comuna, "Other"),
    Comuna_grouped = factor(Comuna_grouped, levels = c(sort(unique(top_comunas)), "Other"))
  )
data_summary <- data_clean %>%
  group_by(Region, Comuna_grouped) %>%
  summarise(Total_Hours = sum(Hours, na.rm = TRUE), .groups = "drop")

poverty_data <- read_excel("pone.0323409.s001.xlsx") %>%
  mutate(`Comuna name` = trimws(`Comuna name`))

data_poverty <- data_clean %>%
  left_join(poverty_data, by = c("Comuna" = "Comuna name")) %>%
  mutate(
    pov_group = cut(
      pov2022,
      breaks = c(0, 5, 10, 20, 50, 100),
      labels = c("0-5%", "5-10%", "10-20%", "20-50%", "50-100%"),
      include.lowest = TRUE
    )
  )

data_poverty_summary <- data_poverty %>%
  group_by(Region, Comuna, pov_group) %>%
  summarise(Total_Hours = sum(Hours, na.rm = TRUE), .groups = "drop")


# Plot stacked bar chart
plot_stacked <- ggplot(data_summary, aes(
  x = Region,
  y = Total_Hours,
  fill = Comuna_grouped
)) +
  geom_col(position = "stack") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(size = 18, face = "bold")
  ) + 
  scale_fill_manual(
    values = c(
      setNames(scales::hue_pal()(length(top_comunas)), sort(unique(top_comunas))),
      "Other" = "grey70"
    )
  ) +
  labs(
    title = "Total Hours of Diagnostic Services by Region (Stacked by Comuna)",
    x = "Region",
    y = "Total Hours",
    fill = "Comuna"
  )

# Save the plot
ggsave("stacked_hours_plot.png", plot = plot_stacked, width = 12, height = 8)

plot_stacked_poverty <- ggplot(data_poverty_summary, aes(
  x = Region,
  y = Total_Hours,
  fill = pov_group
)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_brewer(palette = "Reds", na.value = "grey80") +
  labs(
    title = "Total Diagnostic Hours by Region (Stacked by Comuna Poverty Level)",
    x = "Region",
    y = "Total Hours",
    fill = "Poverty % 2022"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 30), 
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(size = 15)
  )

# Save the poverty-stacked plot
ggsave("stacked_poverty_hours_plot.png", plot = plot_stacked_poverty, width = 12, height = 8)
