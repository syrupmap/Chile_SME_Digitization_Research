library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(readxl)

# Load data
data <- read_excel("PLACEHOLDER.xlsx")
poverty_data <- read_excel("PLACEHOLDER.xlsx")

# Clean names
data_clean <- data %>%
  rename(
    Red = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Nombre de su Red:_1 [6406014]`,
    Servicio = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]`,
    Region = `[Persona Jurídica Localización] Región_1 [6406076]`,
    Comuna = `[Persona Jurídica Localización] Comuna_1 [6406078]`
  ) %>%
  mutate(
    Comuna = str_trim(Comuna),
    Servicio = str_trim(Servicio),
    Region = stri_trans_general(Region, "Latin-ASCII") %>%
      str_remove_all(regex("\\s*(Region)\\s*$", ignore_case = TRUE)) %>%
      str_replace_all(regex("^Region\\s+de\\s+", ignore_case = TRUE), "") %>%
      str_trim()
  )

poverty_data <- poverty_data %>%
  mutate(`Comuna name` = str_trim(`Comuna name`))

# Filter diagnostics only and join poverty
data_diag <- data_clean %>%
  filter(Servicio == "Diagnóstico") %>%
  left_join(poverty_data, by = c("Comuna" = "Comuna name"))

# Bin poverty levels
data_diag <- data_diag %>%
  mutate(pov_group = cut(
    pov2022,
    breaks = c(0,5,10,20,50,100),
    labels = c("0-5%","5-10%","10-20%","20-50%","50-100%"),
    include.lowest = TRUE
  ))

# Aggregate counts by Region and Comuna
data_summary <- data_diag %>%
  group_by(Region, Comuna, pov_group) %>%
  summarise(services = n(), .groups = "drop")

# Plot stacked bars: Comunas stacked, colored by poverty group
plot_comuna_poverty <- ggplot(data_summary, aes(
  x = Region,
  y = services,
  fill = pov_group
)) +
  geom_col(position = "stack") +
  coord_flip() +
  # scale_y_continuous(
  #   breaks = seq(0, ceiling(max(data_summary$services) / 50) * 50, by = 50),
  #   expand = c(0, 0)
  # ) +
  scale_fill_brewer(palette = "Reds") +  # red palette to show higher poverty = darker
  labs(
    title = "Number of Services by Region (Stacked Comunas, Colored by Poverty Level)",
    x = "Region",
    y = "Number of Services",
    fill = "Poverty % 2022"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 30), 
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(size = 15)
  )
# Save
png("plot_comuna_poverty.png", width=1200, height=800)
print(plot_comuna_poverty)
dev.off()