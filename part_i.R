library(dplyr)
library(ggplot2)
library(readxl)
### [DO THIS] i. Plot the distribution of "numero de iteraciones" (from column G: [SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]) 
###     by "Tamaño Empresa Registrada en SII_1" (from column Q: [Datos Informativos desde el SII] Tamaño Empresa Registrada en SII_1 [6406123])
###     by Red (from column F: [SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Nombre de su Red:_1 [6406014])

data <- read_excel("PLACEHOLDER.xlsx")

data_hours_analysis <- data %>%
  rename(
    Iteraciones = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]`,
    Tamano = `[Datos Informativos desde el SII] Tamaño Empresa Registrada en SII_1 [6406123]`,
    Red = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Nombre de su Red:_1 [6406014]`
  ) %>%
  filter(!is.na(Iteraciones), !is.na(Tamano), !is.na(Red)) %>%
  mutate(
    Iteraciones = as.numeric(Iteraciones)  # important if it's read as text
  )

# Outcomes by Region
# data_summary <- data %>%
#   rename(
#     Region = `[Persona Jurídica Localización] Región_1 [6406076]`,
#     Servicio = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]`
#   ) %>%
#   filter(!is.na(Region), !is.na(Servicio)) %>%
#   group_by(Region, Servicio) %>%
#   summarise(n = n(), .groups = "drop")
data_summary <- data %>%
  rename(
    Region = `[Persona Jurídica Localización] Región_1 [6406076]`,
    Services = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]`
  ) %>%
  mutate(
    Services = str_squish(Servicio),
    
    Services = case_when(
      str_detect(Servicio, "Finalizada") ~ "Technical Assistance Completed",
      str_detect(Servicio, "Iniciada") ~ "Technical Assistance Started",
      str_detect(Servicio, "Multinivel") ~ "Multilevel Comprehensive Support",
      str_detect(Servicio, "Diagn") ~ "Diagnostic Assessment",  # catches Diagnóstico
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(Region), !is.na(Services)) %>%
  group_by(Region, Servicio) %>%
  summarise(n = n(), .groups = "drop")
plot_outcomesByRegion <- ggplot(data_summary, aes(x = Region, y = n, fill = Services)) +
  geom_col(position = "fill") +
  coord_flip() + 
  theme(
    plot.title = element_text(size = 30), 
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(size = 15)
  )
 ggsave("plot_outcomesByRegion.png", plot = plot_outcomesByRegion, width = 10, height = 6)




# #   Hours by Region
# plot_hoursByRegion <- ggplot(data_summary, aes(x = Region, y = Hours)) +
#   geom_boxplot() +
#   coord_flip() + 
#   theme(
#     legend.position = "none",
#     plot.title = element_text(size = 30), 
#     axis.title.x = element_text(size = 30),
#     axis.title.y = element_text(size = 30),
#     axis.text = element_text(size = 15),
#     axis.text.x = element_text(size = 15)
#   )
# ggsave("plot_hoursByRegion.png", plot = plot_hoursByRegion, width = 10, height = 6)

