# Loading the poverty dataset 

install.packages("ggplot2")
install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
data <- read_excel("PLACEHOLDER.xlsx")
poverty_data <- read_excel("pone.0323409.s001.xlsx")
data_clean <- data %>%
  rename(
    Red = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Nombre de su Red:_1 [6406014]`,
    Servicio = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]`,
    Region = `[Persona Jurídica Localización] Región_1 [6406076]`,
    Comuna = `[Persona Jurídica Localización] Comuna_1 [6406078]`
  )

library(dplyr)
library(stringr)
data_clean <- data_clean %>%
  mutate(Comuna = str_trim(Comuna))
poverty_data <- poverty_data %>%
  mutate(`Comuna name` = str_trim(`Comuna name`))
data_summary <- data_clean %>%
  filter(Servicio == "Diagnóstico") %>%
  group_by(Comuna) %>%
  summarise(Diagnostics = n(), .groups = "drop")
data_with_poverty <- data_summary %>%
  left_join(poverty_data, by = c("Comuna" = "Comuna name"))

# plot_poor <- ggplot(data_with_poverty, aes(x = reorder(Comuna, pov2022), y = Diagnostics, fill = pov2022)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_gradient(low = "lightblue", high = "darkred") +
#   labs(title = "Number of Services per Comuna (2022 Poverty Highlighted)",
#        x = "Comuna",
#        y = "Number of Diagnostics",
#        fill = "Poverty % 2022") +
#   theme_minimal()
#    png("plot_poor.png", width=1200, height=800)
#   print(plot_poor)
#   dev.off()


#### Plotting the top poorest comuna's, how many services
#   top_comunas <- data_with_poverty %>%
#   arrange(desc(Diagnostics)) %>%
#   slice(1:20)  # top 20 comunas

# plot_top_poor <- ggplot(top_comunas, aes(x = reorder(Comuna, Diagnostics), y = Diagnostics, fill = pov2022)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_gradient(low = "lightblue", high = "darkred") +
#   labs(title = "Poverty Levels of Top 20 Comunas with MOST Services (2022 Poverty Highlighted)",
#        x = "Comuna",
#        y = "Number of Services",
#        fill = "Poverty % 2022") +
#   theme_minimal()
#   png("plot_top_poor.png", width=1200, height=800)
#   print(plot_top_poor)
#   dev.off()



##### Plotting richest communa's, how many services
# diagnostics_per_comuna <- data_clean %>%
#   filter(Servicio == "Diagnóstico") %>%
#   group_by(Comuna) %>%
#   summarise(Diagnostics = n(), .groups = "drop") %>%
#   left_join(poverty_data, by = c("Comuna" = "Comuna name"))
# bottom_comunas <- data_clean %>%
#   filter(Servicio == "Diagnóstico") %>%
#   count(Comuna, name = "Diagnostics") %>%  # count occurrences per Comuna
#   left_join(poverty_data, by = c("Comuna" = "Comuna name")) %>%
#   arrange(Diagnostics) %>%                 # sort ascending
#   slice(1:20)                         # take bottom 10
# plot_bottom_poor <- ggplot(bottom_comunas, aes(x = reorder(Comuna, Diagnostics), y = Diagnostics, fill = pov2022)) +
#   geom_col() +
#   coord_flip() +
#   scale_fill_gradient(low = "lightblue", high = "darkred") +
#   labs(title = "Poverty Levels of Bottom 10 Comunas with Fewest Services (2022 Poverty Highlighted)",
#        x = "Comuna",
#        y = "Number of Services",
#        fill = "Poverty % 2022") +
#   theme_minimal()
# png("plot_bottom_poor.png", width=1200, height=800)
# print(plot_bottom_poor)
# dev.off()



data_with_poverty <- data_with_poverty %>%
  mutate(pov_group = cut(pov2022, breaks = c(0,5,10,20,50,100), labels=c("0-5%","5-10%","10-20%","20-50%","50-100%")))

plot_binned_poverty <- ggplot(data_with_poverty, aes(x = pov_group, y = Diagnostics, fill = pov_group)) +
  geom_col(width = 0.5) +
  labs(title = "Number of Services by Poverty Level (2022)",
       x = "Poverty % Group",
       y = "Number of Services") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 40, face = "bold"),
    axis.title = element_text(size = 30, face = "bold"),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(size = 20)
  )

png("plot_binned_poverty.png", width = 1200, height = 800)
print(plot_binned_poverty)
dev.off()

# BROKEN I DUNNO
# data_binned <- data_clean %>%
#   filter(Servicio == "Diagnostics") %>%
#   left_join(poverty_data, by = c("Comuna" = "Comuna name")) %>%
#   mutate(pov_bin = cut(pov2022, breaks = seq(0, 50, 5), include.lowest = TRUE)) %>%
#   group_by(pov_bin) %>%
#   summarise(services = n(), .groups = "drop")

# plot_servicesByPoverty <- ggplot(data_binned, aes(x = pov_bin, y = services)) +
#   geom_col(fill = "steelblue") +
#   labs(title = "Services by Poverty Level Bin", x = "Poverty Bin (%)", y = "Number of Diagnostics") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# png("plot_servicesByPoverty.png", width=1200, height=800)
#   print(plot_servicesByPoverty)
#   dev.off()
