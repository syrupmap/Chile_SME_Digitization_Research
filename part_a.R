install.packages("ggplot2")
install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

### a. Plot the distribution of "Diagnosticos" (from column G: [SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]) 
###     by Comuna (from column M: [Persona Jurídica Localización] Comuna_1 [6406078]) 
###     by Red (from column F: [SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Nombre de su Red:_1 [6406014])

data <- read_excel("PLACEHOLDER.xlsx")


data_clean <- data %>%
  rename(
    Red = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Nombre de su Red:_1 [6406014]`,
    Servicio = `[SERVICIOS RED DE ASISTENCIA DIGITAL FORTALECE PYME] Tipo de Servicio:_1 [6406016]`,
    Region = `[Persona Jurídica Localización] Región_1 [6406076]`,
    Comuna = `[Persona Jurídica Localización] Comuna_1 [6406078]`
  )


# Distribution of Services per Region by Red
# Plot where x is the region or red and comunas is fill and y is the total quantity of diagnostics 
# plot_region <- ggplot(data_clean, aes(
#   x = Region,
#   # if (Servicio == "Diagnosticos"){
#   #   y = Servicio
#   # }, 
#   fill = Comuna
# )) +
#   geom_bar() +
#   facet_wrap(~ Red) +
#   coord_flip() +
#   theme_minimal() +
#   theme(legend.position = "hidden") +
#   labs(
#     title = "Distribution of Services per Region by Red",
#     x = "Region",
#     y = "Number of Services"
#   )
# png("plot_region.png", width=1200, height=800)
# print(plot_region)
# dev.off()


### b. Repeat a. with "Asistencia Tecnica", "Asistencia Tecnica Iniciada" and "Atencion Integral Multinivel".
# Since 4 different types of services, y as service will collapse all and 

data_diag <- data_clean %>%
  filter(Servicio == "Diagnóstico", !is.na(Comuna), !is.na(Region))

data_diag <- data_diag %>%
  mutate(
    Red_clean = Red %>%
      str_remove("^\\S+\\s+") %>%              # remove leading code
      str_to_title() %>%                      # nicer capitalization
      str_replace_all("\\s+", " ") %>%        # clean extra spaces
      str_replace("U\\. De", "Universidad de") %>% 
      str_replace("U\\. Cat", "Universidad cat")       # split org and region
  )

# Group comunas by region and make a different plot per region
regions <- unique(data_diag$Region)
for (r in regions) { # For each region I filtered it (no more blank ones) and then count by Communa
  df_region <- data_diag %>%
    filter(Region == r) %>%
    count(Comuna)
  p <- ggplot(df_region, aes(
    x = reorder(Comuna, n),
    y = n
  )) +
    geom_col(fill = "purple") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Diagnostics in", r),
      x = "Comuna",
      y = "Number of Services"
    )
  ggsave(paste0("plot_", r, ".png"), plot = p, width = 10, height = 6)
}


# Stacked Comuna’s inside each Region
comuna_counts <- data_diag %>% #selected top comuna's 
  count(Comuna, sort = TRUE) %>%
  mutate(
    prop = n / sum(n),
    cumprop = cumsum(prop)
  )
top_comunas <- comuna_counts %>%
  filter(cumprop <= 0.85) %>%   
  pull(Comuna)
data_diag_grouped <- data_diag %>%
  mutate(Comuna_grouped = ifelse(Comuna %in% top_comunas, Comuna, "Other"))
  data_diag_grouped$Comuna_grouped <- factor(
  data_diag_grouped$Comuna_grouped,
  levels = c(sort(unique(top_comunas)), "Other")
)
data_diag_summary <- data_diag_grouped %>% #stacking comuna's within each region
  count(Region, Comuna_grouped)
plot_comuna <- ggplot(data_diag_summary, aes(
  x = Region,
  y = n,
  fill = Comuna_grouped
)) +
  geom_col(position = "stack") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "hidden") + 
  scale_fill_manual(
    values = c(
      setNames(scales::hue_pal()(length(top_comunas)), sort(unique(top_comunas))),
      "Other" = "grey70"
    )
  ) + 
  labs(
    title = "Diagnostics by Region (Stacked Communes)",
    x = "Region",
    y = "Number of Services",
    fill = "Comuna"
  ) + theme(
    legend.position = "none",
    plot.title = element_text(size = 30), 
    axis.title.x = element_text(size = 120),
    axis.title.y = element_text(size = 120),
    axis.text = element_text(size = 30),
    axis.text.x = element_text(size = 30)
  ) 
  png("plot_comuna.png", width=2400, height=1600)
  print(plot_comuna)
  dev.off()
  


# Services by Red with stacked comuna's
data_diag_grouped <- data_diag_grouped %>%
  mutate(Region_Red = ifelse(
    Region %in% c("Region de Valparaiso", "Region del Biobio"),
    paste(Region, Red, sep = " - "),
    Region
  ))
data_diag_summary <- data_diag_grouped %>%
  count(Region_Red, Comuna_grouped)
plot_normal_red_stack <- ggplot(data_diag_summary, aes(
  x = Region_Red,
  y = n,
  fill = Comuna_grouped
)) +
  geom_col(position = "stack") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 30), 
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(size = 15)
  ) + 
  labs(
    title = "Diagnostics by Region (Stacked Communes)",
    x = "Region",
    y = "Number of Services",
    fill = "Comuna"
  )
  png("plot_normal_red_stack.png", width=1200, height=800)
  print(plot_normal_red_stack)
  dev.off()



# Services by Red with stacked comuna's (TOP 7)
comuna_counts <- data_diag %>%
  count(Comuna, sort = TRUE)
top_comunas <- comuna_counts %>%
  slice_max(n, n = 8) %>%   # adjust number
  pull(Comuna)
data_diag_grouped <- data_diag %>%
  mutate(
    Comuna_grouped = ifelse(Comuna %in% top_comunas, Comuna, "Other")
  )
data_diag_summary <- data_diag_grouped %>%
  count(Red_clean, Comuna_grouped)
top_colors <- colorRampPalette(c("#fdbbb4", "red"))(length(top_comunas))
names(top_colors) <- top_comunas
plot_region_red <- ggplot(data_diag_summary, aes(
  x = Red_clean,
  y = n,
  fill = Comuna_grouped
)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      top_colors,
      "Other" = "grey80"
    )
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", 
  legend.position = "none",
    plot.title = element_text(size = 60), 
    axis.title.x = element_text(size = 60),
    axis.title.y = element_text(size = 60),
    axis.text = element_text(size = 60),
    axis.text.x = element_text(size = 60))) + 
  labs(
    title = "Services by Red with Stacked Communes",
    fill = "Comuna",
  ) 
png("plot_region_red.png", width=1200, height=800)
print(plot_region_red)
dev.off()

