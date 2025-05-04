packages <- c("tidyverse", "tidycensus", "tigris", "sf", "showtext")

# Install any missing packages
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load the packages
lapply(packages, library, character.only = TRUE)
# https://walker-data.com/tidycensus/articles/other-datasets.html
## You might 



net_migration <- get_estimates(geography = "county",
                               variables = "RNETMIG",
                               vintage = 2023,
                               geometry = TRUE,
                               resolution = "20m") %>%
  shift_geometry()

net_migration


font_add_google("Roboto")
showtext_auto()

order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration <- net_migration %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>%
  filter(GEOID != "72") %>%
  shift_geometry()

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), size = 0.1) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto", base_size = 18) +
  labs(title = "Net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2023 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | @kyle_e_walker")
