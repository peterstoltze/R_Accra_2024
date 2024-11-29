library(dplyr)
library(sf)
library(ggplot2)

ghana_regions <- st_read("data/geoBoundaries-GHA-ADM1_simplified.geojson")

reg_cor <- openxlsx::read.xlsx("data/region_correspondance.xlsx")

ghana_regions <- ghana_regions |> 
  left_join(reg_cor, by="shapeName")

# Summarize by quasiregion to collapse geometries
ghana_quasiregions <- ghana_regions |> 
  group_by(reg_name) |> 
  summarize(geometry = st_union(geometry), .groups = "drop")

# Plot the quasiregions
ggplot(ghana_quasiregions) +
  geom_sf(aes(fill = reg_name), color = "black") +  # Fill by quasiregion
  labs(
    title = "Map of Ghana by Quasiregions",
    caption = "Data source: Ghana shapefile"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Quasiregions") +  # Nice color palette
  theme_minimal()
