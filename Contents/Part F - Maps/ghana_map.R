library (sf)         # For handling spatial data
library (ggplot2)    # For plotting
library (dplyr)

# Step 1: Load a GeoJSON file with Ghana's regions
# https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-ghana/resource/8137658f-9bf7-4ad4-8a33-6128e2f931a2
ghana_regions <- st_read("data/geoBoundaries-GHA-ADM1_simplified.geojson")

# Step 2: Draw map of Ghana regions
ggplot(ghana_regions)+
  geom_sf()

ggplot(ghana_regions) +
  geom_sf(fill = "lightblue", color = "black") +
  geom_sf_text(aes(label = stringr::str_remove(shapeName, " Region")), size = 3) +
  labs(
    title = "Map of Ghana with Region Names",
    caption = "Data source: Ghana shapefile"
  ) +
  theme_minimal()


# Step 3: Add summary statistics on regional level
ibes_reg_stats <- readRDS("data/ibes_reg_stats_20241128.rds") |> 
  select (-reg_code)

reg_cor <- openxlsx::read.xlsx("data/region_correspondance.xlsx")

ibes_reg_stats_ext <- ibes_reg_stats |> 
  left_join(reg_cor, by="reg_name") |> 
  select (-reg_name)

ghana_regions <- ghana_regions |> 
  left_join(ibes_reg_stats_ext, by="shapeName")

rm (ibes_reg_stats, ibes_reg_stats_ext, reg_cor)

# Step 4: Fill region with mean values of employees
ggplot(ghana_regions) +
  geom_sf(aes(fill = mean_size))
  
# Step 5
p <- ggplot(ghana_regions) +
  geom_sf(aes(fill = mean_size), color = "grey", size = 0.3) +
  scale_fill_viridis_c(option = "viridis", na.value = "gray90", name = "Attribute") +
  labs(title = "Tony's map of Ghana",
       subtitle = "Regions colored by mean number of employees",
       caption = "Source: IBES") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10)
  )

p

ggsave ("data/tonys_map_20241128.pdf", 
        plot = p,
        height = 8,
        width = 6,
        units = "in")
