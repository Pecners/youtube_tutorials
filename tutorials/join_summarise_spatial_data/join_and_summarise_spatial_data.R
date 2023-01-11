library(raster)
library(sf)
library(tidyverse)
library(osmdata)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

# download france country boundaries

france <- getData("GADM", country = "FRA", level = 0) |> 
  st_as_sf()
 
france |> 
  ggplot() +
  geom_sf()

# get france bounding box

france_bb <- st_bbox(france)

france_bb |> 
  st_as_sfc() |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = france, color = "red", fill = "red")

# download france level 2 boundaries

fr_level_2 <- getData("GADM", country = "FRA", level = 2) |> 
  st_as_sf()

fr_level_2 |> 
  ggplot() +
  geom_sf()

# download winery locations from OpenStreetMap

wineries <- opq(france_bb) |> 
  add_osm_feature(key = "craft", value = "winery") |> 
  osmdata_sf()

wine_points <- wineries$osm_points |> 
  select(geometry)

france |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = wine_points,
          alpha = .25)

# summarise winery locations within departments

joined <- st_join(wine_points, fr_level_2, left = FALSE)

sum_joined <- joined |> 
  group_by(GID_2) |> 
  count()

# join counts with departments

less_geo <- sum_joined |> 
  as_tibble() |> 
  select(-geometry)

level_2_counts <- left_join(fr_level_2, less_geo)

level_2_counts |> 
  ggplot(aes(fill = n)) + 
  geom_sf()


# convert wineries data out of sf format

wine_coords <- wine_points |> 
  st_coordinates() |> 
  as_tibble()

wine_coords_sf <- wine_coords |> 
  st_as_sf(coords = c("X", "Y"), crs = 4326)

wine_coords_sf |> 
  ggplot() +
  geom_sf()

# group by hex grid

fr_new_crs <- st_transform(france, crs = 2154)

hex_grid <- fr_new_crs |> 
  st_make_grid(square = FALSE, cellsize = 50000)

hex_grid |> 
  ggplot() +
  geom_sf() +
  coord_sf(crs = 2154)

fr_hex <- hex_grid[fr_new_crs]

fr_hex |> 
  ggplot() +
  geom_sf() +
  coord_sf(crs = 2154)


# summarise winery locations within hexes
fr_hex_sf <- st_sf(fr_hex) |> 
  mutate(ind = row_number())
wine_points_hex <- st_transform(wine_points, crs = st_crs(fr_hex_sf))

joined_hex <- st_join(wine_points_hex, fr_hex_sf, left = FALSE)

sum_joined_hex <- joined_hex |> 
  group_by(ind) |> 
  count()

# join counts with departments

less_geo_hex <- sum_joined_hex |> 
  as_tibble() |> 
  select(-geometry)

hex_counts <- left_join(fr_hex_sf, less_geo_hex)

hex_counts |> 
  ggplot(aes(fill = n)) + 
  geom_sf(color = "grey90") +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  theme(text = element_text(family = "El Messiri",
                            color = "#440154FF"),
        plot.title = element_text(size = 30, face = "bold",
                                  hjust = .5),
        legend.title = element_text(margin = margin(b = 10)),
        plot.caption = element_text(hjust = .5,
                                    margin = margin(b = 10),
                                    size = 12,
                                    color = alpha("#440154FF", .5))) +
  labs(title = "Wineries in France",
       fill = "# of Wineries",
       caption = "Data from OpenStreetMap")

ggsave("tutorials/join_summarise_spatial_data/thumbnail.png",
       bg = "white")
