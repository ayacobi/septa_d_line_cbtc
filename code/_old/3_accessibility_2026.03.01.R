#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
# Title:   Accessibility
# Author:  Aden Yacobi
# Date:    2026/03/01
#
# Calculate accessibility to jobs and map isochrones.
#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

options(java.parameters = "-Xmx2G") # increase memory available to Java

# load packages
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse, rstudioapi, r5r, sf, data.table, ggplot2, interp, h3jsr, 
       tmap)

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# TODO maybe just use new GTFS but filtered for before/after
OLD_PATH <- "C:/Users/aden8/Documents/Urban Planning Projects/SEPTA D Lines/input/Network Dataset_Pre-CBTC" # TODO figure out how to make dynamic
JOBS_PATH <- "./input/OnTheMap/philly_metro_all_jobs_2023/points_2023.shp"
TRACTS_PATH <- "./input/Census TIGER Line Shapefiles/tl_2025_42_tract/tl_2025_42_tract.shp"
# BGS_PATH <- "./input/Census TIGER Line Shapefiles/tl_2025_42_bg/tl_2025_42_bg.shp" TODO delete
BLOCKS_PATH <- "./input/Census TIGER Line Shapefiles/tl_2025_42_tabblock20/tl_2025_42_tabblock20.shp"

# CRS
GCS <- 4326 # WGS84
PCS <- 2272 # NAD83 / Pennsylvania South (ftUS)


# Read --------------------------------------------------------------------

# build network - no elevation
r5r_network <- r5r::build_network(OLD_PATH)

# jobs
jobs <- st_read(JOBS_PATH) %>%
  st_transform(crs = GCS)

# blocks
blocks <- st_read(BLOCKS_PATH) %>%
  st_transform(crs = GCS)

counties <- c(
  "017", # Bucks
  "029", # Chester
  "045", # Delaware
  "091", # Montgomery
  "101")  # Philadelphia
test <- blocks %>%
  filter(COUNTYFP20 %in% counties) %>%
  left_join(jobs %>%
              st_drop_geometry() %>%
              mutate(COUNTYFP = substr(id, 3, 5)) %>%
              filter(COUNTYFP %in% counties) %>%
              select(id, num_jobs = c000),
            by = c("GEOID20" = "id"))
  test2 <- test %>%
    filter(is.na(num_jobs))

# # census tracts
# tracts <- st_read(TRACTS_PATH)
# tracts <- tracts %>%
#   st_as_sf(crs = GCS) %>%
#   # st_transform(crs = PCS) %>%
#   filter(COUNTYFP %in% c(# "017", # Bucks
#                          # "029", # Chester
#                          "045", # Delaware
#                          # "091", # Montgomery
#                          "101"  # Philadelphia
#                          ))
# bgs <- st_read(BGS_PATH)
# bgs <- bgs %>%
#   st_as_sf(crs = GCS) %>%
#   # st_transform(crs = PCS) %>%
#   filter(COUNTYFP %in% c(
#     "017", # Bucks
#     "029", # Chester
#     "045", # Delaware
#     "091", # Montgomery
#     "101"  # Philadelphia
#   ))


# Access to Jobs ----------------------------------------------------------

# TODO delete
# points_bg <- jobs %>%
#   as_tibble() %>%
#   mutate(id = substr(id, 1, 12)) %>%
#   group_by(id) %>%
#   summarize(num_jobs = sum(c000)) %>%
#   ungroup() %>%
#   inner_join(bgs %>% select(GEOID, STATEFP, geometry),
#              by = c("id" = "GEOID")) %>%
#   st_as_sf() %>%
#   st_transform(crs = GCS) %>%
#   st_centroid() %>%
#   cbind(st_coordinates(.)) %>%
#   rename(lon = X, lat = Y) %>%
#   st_drop_geometry()

# extract coordinates (at 45 min, this is bounded to PA)
# jobs_lat_lon <- jobs %>%
#   select(id, num_jobs = c000, geometry) %>%
#   cbind(st_coordinates(.)) %>%
#   rename(lon = X, lat = Y) %>%
#   st_drop_geometry()
  blocks_centroid <- blocks %>%
    filter(COUNTYFP20 %in% counties) %>%
    select(GEOID20, geometry) %>%
    st_centroid()
 blocks_lat_lon <- blocks_centroid %>%
    cbind(st_coordinates(.)) %>%
    rename(id = GEOID20, lon = X, lat = Y) %>%
    st_drop_geometry()

# starting point (from Google Maps)
origin_ids <- c("Orange St/Media", "Sharon Hill", 
                "Drexel Hill Junction", "69th St Transit center")
origin_lons <- c(-75.3938792551593, -75.27806969923209, 
                 -75.29283481754955, -75.2596839468921)
origin_lats <- c(39.918535820576224, 39.90649575288983, 
                 39.94699948308914, 39.96216420913261)
starting_point <- tibble(id = origin_ids, 
                         lon = origin_lons, 
                         lat = origin_lats)

# isochrone intervals
time_intervals <- c(15, 30, 45)

# routing inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 30      # in minutes
max_trip_duration <- max(time_intervals)  # in minutes
time_window <- 120       # in minutes
percentiles <- c(50)
departure_datetime <- as.POSIXct("04-02-2026 8:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# calculate travel time matrix
ttm <- travel_time_matrix(r5r_network,
                          origins = starting_point,
                          destinations = blocks_lat_lon, # jobs_lat_lon,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_time = max_walk_time,
                          max_trip_duration = max_trip_duration,
                          time_window = time_window,
                          percentiles = percentiles,
                          progress = FALSE)

ttm_sf <- ttm %>%
  # pivot_wider(names_from = from_id,
  #             values_from = travel_time_p50) %>%
  # mutate(origin = case_when(!is.na(`Orange St/Media`) & is.na(`Sharon Hill`)))
  # inner_join(jobs_lat_lon, by = c("to_id" = "id")) %>%
  inner_join(blocks_lat_lon, by = c("to_id" = "id")) %>%
  left_join(jobs %>% st_drop_geometry() %>% select(id, num_jobs = c000), by = c("to_id" = "id")) %>%
  mutate(num_jobs = if_else(is.na(num_jobs), 0, num_jobs)) %>%
  # group_by(to_id) %>%
  # summarize(,
  #           across(c(num_jobs, lon, lat) = first(.)))
  st_as_sf(coords = c("lon", "lat"), crs = GCS)
tmap_mode("plot")
tm_shape(ttm_sf # %>% filter(from_id == "Orange St/Media")
         ) +
  tm_dots(fill = "from_id",
          # size = "num_jobs",
          # size.scale = tm_scale_continuous_log10()
          ) +
  # tm_dots(fill = "travel_time_p50",
  #         fill.scale = tm_scale_intervals(style = "fixed",
  #                                         breaks = c(0, 15, 30, 45),
  #                                         values = "viridis")) +
  tm_basemap("Esri.WorldTopoMap")


# --- Spatial interpolation of travel times --- #

# add coordinates of destinations to travel time matrix
ttm[blocks_lat_lon, on=c('to_id' ='id'), `:=`(lon = i.lon, lat = i.lat)]

# interpolate estimates to get spatially smooth result
ttm_sub <- ttm %>% filter(from_id == "Orange St/Media")
travel_times.interp <- with(na.omit(ttm_sub), interp(lon, lat, travel_time_p50)) |>
  with(cbind(travel_time=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) |>
  as.data.frame() |> na.omit()

# find isochrone's bounding box to crop the map below
bb_x <- c(min(travel_times.interp$x), max(travel_times.interp$x))
bb_y <- c(min(travel_times.interp$y), max(travel_times.interp$y))

# plot
basemaps::set_defaults(map_service = "esri", map_type = "world_topo_map")
ggplot(travel_times.interp #%>%
         # mutate(t = case_when(travel_time <= 15 ~ "15",
         #                                travel_time <= 30 ~ "30",
         #                                TRUE ~ "45"))
       ) +
  basemap_gglayer(ext = st_bbox(ttm_sf %>% st_transform(3857))) +
  # geom_sf(data = main_roads, color = "gray55", size=0.01, alpha = 0.7) +
  geom_contour_filled(aes(x=x, y=y, z=travel_time), alpha=1, breaks=c(0, 15, 30, 45)) +
  scale_fill_viridis_d() +
  new_scale_fill() +
  geom_point(aes(x=lon, y=lat, color='black'),
             data=starting_point %>% filter(id == "Orange St/Media")) +
  new_scale_color() +
  geom_point(aes(x=lon, y=lat, color=travel_time_p50),
             data=ttm_sub %>% mutate(travel_time_p50 = case_when(travel_time_p50 <= 15 ~ "A",
                                                                 travel_time_p50 <= 30 ~ "B",
                                                                 TRUE ~ "C"))) +
  scale_color_viridis_d() +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  # scale_fill_manual(values = rev(colors) ) +
  # scale_color_manual(values=c('Origin'='black')) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())

# Map Isochrones ----------------------------------------------------------

# calculate travel time matrix
iso1 <- r5r::isochrone(r5r_network,
                       origins = starting_point,
                       mode = mode,
                       cutoffs = time_intervals,
                       sample_size = 1,
                       departure_datetime = departure_datetime,
                       max_walk_time = max_walk_time,
                       max_trip_duration = max_trip_duration,
                       time_window = time_window,
                       progress = FALSE)

# extract OSM network
transit_net <- transit_network_to_sf(r5r_network)
rail_routes <- subset(transit_net$routes, mode %like% 'TRAM|SUBWAY|RAIL')

x <- "69th St Transit center"
tm_shape(iso1 %>% filter(id == x) %>% mutate(isochrone = isochrone - 1)) +
  tm_polygons(fill = "isochrone", fill_alpha = 0, lwd = 5,
              # fill.scale = tm_scale_ordinal(n.max = 3,
              #                               values = "viridis")
              fill.scale = tm_scale_intervals(style = "fixed",
                                              breaks = c(0, 15, 30, 45, 60),
                                              values = "viridis")
              ) + 
  tm_shape(ttm_sf %>% filter(from_id == x)) +
  # tm_dots(fill = "from_id",
  #         size = "num_jobs",
  #         size.scale = tm_scale_continuous_log10()
  #         )
  tm_dots(fill = "travel_time_p50",
          fill.scale = tm_scale_intervals(style = "fixed",
                                          breaks = c(0, 15, 30, 45, 60),
                                          values = "viridis")) +
  tm_basemap("Esri.WorldTopoMap")

colors <- c('#ffcb69','#ffa600','#ff7c43','#f95d6a')

tmap_mode("plot")
tm_shape(bgs) +
  tm_polygons(fill = "COUNTYFP") +
  tm_shape(iso1) +
  tm_polygons(fill = "isochrone", col = NA, fill_alpha = 0.7) +
  tm_shape(rail_routes) +
  tm_lines(col = "mode") +
  tm_shape(starting_point) +
  tm_dots(fill = "black")


ggplot() +
  geom_sf(data = tracts) +
  geom_sf(data = iso1, aes(fill=factor(isochrone)), color = NA, alpha = .7) +
  geom_sf(data = rail_routes, aes(fill = factor(mode))) +
  # geom_sf(data = main_roads, color = "gray55", size=0.01, alpha = 0.2) +
  geom_sf(data = starting_point, aes(fill='Starting Point')) +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  # scale_fill_manual(values = rev(colors) ) +
  scale_color_manual(values=c('Central bus\nstation'='black')) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())





# Clean up after usage ----------------------------------------------------

r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)

output <- travel_times.interp %>%
  st_as_sf(coords = c("x", "y"), crs = GCS)
st_write(output, "./intermediate/test/test.shp")

