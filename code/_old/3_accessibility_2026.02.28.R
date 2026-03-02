## Accessibility


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
       tmap, terra, gstat, stars)

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# TODO merge with regional rail
# TODO maybe just use new GTFS but filtered for before/after
OLD_PATH <- "C:/Users/aden8/Documents/Urban Planning Projects/SEPTA D Lines/input/Network Dataset_Pre-CBTC" # TODO figure out how to make dynamic
JOBS_PATH <- "./input/OnTheMap/philly_metro_all_jobs_2023/points_2023.shp"
TRACTS_PATH <- "./input/Census TIGER Line Shapefiles/tl_2025_42_tract/tl_2025_42_tract.shp"
BGS_PATH <- "./input/Census TIGER Line Shapefiles/tl_2025_42_bg/tl_2025_42_bg.shp"

# CRS
GCS <- 4326 # WGS84
PCS <- 2272 # NAD83 / Pennsylvania South (ftUS)


# Read --------------------------------------------------------------------


r5r_network <- r5r::build_network(OLD_PATH)

# data_path <- system.file("extdata/poa", package = "r5r")
# r5r_network <- build_network(data_path)

tracts <- st_read(TRACTS_PATH)
tracts <- tracts %>%
  st_as_sf(crs = GCS) %>%
  # st_transform(crs = PCS) %>%
  filter(COUNTYFP %in% c(# "017", # Bucks
                         # "029", # Chester
                         "045", # Delaware
                         # "091", # Montgomery
                         "101"  # Philadelphia
                         ))
bgs <- st_read(BGS_PATH)
bgs <- bgs %>%
  st_as_sf(crs = GCS) %>%
  # st_transform(crs = PCS) %>%
  filter(COUNTYFP %in% c(
    "017", # Bucks
    "029", # Chester
    "045", # Delaware
    "091", # Montgomery
    "101"  # Philadelphia
  ))

# Quick and Easy Approach -------------------------------------------------

# read all points in the city
points_sf <- st_read(JOBS_PATH)
points <- points_sf %>%
  st_transform(crs = GCS) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  as_tibble() %>%
  select(id, c000, lon, lat)

points_bg <- points %>%
  as_tibble() %>%
  mutate(id = substr(id, 1, 12)) %>%
  group_by(id) %>%
  summarize(num_jobs = sum(c000)) %>%
  ungroup() %>%
  inner_join(bgs %>% select(GEOID, STATEFP, geometry),
             by = c("id" = "GEOID")) %>%
  st_as_sf() %>%
  st_transform(crs = GCS) %>%
  st_centroid()

# starting point
starting_point <- tibble(id = 0, 
                         lon = -75.3938792551593, lat = 39.918535820576224) %>%
  st_as_sf(coords = c("lon", "lat"), crs = GCS)
# isochrone intervals
time_intervals <- c(15, 30, 45, 60)

# routing inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 30      # in minutes
max_trip_duration <- max(time_intervals)  # in minutes
time_window <- 120       # in minutes
departure_datetime <- as.POSIXct("04-02-2026 7:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

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

colors <- c('#ffcb69','#ffa600','#ff7c43','#f95d6a')

library(tmap)
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


# More Robust Alternative -------------------------------------------------

points_bg <- points %>%
  as_tibble() %>%
  mutate(id = substr(id, 1, 12)) %>%
  group_by(id) %>%
  summarize(num_jobs = sum(c000)) %>%
  ungroup() %>%
  inner_join(bgs %>% select(GEOID, STATEFP, geometry),
             by = c("id" = "GEOID")) %>%
  st_as_sf() %>%
  st_transform(crs = GCS) %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  rename(lon = X, lat = Y) %>%
  st_drop_geometry()

time_window <- 1

# starting point
starting_point <- tibble(id = "Orange St/Media", 
                         lon = -75.3938792551593, lat = 39.918535820576224)

# calculate travel time matrix
ttm <- travel_time_matrix(r5r_network,
                          origins = starting_point,
                          destinations = points_bg,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_time = max_walk_time,
                          max_trip_duration = max_trip_duration,
                          time_window = time_window,
                          progress = FALSE)

head(ttm)

# add coordinates of destinations to travel time matrix
ttm[points_bg, on=c('to_id' ='id'), `:=`(lon = lon, lat = lat)]

# interpolate estimates to get spatially smooth result
travel_times.interp <- with(na.omit(ttm), interp(lon, lat, travel_time_p50)) |>
  with(cbind(travel_time=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) |>
  as.data.frame() |> na.omit()

# find isochrone's bounding box to crop the map below
bb_x <- c(min(travel_times.interp$x), max(travel_times.interp$x))
bb_y <- c(min(travel_times.interp$y), max(travel_times.interp$y))

# plot
tm_shape(tracts) +
  tm_borders() +
  tm_shape(travel_times.interp %>% st_as_sf(coords = c("x", "y"), crs = GCS)) +
  # tm_shape(ttm %>% rename(travel_time = travel_time_p50) %>% st_as_sf(coords = c("lon", "lat"), crs = GCS)) +
  tm_dots(fill = "travel_time",
          fill.scale = tm_scale_intervals(style = "fixed",
                                           breaks = c(0, 15, 30, 45, 60),
                                           values = "viridis")) +
  tm_shape(starting_point %>% st_as_sf(coords = c("lon", "lat"), crs = GCS)) +
  tm_dots() +
  tm_shape(rail_routes) +
  tm_lines(col = "blue")

ggplot(travel_times.interp) +
  geom_sf(data = tracts) +
  geom_sf(data = rail_routes, color = "gray55", size=0.01, alpha = 0.7) +
  geom_contour_filled(aes(x=x, y=y, z=travel_time), alpha=.7) +
  geom_point(aes(x=lon, y=lat, color=id),
             data=starting_point) +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  # scale_fill_manual(values = rev(colors) ) +
  # scale_color_manual(values=c('Central bus\nstation'='black')) +
  # scale_x_continuous(expand=c(0,0)) +
  # scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())

destination_ids <- c("421019800032", "420454045001", "420454045003", "420454075022", "420454075012")
destinations <- points_bg %>%
  filter(id %in% destination_ids)
detailed_itineraries2 <- detailed_itineraries(r5r_network,
                                             origins = starting_point,
                                             destinations = destinations,
                                             mode = mode,
                                             departure_datetime = departure_datetime,
                                             max_walk_time = max_walk_time,
                                             max_trip_duration = max_trip_duration,
                                             time_window = time_window,
                                             progress = FALSE
                                             )


# Clean up after usage ----------------------------------------------------

r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)

output <- travel_times.interp %>%
  st_as_sf(coords = c("x", "y"), crs = GCS)
st_write(output, "./intermediate/test/test.shp")

