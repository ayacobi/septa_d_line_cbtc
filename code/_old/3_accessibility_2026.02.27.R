## Accessibility


# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

options(java.parameters = "-Xmx4G") # increase memory available to Java

# load packages
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse, rstudioapi, r5r, sf, data.table, ggplot2, interp, h3jsr)

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# TODO merge with regional rail
# TODO maybe just use new GTFS but filtered for before/after
OLD_PATH <- "C:/Users/aden8/Documents/Urban Planning Projects/SEPTA D Lines/input/Network Dataset" #"./input/Network Dataset"
JOBS_PATH <- "./input/OnTheMap/philly_metro_all_jobs_2023/points_2023.shp"
TRACTS_PATH <- "./input/Census TIGER Line Shapefiles/tl_2025_42_tract/tl_2025_42_tract.shp"
BGS_PATH <- "./input/Census TIGER Line Shapefiles/tl_2025_42_bg/tl_2025_42_bg.shp"

GCS <- 4326


# Read --------------------------------------------------------------------


r5r_network <- r5r::build_network(OLD_PATH)

# data_path <- system.file("extdata/poa", package = "r5r")
# r5r_network <- build_network(data_path)

tracts <- st_read(TRACTS_PATH)
tracts <- tracts %>%
  st_as_sf(crs = GCS) %>%
  filter(COUNTYFP %in% c(# "017", # Bucks
                         # "029", # Chester
                         "045", # Delaware
                         # "091", # Montgomery
                         "101"  # Philadelphia
                         ))
bgs <- st_read(BGS_PATH)
bgs <- bgs %>%
  st_as_sf(crs = GCS) %>%
  filter(COUNTYFP %in% c(# "017", # Bucks
    # "029", # Chester
    "045", # Delaware
    # "091", # Montgomery
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
  mutate(bg_id = substr(id, 1, 12)) %>%
  group_by(bg_id) %>%
  summarize(num_jobs = sum(c000)) %>%
  ungroup() %>%
  inner_join(bgs %>% select(GEOID, STATEFP, geometry),
             by = c("bg_id" = "GEOID"))

# starting point
starting_point <- tibble(id = 0, lon = -75.39106288361312, lat = 39.9181505605741)

# isochrone intervals
time_intervals <- c(15, 30, 45, 60)

# routing inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 30      # in minutes
max_trip_duration <- max(time_intervals)  # in minutes
time_window <- 120       # in minutes
departure_datetime <- as.POSIXct("10-02-2026 7:00:00",
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

ggplot() +
  geom_sf(data = tracts) +
  geom_sf(data = iso1, aes(fill=factor(isochrone)), color = NA, alpha = .7) +
  geom_sf(data = rail_routes, aes(fill = factor(mode))) +
  # geom_sf(data = main_roads, color = "gray55", size=0.01, alpha = 0.2) +
  geom_point(data = starting_point, aes(x=lon, y=lat, color='Central bus\nstation')) +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  # scale_fill_manual(values = rev(colors) ) +
  scale_color_manual(values=c('Central bus\nstation'='black')) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())


# More Robust Alternative -------------------------------------------------

# calculate travel time matrix
ttm <- travel_time_matrix(r5r_network,
                          origins = starting_point,
                          destinations = points,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_time = max_walk_time,
                          max_trip_duration = max_trip_duration,
                          time_window = time_window,
                          progress = FALSE)

head(ttm)

# add coordinates of destinations to travel time matrix
ttm[points, on=c('to_id' ='id'), `:=`(lon = i.lon, lat = i.lat)]

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
ggplot(travel_times.interp) +
  geom_sf(data = main_roads, color = "gray55", size=0.01, alpha = 0.7) +
  geom_contour_filled(aes(x=x, y=y, z=travel_time), alpha=.7) +
  geom_point(aes(x=lon, y=lat, color='Central bus\nstation'),
             data=starting_point) +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_fill_manual(values = rev(colors) ) +
  scale_color_manual(values=c('Central bus\nstation'='black')) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())


# Clean up after usage ----------------------------------------------------

rJava::.jgc(R.gc = TRUE)


