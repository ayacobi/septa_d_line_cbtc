# https://cran.r-project.org/web/packages/r5r/vignettes/isochrones.html


# Build Network -----------------------------------------------------------


options(java.parameters = "-Xmx2G")

library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(interp)

# system.file returns the directory with example data inside the r5r package
# set data path to directory containing your own data if not running this example
data_path <- system.file("extdata/poa", package = "r5r")

r5r_network <- build_network(data_path)


# Quick and Easy Approach -------------------------------------------------

# read all points in the city
points <- fread(file.path(data_path, "poa_hexgrid.csv"))

# subset point with the geolocation of the central bus station
central_bus_stn <- points[291,]

# isochrone intervals
time_intervals <- seq(0, 100, 10)

# routing inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 30      # in minutes
max_trip_duration <- 100 # in minutes
time_window <- 120       # in minutes
departure_datetime <- as.POSIXct("13-05-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# calculate travel time matrix
iso1 <- r5r::isochrone(r5r_network,
                       origins = central_bus_stn,
                       mode = mode,
                       cutoffs = time_intervals,
                       sample_size = 1,
                       departure_datetime = departure_datetime,
                       max_walk_time = max_walk_time,
                       max_trip_duration = max_trip_duration,
                       time_window = time_window,
                       progress = FALSE)

iso2 <- r5r::isochrone(r5r_network,
                       origins = central_bus_stn,
                       mode = mode,
                       cutoffs = time_intervals,
                       sample_size = 0.8,
                       departure_datetime = departure_datetime,
                       max_walk_time = max_walk_time,
                       max_trip_duration = max_trip_duration,
                       time_window = time_window,
                       progress = FALSE)

# extract OSM network
street_net <- street_network_to_sf(r5r_network)
main_roads <- subset(street_net$edges, street_class %like% 'PRIMARY|SECONDARY')

colors <- c('#ffe0a5','#ffcb69','#ffa600','#ff7c43','#f95d6a',
            '#d45087','#a05195','#665191','#2f4b7c','#003f5c')

# iso1
ggplot() +
  geom_sf(data = iso1, aes(fill=factor(isochrone)), color = NA, alpha = .7) +
  geom_sf(data = main_roads, color = "gray55", size=0.01, alpha = 0.2) +
  geom_point(data = central_bus_stn, aes(x=lon, y=lat, color='Central bus\nstation')) +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_fill_manual(values = rev(colors) ) +
  scale_color_manual(values=c('Central bus\nstation'='black')) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())

# iso2
ggplot() +
  geom_sf(data = iso2, aes(fill=factor(isochrone)), color = NA, alpha = .7) +
  geom_sf(data = main_roads, color = "gray55", size=0.01, alpha = 0.2) +
  geom_point(data = central_bus_stn, aes(x=lon, y=lat, color='Central bus\nstation')) +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_fill_manual(values = rev(colors) ) +
  scale_color_manual(values=c('Central bus\nstation'='black')) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())


# Travel Time Matrix ------------------------------------------------------

# calculate travel time matrix
ttm <- travel_time_matrix(r5r_network,
                          origins = central_bus_stn,
                          destinations = points,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_time = max_walk_time,
                          max_trip_duration = max_trip_duration,
                          time_window = time_window,
                          progress = FALSE)

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
             data=central_bus_stn) +
  # scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_fill_manual(values = rev(colors) ) +
  scale_color_manual(values=c('Central bus\nstation'='black')) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(fill = "Travel time\n(in minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())


# Wind Down ---------------------------------------------------------------


r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)