#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
# Title:   Compare D Line Trips
# Author:  Aden Yacobi
# Date:    2026/03/01
#
# Parses pre-CBTC and post-CBTC GTFS to compare SEPTA D1 and D2 trips.
#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

# load packages
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse, tidytransit, sf, lubridate, lwgeom, units, magrittr, 
       rstudioapi, writexl, magrittr)

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# CRS
GCS <- 4326 # WGS84
PCS <- 2272 # NAD83 / Pennsylvania South (ftUS)

# read path
OLD_PATH <- "./input/GTFS/gtfs_public_v202602010/google_bus.zip"
NEW_PATH <- "./input/GTFS/gtfs_public_v202602150/google_bus.zip"

# output path
TRIP_TIME_COMP_PATH <- "./intermediate/trip_time_comparisons.xlsx"
ROUTES_PATH <- "./intermediate/routes.gpkg"


# Read --------------------------------------------------------------------

# gtfs
old <- tidytransit::read_gtfs(OLD_PATH)
new <- tidytransit::read_gtfs(NEW_PATH)


# Feed Validation ---------------------------------------------------------

# function to validate feed
# Input:  gtfs - GTFS feed
# Output: Validation table
validate_feed <- function(gtfs) {
  validation_result <- attr(gtfs, "validation_result")
  print(validation_result %$% table(file_spec, file_provided_status))
  print(validation_result %$% table(field_spec, field_provided_status))
  print(validation_result %$% table(file_spec, field_spec, field_provided_status))
  
  # is there a time when file and field are both required but field does not 
  # exist?
  bad_validation <- validation_result %>%
    filter(file_spec == "Required",
           field_spec == "Required",
           !field_provided_status)
  if (nrow(bad_validation) != 0) {
    message <- paste0(deparse(substitute(gtfs)), " fails validation.")
    print(message)
  }
  return(validation_result)
}

# validate feeds
old_validation_result <- validate_feed(old)
new_validation_result <- validate_feed(new)


# Filter Routes -----------------------------------------------------------

# routes to filter
routes_to_filter <- c("D1", "D2")
bus_routes_to_filter <- c("D1 BUS", "D2 BUS")

# pruned GTFS feeds
d_old <- old %>%
  gtfstools::filter_by_route_id(routes_to_filter)
d_new <- new %>%
  gtfstools::filter_by_route_id(routes_to_filter)
d_new_bus <- new %>%
  gtfstools::filter_by_route_id(bus_routes_to_filter)

# remove full GTFS feeds for memory
rm(old, new)
gc()


# Questions ---------------------------------------------------------------

# 1. Is number of trips actually the same?
# 2. How much longer is average route, holding length and day of week constant?
# 3. Compare average times between stations.

# Notes:
# Service ID (old schedule/new schedule) of...
#   -(1)40/390240 is weekday
#   -(1)42/390242 is Saturday
#   -(1)43/390243 is Sunday
# New GTFS has double the number of trips
#   - This is because service ID 140, 142, 143 are for old trips
#      - The stop_times file exactly matches old
#   - Service ID 390240, 390242, 390243 are the actual new schedules
# Stop IDs are the same between new and old
# Arrival and departure times are the same for the D trolleys
# Direction ID of...
#   - 0 is southern termini
#   - 1 is 69th St Transit Center
# Trip Headsigns are consistent between new and old

# # look at extra trips
# trips_short <- d_new$trips %>%
#   filter(service_id %in% c(140, 142, 143))
# trips_long <- d_new$trips %>%
#   filter(!service_id %in% c(140, 142, 143))
# stop_times_short <- d_new$stop_times %>%
#   inner_join(trips_short %>% select(trip_id), by = c("trip_id"))
# stop_times_long <- d_new$stop_times %>%
#   inner_join(trips_long %>% select(trip_id), by = c("trip_id"))
# all.equal(d_old$stop_times %>% arrange(trip_id), 
#           stop_times_short %>% arrange(trip_id))

# # stop ids
# old_stops <- d_old$stop_times %>%
#   distinct(stop_id)
# new_stops <- d_new$stop_times %>%
#   distinct(stop_id)
# all.equal(old_stops, new_stops)
# all.equal(d_old$stops, d_new$stops)

# # are arrival and departure times the same? Yes.
# arr_vs_dep_old_test <- d_old$stop_times %>%
#   filter(arrival_time != departure_time)
# arr_vs_dep_new_test <- d_new$stop_times %>%
#   filter(arrival_time != departure_time)

# Filter new GTFS to new schedules
new_schedule_service_ids <- c("390240", "390242", "390243")
d_new <- d_new %>%
  gtfstools::filter_by_service_id(new_schedule_service_ids)


# 1. Is number of trips actually the same? --------------------------------
# Yes! Across all instances, too.

# Count number of trips and get duration by route, day, and start/end points
# Input:  gtfs - GTFS feed
# Output: tibble with count and duration of trips by route, day, and termini
count_and_time_trips <- function(gtfs) {
  # select tables
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  shapes <- gtfs$shapes
  stops <- gtfs$stops
  
  # count number of trips by route, day, and start/end points
  trip_counts <- stop_times %>%
    # get start and end trip info
    group_by(trip_id) %>%
    summarize(start = stop_id[stop_sequence == min(stop_sequence)],
              end = stop_id[stop_sequence == max(stop_sequence)],
              start_time = departure_time[stop_sequence == min(stop_sequence)],
              end_time = arrival_time[stop_sequence == max(stop_sequence)]) %>%
    ungroup() %>%
    mutate(across(c(start_time, end_time), ~ as.numeric(hms(.))),
           duration_sec = end_time - start_time) %>%
    # calculate number of trips and time of trips
    left_join(trips %>%
                select(trip_id, route_id, service_id, shape_id),
              by = "trip_id") %>%
    group_by(route_id, service_id, start, end, shape_id) %>%
    summarize(num_trips_per_day = n(),
              avg_duration_sec = mean(duration_sec)) %>%
    ungroup() %>%
    # bring in stop names
    left_join(stops %>%
                select(stop_id, start_name = stop_name),
              by = c("start" = "stop_id")) %>%
    left_join(stops %>%
                select(stop_id, end_name = stop_name),
              by = c("end" = "stop_id")) %>%
    # standardize service id as day of week
    mutate(day_of_week = case_when(str_detect(service_id, "40$") ~ "Weekday",
                                   str_detect(service_id, "42$") ~ "Saturday",
                                   str_detect(service_id, "43$") ~ "Sunday",
                                   TRUE ~ NA_character_)) %>%
    # format table
    select(day_of_week, service_id, route_id, start, start_name, end, end_name, 
           shape_id, num_trips_per_day, avg_duration_sec) %>%
    arrange(service_id, route_id, start_name, end_name)
  
  return(trip_counts)
}

# count number and time of trips
old_trip_counts <- count_and_time_trips(d_old)
new_trip_counts <- count_and_time_trips(d_new)
new_bus_trip_counts <- count_and_time_trips(d_new_bus)

# compare trip counts
all.equal(old_trip_counts %>% select(day_of_week, route_id, start, start_name,
                                     end, end_name, num_trips_per_day),
          new_trip_counts %>% select(day_of_week, route_id, start, start_name,
                                     end, end_name, num_trips_per_day))


# 2. How much longer is average route? ------------------------------------

# function to calculate trip length
# Input:  gtfs - GTFS feed
# Output: tibble with length of trips by each shape
get_trip_lengths <- function(gtfs) {
  shapes <- gtfs$shapes
  lines <- shapes %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = GCS) %>%
    st_transform(crs = PCS) %>%
    group_by(shape_id) %>%
    summarize(septa_km = shape_dist_traveled[shape_pt_sequence == max(shape_pt_sequence)],
              do_union=FALSE) %>%
    ungroup() %>%
    st_cast("LINESTRING") %>%
    mutate(calc_km = as.numeric(set_units(st_length(geometry), "km"))) %>%
    select(shape_id, septa_km, calc_km, geometry)
  return(lines)
}

# function to add in trip lengths and standardize units
# (use calculated trip lengths)
# Input:  trip_counts  - tibble with count and duration of trips by route, day,
#                        and termini
#         trip_lengths - tibble with length of trip by shape
# Output: tibble with count, duration, length, and speed of trips by route, day,
#         and termini
standardize_duration_distance <- function(trip_counts, trip_lengths) {
  standardized <- trip_counts %>%
    left_join(trip_lengths %>% 
                as_tibble() %>%
                select(shape_id, calc_km),
              by = "shape_id") %>%
    mutate(avg_duration_sec = set_units(avg_duration_sec, "s"),
           distance_mi = set_units(calc_km, "miles"),
           avg_duration_min = set_units(avg_duration_sec, "minutes"),
           avg_speed_mph = distance_mi / set_units(avg_duration_sec, 
                                                   "hours"))
  return(standardized)
}

# calculate trip lengths
old_trip_lengths <- get_trip_lengths(d_old)
new_trip_lengths <- get_trip_lengths(d_new)
new_bus_trip_lengths <- get_trip_lengths(d_new_bus)

# standardize units and calculate speed
old_trip_counts <- standardize_duration_distance(old_trip_counts, 
                                                 old_trip_lengths)
new_trip_counts <- standardize_duration_distance(new_trip_counts, 
                                                 new_trip_lengths)
new_bus_trip_counts <- standardize_duration_distance(new_bus_trip_counts, 
                                                     new_bus_trip_lengths)

# compare times per route, start/end, day
trip_time_comparison_route_termini_day <- new_trip_counts %>%
  # join
  select(day_of_week, route_id:end_name, num_trips_per_day, 
         distance_mi, avg_duration_min, avg_speed_mph) %>%
  inner_join(old_trip_counts %>%
               select(day_of_week, route_id:end_name, avg_duration_min, 
                      avg_speed_mph),
             by = join_by(day_of_week, route_id, start, start_name, end, 
                          end_name),
             suffix = c("_new", "_old")) %>%
  # calculate stats
  mutate(num_trips_per_week = if_else(day_of_week == "Weekday", 
                                      num_trips_per_day * 5, 
                                      num_trips_per_day),
         avg_duration_min_diff = avg_duration_min_new - avg_duration_min_old,
         avg_duration_min_diff_perc = avg_duration_min_diff / avg_duration_min_old,
         avg_speed_mph_diff = avg_speed_mph_new - avg_speed_mph_old,
         avg_speed_mph_diff_perc = avg_speed_mph_diff / avg_speed_mph_old,
         across(c(distance_mi, avg_duration_min_new:avg_speed_mph_diff_perc), 
                as.numeric)) %>%
  # format
  select(day_of_week, route_id:end_name, num_trips_per_day, num_trips_per_week,
         distance_mi,
         avg_duration_min_new, avg_duration_min_old, 
         avg_duration_min_diff, avg_duration_min_diff_perc,
         avg_speed_mph_new, avg_speed_mph_old,
         avg_speed_mph_diff, avg_speed_mph_diff_perc)

# compare times per route and start/end
trip_time_comparison_route_termini <- new_trip_counts %>%
  # join
  select(day_of_week, route_id:end_name, num_trips_per_day, 
         distance_mi, avg_duration_min, avg_speed_mph) %>%
  inner_join(old_trip_counts %>%
               select(day_of_week, route_id:end_name, avg_duration_min, 
                      avg_speed_mph),
             by = join_by(day_of_week, route_id, start, start_name, end, 
                          end_name),
             suffix = c("_new", "_old")) %>%
  # aggregate by summing up weekly totals
  group_by(route_id, start, start_name, end, end_name) %>%
  mutate(num_trips_per_week = if_else(day_of_week == "Weekday", 
                                      num_trips_per_day * 5, 
                                      num_trips_per_day),
         total_distance_mi = distance_mi * num_trips_per_week,
         total_duration_min_new = avg_duration_min_new * num_trips_per_week,
         total_duration_min_old = avg_duration_min_old * num_trips_per_week) %>%
  summarize(distance_mi = first(distance_mi),
            across(num_trips_per_week:total_duration_min_old, sum)) %>%
  ungroup() %>%
  # calculate stats
  mutate(avg_duration_min_new = total_duration_min_new / num_trips_per_week,
         avg_duration_min_old = total_duration_min_old / num_trips_per_week,
         avg_speed_mph_new = total_distance_mi / set_units(total_duration_min_new, 
                                                           "hours"),
         avg_speed_mph_old = total_distance_mi / set_units(total_duration_min_old, 
                                                           "hours"),
         avg_duration_min_diff = avg_duration_min_new - avg_duration_min_old,
         avg_duration_min_diff_perc = avg_duration_min_diff / avg_duration_min_old,
         avg_speed_mph_diff = avg_speed_mph_new - avg_speed_mph_old,
         avg_speed_mph_diff_perc = avg_speed_mph_diff / avg_speed_mph_old,
         across(!where(is.character), as.numeric)) %>%
  # format
  select(route_id:end_name, num_trips_per_week,
         distance_mi,
         avg_duration_min_new, avg_duration_min_old, 
         avg_duration_min_diff, avg_duration_min_diff_perc,
         avg_speed_mph_new, avg_speed_mph_old,
         avg_speed_mph_diff, avg_speed_mph_diff_perc) %>%
  arrange(route_id, start_name, end_name)

# compare new weekend times with bus weekend times
# compare times per route and start/end
trip_time_comparison_new_vs_bus_weekend_route_termini <- new_trip_counts %>%
  filter(day_of_week != "Weekday") %>%
  # join
  select(day_of_week, route_id:end_name, num_trips_per_day, 
         distance_mi, avg_duration_min, avg_speed_mph) %>%
  inner_join(new_bus_trip_counts %>%
               select(day_of_week, route_id:end_name, distance_mi,
                      avg_duration_min, avg_speed_mph) %>%
               # standardize D1 BUS and D2 BUS with D1 and D2
               mutate(route_id = trimws(str_remove(route_id, "BUS")),
                      end = if_else(end == "1948", "1947", end),
                      across(c(start, end), ~ if_else(. == "1962", "20431", .)),
                      across(c(start_name, end_name), ~ if_else(. == "Chester Pk & Brainerd Blvd - FS", 
                                                                "Chester Pike/Sharon Hill",
                                                                .))),
             by = join_by(day_of_week, route_id, start, start_name, end, 
                          end_name),
             suffix = c("_trolley", "_bus")) %>%
  # aggregate by summing up weekly totals
  group_by(route_id, start, start_name, end, end_name) %>%
  mutate(num_trips_per_week = if_else(day_of_week == "Weekday", 
                                      num_trips_per_day * 5, 
                                      num_trips_per_day),
         total_distance_mi_trolley = distance_mi_trolley * num_trips_per_week,
         total_distance_mi_bus = distance_mi_bus * num_trips_per_week,
         total_duration_min_trolley = avg_duration_min_trolley * num_trips_per_week,
         total_duration_min_bus = avg_duration_min_bus * num_trips_per_week) %>%
  summarize(across(c(distance_mi_trolley, distance_mi_bus), first),
            across(num_trips_per_week:total_duration_min_bus, sum)) %>%
  ungroup() %>%
  # calculate stats
  mutate(avg_duration_min_trolley = total_duration_min_trolley / num_trips_per_week,
         avg_duration_min_bus = total_duration_min_bus / num_trips_per_week,
         avg_speed_mph_trolley = total_distance_mi_trolley / set_units(total_duration_min_trolley, 
                                                               "hours"),
         avg_speed_mph_bus = total_distance_mi_bus / set_units(total_duration_min_bus, 
                                                           "hours"),
         avg_duration_min_diff = avg_duration_min_trolley - avg_duration_min_bus,
         avg_duration_min_diff_perc = avg_duration_min_diff / avg_duration_min_bus,
         avg_speed_mph_diff = avg_speed_mph_trolley - avg_speed_mph_bus,
         avg_speed_mph_diff_perc = avg_speed_mph_diff / avg_speed_mph_bus,
         across(!where(is.character), as.numeric)) %>%
  # format
  select(route_id:end_name, num_trips_per_week,
         distance_mi_trolley, distance_mi_bus,
         avg_duration_min_trolley, avg_duration_min_bus, 
         avg_duration_min_diff, avg_duration_min_diff_perc,
         avg_speed_mph_trolley, avg_speed_mph_bus,
         avg_speed_mph_diff, avg_speed_mph_diff_perc) %>%
  arrange(route_id, start_name, end_name)


# Map ---------------------------------------------------------------------

# TODO delete section and do in ArcGIS
# DELCO_LAND_USE_PATH <- "./input/DVRPC/delco_land_use_2023/Delaware.shp"
# delco_land_use <- st_read(DELCO_LAND_USE_PATH)
# delco_land_use <- delco_land_use %>%
#   st_transform(crs = PCS)
# delco_land_use2 <- delco_land_use %>%
#   mutate(land_use = if_else(lu23catn %in% c("Transportation", "Water", "Wooded"), 
#                             lu23catn,
#                             "Other")) %>%
#   group_by(land_use) %>%
#   summarize(do_union = FALSE) %>%
#   ungroup()
# 
# tmap_mode("view")
# # tm_shape(delco_land_use2) +
# #   tm_polygons(fill = "land_use", lwd = 0) +
#   tm_shape(new_bus_trip_lengths) +
#   tm_lines(col = "black", lwd = 2) +
#   tm_shape(new_trip_lengths) +
#   tm_lines(col = "#E5417B", lwd = 2)
  

# Write -------------------------------------------------------------------

# write to excel
excel_list <- list("route_shape_direction_day_comp" = trip_time_comparison_route_termini_day,
                   "route_shape_direction_comp" = trip_time_comparison_route_termini,
                   "route_shape_direction_bus_comp" = trip_time_comparison_new_vs_bus_weekend_route_termini)
write_xlsx(excel_list, TRIP_TIME_COMP_PATH)

# write to geopackage
st_write(new_trip_lengths, ROUTES_PATH, layer = "trolleys")
st_write(new_bus_trip_lengths, ROUTES_PATH, layer = "buses")
