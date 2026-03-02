library(rstudioapi)
library(tidytransit)
library(dplyr)
library(tidyr)
library(sf)
library(sfnetworks)
library(lubridate)
library(stringr)
library(lwgeom)
library(units)
library(scales)
library(magrittr)
library(tmap)
library(tmaptools)
library(mapview)
library(maptiles)
library(paletteer)
library(ggplot2)
# library(plotly)
library(readxl)
library(writexl)

rm(list = ls())
gc()

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# time range
START_TIME = "12:00:00" # "07:30:00" # "12:00:00"
END_TIME = "13:00:00" # "08:30:00" # "13:00:00"

# CRS
GCS <- 4326 # WGS84
PCS <- 2272 # NAD83 / Pennsylvania South (ftUS)

# read path
OLD_PATH <- "./input/GTFS/gtfs_public_v202602010/google_bus.zip"
NEW_PATH <- "./input/GTFS/gtfs_public_v202602150/google_bus.zip"
GC_PATH <- "./input/Grade Crossings/Grade Crossings.xlsx"

# output path
# COMPARISON_GPKG_PATH <- "./intermediate/comparisons.gpkg" # TODO delete
TRIP_TIME_COMP_PATH <- "./intermediate/trip_time_comparisons.xlsx"
STOP_SEG_COMP_PATH <- "./intermediate/stop_segment_comparisons.gpkg"


# Read --------------------------------------------------------------------

# gtfs
old <- tidytransit::read_gtfs(OLD_PATH)
new <- tidytransit::read_gtfs(NEW_PATH)

# grade crossings
gc <- read_xlsx(GC_PATH, "grade_crossings", guess_max = Inf)
sr <- read_xlsx(GC_PATH, "street_running", guess_max = Inf)


# Feed Validation ---------------------------------------------------------

# function to validate feed
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
routes_to_filter <- c("D1", "D2") # D1 Bus, D2 Bus

# pruned GTFS feeds
d_old <- old %>%
  gtfstools::filter_by_route_id(routes_to_filter)
d_new <- new %>%
  gtfstools::filter_by_route_id(routes_to_filter)

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

# count number of trips and get duration by route, day, and start/end points
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

# compare trip counts
all.equal(old_trip_counts %>% select(day_of_week, route_id, start, start_name,
                                     end, end_name, num_trips_per_day),
          new_trip_counts %>% select(day_of_week, route_id, start, start_name,
                                     end, end_name, num_trips_per_day))


# 2. How much longer is average route? ------------------------------------

# calculate trip length
get_trip_lengths <- function(gtfs) {
  shapes <- gtfs$shapes
  lines <- shapes %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = GCS) %>%
    group_by(shape_id) %>%
    summarize(septa_km = shape_dist_traveled[shape_pt_sequence == max(shape_pt_sequence)],
              do_union=FALSE) %>%
    ungroup() %>%
    st_cast("LINESTRING") %>%
    mutate(calc_km = as.numeric(set_units(st_length(geometry), "km"))) %>%
    select(shape_id, septa_km, calc_km, geometry)
  return(lines)
}

# add in trip lengths and standardize units (use calculated trip lengths)
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

# standardize units and calculate speed
old_trip_counts <- standardize_duration_distance(old_trip_counts, 
                                                 old_trip_lengths)
new_trip_counts <- standardize_duration_distance(new_trip_counts, 
                                                 new_trip_lengths)

# compare times per route, day, and start/end
trip_time_comparison <- new_trip_counts %>%
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
trip_time_comparison2 <- new_trip_counts %>%
  # join
  select(day_of_week, route_id:end_name, num_trips_per_day, 
         distance_mi, avg_duration_min, avg_speed_mph) %>%
  inner_join(old_trip_counts %>%
               select(day_of_week, route_id:end_name, avg_duration_min, 
                      avg_speed_mph),
             by = join_by(day_of_week, route_id, start, start_name, end, 
                          end_name),
             suffix = c("_new", "_old")) %>%
  # aggregate by calculating weighted averages
  group_by(route_id, start, start_name, end, end_name) %>%
  mutate(num_trips_per_week = if_else(day_of_week == "Weekday", 
                                      num_trips_per_day * 5, 
                                      num_trips_per_day),
         prop_trips_per_week = num_trips_per_week / sum(num_trips_per_week),
         across(avg_duration_min_new:avg_speed_mph_old, ~ . * prop_trips_per_week)) %>%
  summarize(across(c(num_trips_per_week, avg_duration_min_new:avg_speed_mph_old), ~ sum(.)),
            distance_mi = first(distance_mi)) %>%
  ungroup() %>%
  # calculate stats
  mutate(avg_duration_min_diff = avg_duration_min_new - avg_duration_min_old,
         avg_duration_min_diff_perc = avg_duration_min_diff / avg_duration_min_old,
         avg_speed_mph_diff = avg_speed_mph_new - avg_speed_mph_old,
         avg_speed_mph_diff_perc = avg_speed_mph_diff / avg_speed_mph_old,
         across(c(distance_mi, avg_duration_min_new:avg_speed_mph_diff_perc), 
                as.numeric)) %>%
  # format
  select(route_id:end_name, num_trips_per_week,
         distance_mi,
         avg_duration_min_new, avg_duration_min_old, 
         avg_duration_min_diff, avg_duration_min_diff_perc,
         avg_speed_mph_new, avg_speed_mph_old,
         avg_speed_mph_diff, avg_speed_mph_diff_perc) %>%
  arrange(route_id, start_name, end_name)


# 3. Which segments take longer? ------------------------------------------


# 3.A. Setup --------------------------------------------------------------

# initialization TODO delete
# trips <- d_new$trips
# shapes <- d_new$shapes
# stops <- d_new$stops
# stop_times <- d_new$stop_times

# get stop id pairs
stop_pairs_new <- d_new$stop_times %>%
  select(trip_id, stop_sequence, stop_id, stop_time = arrival_time) %>%
  group_by(trip_id) %>%
  mutate(next_stop_id = lead(stop_id),
         next_stop_time = lead(stop_time)) %>%
  ungroup() %>%
  filter(!is.na(next_stop_id)) # remove trip ends
stop_pairs_old <- d_old$stop_times %>%
  select(trip_id, stop_sequence, stop_id, stop_time = arrival_time) %>%
  group_by(trip_id) %>%
  mutate(next_stop_id = lead(stop_id),
         next_stop_time = lead(stop_time)) %>%
  ungroup() %>%
  filter(!is.na(next_stop_id)) # remove trip ends

# get unique df of stop pairs
unique_stop_pairs <- stop_pairs_new %>%
  distinct(stop_id, next_stop_id)

# for each full-length trip direction, assign each stop to closest point on path
shape_ids <- c("d1_sb" = "312437", 
               "d1_nb" = "312441", 
               "d2_sb" = "312448", 
               "d2_nb" = "312451")

# Add geometry
# assign termini stops the first point on their respective shape. shapes that 
# are of same route but different direction have same start/end.
get_first_lon <- function(shapes_df, target_shape_id) {
  lon <- shapes_df %>%
    filter(shape_id == target_shape_id) %>%
    slice(1) %>%
    pull(shape_pt_lon)
  return(lon)
}
get_first_lat <- function(shapes_df, target_shape_id) {
  lat <- shapes_df %>%
    filter(shape_id == target_shape_id) %>%
    slice(1) %>%
    pull(shape_pt_lat)
  return(lat)
}
sn_lon <- get_first_lon(d_new$shapes, shape_ids["d1_sb"])
sn_lat <- get_first_lat(d_new$shapes, shape_ids["d1_sb"])
om_lon <- get_first_lon(d_new$shapes, shape_ids["d1_nb"])
om_lat <- get_first_lat(d_new$shapes, shape_ids["d1_nb"])
sh_lon <- get_first_lon(d_new$shapes, shape_ids["d2_nb"])
sh_lat <- get_first_lat(d_new$shapes, shape_ids["d2_nb"])
stops_sf <- d_new$stops %>%
  # adjust termini stop locations
  mutate(stop_lon = case_when(stop_name == "69th St Transit Center" ~ sn_lon,
                              stop_name == "Orange St/Media" ~ om_lon,
                              stop_name == "Chester Pike/Sharon Hill" ~ sh_lon,
                              TRUE ~ stop_lon),
         stop_lat = case_when(stop_name == "69th St Transit Center" ~ sn_lat,
                              stop_name == "Orange St/Media" ~ om_lat,
                              stop_name == "Chester Pike/Sharon Hill" ~ sh_lat,
                              TRUE ~ stop_lat)) %>%
  # convert to sf
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = GCS)

# create shape lines
shapes_sf <- d_new$shapes %>%
  distinct(shape_id, shape_pt_lat, shape_pt_lon,
           .keep_all = TRUE) %>% # get rid of duplicate shape points
  arrange(shape_id, shape_pt_sequence) %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = GCS) %>%
  group_by(shape_id) %>%
  summarize(#septa_km = shape_dist_traveled[shape_pt_sequence == max(shape_pt_sequence)],
            do_union=FALSE) %>%
  ungroup() %>%
  st_cast("LINESTRING")


# 3.B. Create Stop Segments -----------------------------------------------

# create stop segments function
create_stop_segments <- function(target_shape_id, trips, shapes_sf, stops_sf, 
                                 stop_times, stop_pairs) {
  # select relevant stops
  target_shape <- shapes_sf %>%
    filter(shape_id == target_shape_id)
  representative_trip_id <- trips %>%
    filter(shape_id == target_shape_id) %>%
    distinct(trip_id) %>%
    slice(1) %>%
    pull()
  relevant_stops <- stop_times %>%
    filter(trip_id == representative_trip_id) %>%
    select(stop_id, stop_sequence)
  relevant_stops_sf <- stops_sf %>%
    inner_join(relevant_stops, by = "stop_id") %>%
    arrange(stop_sequence)
  relevant_stop_pairs <- stop_pairs %>%
    filter(trip_id == representative_trip_id)
  
  # split shape into segments
  stop_segments <- list()
  for (i in 2:nrow(relevant_stops_sf)) {
    # last stop
    if (i == nrow(relevant_stops_sf)) {
      stop_segments[i - 1] <- target_shape[[1]]
      
      # intermediate stop
    } else {
      # select stop
      current_stop <- relevant_stops_sf %>%
        slice(i)
      
      # create shortest line between stop and shape
      shortest_line <- st_nearest_points(current_stop, target_shape) %>%
        st_as_sf() %>% 
        rename(geometry = x)
      
      # if line intersects shape
      line_intersects <- st_intersects(shortest_line, target_shape, 
                                       sparse=FALSE)
      if (FALSE) {
      # if (line_intersects) { TODO delete this section
        # split the shape
        parts <- st_collection_extract(st_split(target_shape$geometry, 
                                                shortest_line$geometry), 
                                       "LINESTRING") %>% 
          st_as_sf() %>%
          mutate(id = row_number()) %>%
          rename(geometry = x)
        
        # filter to segment (i-1, i)
        prev_segment <- parts %>%
          filter(id == 1) %>%
          pull(geometry)
        
        # append to list
        stop_segments[i - 1] <- prev_segment
        
        # shorten target_shape
        target_shape <- parts %>%
          filter(id == 2) %>%
          select(geometry)
        
        # if line does not intersect shape (due to floating point precision)
      } else {
        # calculate the floating point error
        e <- st_distance(shortest_line %>% 
                           pull(geometry), 
                         target_shape %>% 
                           pull(geometry))
        
        # add the floating point error to a buffer so it intersects
        shortest_buffer <- shortest_line %>%
          st_buffer(dist = e)
        
        # make sure there is an intersection
        intersects_test <- st_intersects(shortest_buffer, target_shape, 
                                         sparse=FALSE)
        num_no_intersects <- sum(!intersects_test)
        stopifnot("No intersection" = num_no_intersects == 0)
        
        # split the shape
        parts <- st_collection_extract(st_split(target_shape$geometry, 
                                                shortest_buffer$geometry), 
                                       "LINESTRING") %>% 
          st_as_sf()%>%
          mutate(id = row_number()) %>%
          st_cast("POINT") %>%
          rename(geometry = x)
        
        # filter to segment (i-1, i)
        prev_segment <- parts %>%
          filter(id == 1) %>%
          summarize(do_union = FALSE) %>%
          st_cast("LINESTRING") %>%
          pull(geometry)
        
        # append to list
        stop_segments[i - 1] <- prev_segment
        
        # shorten target_shape (need to keep first intersection and remove dups)
        # stopifnot("More than 2 intersections" =
        #             nrow(parts %>% filter(id == 2)) == 2) # TODO delete
        if (nrow(parts %>% filter(id == 2)) != 2) {
          print(paste0(i, " - ", current_stop$stop_name,
                       ": More than 2 intersections. Still split at first one."))
        }
        if (nrow(parts %>% filter(id == 2)) < 2) {
          message <- paste0(i, " - ", current_stop$stop_name,
                            ": Less than 2 intersections")
          stop(message)
        }
        target_shape <- parts %>%
          filter(id > 1) %>% 
          group_by(id) %>%
          mutate(delete = if_else(id == 2 & row_number() == max(row_number()),
                                  1, 0)) %>%
          ungroup() %>%
          filter(delete == 0) %>%
          # slice(-2) %>% # TODO delete
          summarize(do_union = FALSE) %>%
          st_cast("LINESTRING")
      }
    }
  }
  
  # append into table
  stop_segments <- st_sfc(stop_segments, crs = GCS)
  relevant_stop_pairs <- relevant_stop_pairs %>%
    mutate(shape_id = target_shape_id,
           geometry = stop_segments) %>%
    st_as_sf(crs = GCS) %>%
    select(shape_id, stop_sequence, stop_id, next_stop_id, geometry) %>%
    arrange(stop_sequence)
  
  # output
  return(relevant_stop_pairs)
}

# get stop segments
stop_segments_d1_sb <- create_stop_segments(shape_ids["d1_sb"], d_new$trips, 
                                            shapes_sf, stops_sf,
                                            d_new$stop_times, stop_pairs_new)
stop_segments_d1_nb <- create_stop_segments(shape_ids["d1_nb"], d_new$trips, 
                                            shapes_sf, stops_sf,
                                            d_new$stop_times, stop_pairs_new)
stop_segments_d2_sb <- create_stop_segments(shape_ids["d2_sb"], d_new$trips, 
                                            shapes_sf, stops_sf,
                                            d_new$stop_times, stop_pairs_new)
stop_segments_d2_nb <- create_stop_segments(shape_ids["d2_nb"], d_new$trips, 
                                            shapes_sf, stops_sf,
                                            d_new$stop_times, stop_pairs_new)

# mapping test
tm_shape(stop_segments_d2_nb %>% mutate(x = stop_sequence %% 5)) +
  tm_lines(col = "x",
           col.scale = tm_scale_intervals(style = "fixed",
                                          breaks = c(0, 1, 2, 3, 4, 5),
                                          values = "viridis")) +
  tm_shape(stops_sf) +
  tm_dots(fill = "red")


# 3.C. Calculate Average Duration and Speed -------------------------------

# function to adjust stop sequence numbers so they are consistent between D1 
# and D2
standardize_stop_sequences <- function(stop_segments_d1, stop_segments_d2) {
  # assign stop sequences to overlapping stops
  # - note that this still works in southbound direction because 
  #   stop_sequence.x == stop_sequence.y in southbound direction
  adj_stop_sequences <- stop_segments_d1 %>%
    as_tibble() %>%
    select(-geometry) %>%
    inner_join(stop_segments_d2 %>%
                 as_tibble() %>%
                 select(-geometry), 
               by = c("stop_id", "next_stop_id")) %>%
    mutate(stop_sequence_adj = if_else(stop_sequence.x > stop_sequence.y,
                                       stop_sequence.x, stop_sequence.y)) %>%
    select(stop_id, next_stop_id, stop_sequence_adj)
  
  # adjust stop sequences
  stop_segments_d1 <- stop_segments_d1 %>%
    left_join(adj_stop_sequences, by = c("stop_id", "next_stop_id")) %>%
    mutate(route_id = if_else(is.na(stop_sequence_adj), 
                              "D1",
                              "Both"),
           stop_sequence_adj = if_else(is.na(stop_sequence_adj), 
                                       stop_sequence,
                                       stop_sequence_adj)) %>%
    relocate(geometry, .after = last_col())
  stop_segments_d2 <- stop_segments_d2 %>%
    left_join(adj_stop_sequences, by = c("stop_id", "next_stop_id")) %>%
    mutate(route_id = if_else(is.na(stop_sequence_adj), 
                              "D2",
                              "Both"),
           stop_sequence_adj = if_else(is.na(stop_sequence_adj), 
                                       stop_sequence,
                                       stop_sequence_adj)) %>%
    relocate(geometry, .after = last_col())
  
  # output
  stop_segments_both <- list("d1" = stop_segments_d1, 
                             "d2" = stop_segments_d2)
  return(stop_segments_both)
}
# TODO delete
# standardize_stop_sequences <- function(stop_segments_d1, stop_segments_d2) {
#   # assign stop sequences to overlapping stops
#   adj_stop_sequences <- stop_segments_d1 %>%
#     as_tibble() %>%
#     select(-geometry) %>%
#     inner_join(stop_segments_d2 %>%
#                  as_tibble() %>%
#                  select(-geometry), 
#                by = "stop_id") %>%
#     mutate(stop_sequence_adj = if_else(stop_sequence.x > stop_sequence.y,
#                                        stop_sequence.x, stop_sequence.y)) %>%
#     select(stop_id, stop_sequence_adj)
#   
#   # adjust stop sequences
#   stop_segments_d1 <- stop_segments_d1 %>%
#     left_join(adj_stop_sequences, by = "stop_id") %>%
#     mutate(stop_sequence_adj = if_else(is.na(stop_sequence_adj), 
#                                        stop_sequence,
#                                        stop_sequence_adj),
#            route_id = "D1")
#   stop_segments_d2 <- stop_segments_d2 %>%
#     left_join(adj_stop_sequences_nb, by = "stop_id") %>%
#     mutate(stop_sequence_adj = if_else(is.na(stop_sequence_adj), 
#                                        stop_sequence,
#                                        stop_sequence_adj),
#            route_id = "D2")
#   
#   # output
#   stop_segments_both <- list("d1" = stop_segments_d1, 
#                              "d2" = stop_segments_d2)
#   return(stop_segments_both)
# }

# function to calculate distance between stops
calculate_dist_btwn_stops <- function(stop_segments_d1, stop_segments_d2) {
  # # standardize stop sequences TODO delete
  # stop_segments_list <- standardize_stop_sequences(stop_segments_d1,
  #                                                  stop_segments_d2)
  # stop_segments_d1 <- stop_segments_list$d1
  # stop_segments_d2 <- stop_segments_list$d2
  
  # calculate distance between stops
  stop_segments <- stop_segments_d1 %>%
    bind_rows(stop_segments_d2) %>%
    distinct(stop_id, next_stop_id, stop_sequence_adj, route_id,
             .keep_all = TRUE) %>%
    mutate(distance_mi = as.numeric(set_units(st_length(geometry), "mi"))) %>%
    select(stop_id, next_stop_id, stop_sequence_adj, route_id,
           distance_mi, geometry) %>%
    arrange(stop_sequence_adj, route_id)
  return(stop_segments)
}

# function to calculate average time and speed
calculate_avg_speed <- function(stop_segments, stop_pairs) {
  # calculate time between stops
  stop_pairs <- stop_pairs %>%
    mutate(stop_time_hms = hms(stop_time),
           next_stop_time_hms = hms(next_stop_time),
           duration_sec = as.numeric(next_stop_time_hms - stop_time_hms)) %>%
    select(-stop_time_hms, -next_stop_time_hms)
  
  # merge stop segments with stop pairs
  stop_segment_times <- stop_pairs %>%
    inner_join(stop_segments, by = c("stop_id", "next_stop_id")) %>%
    st_as_sf(crs = GCS) %>%
    mutate(speed_mph = distance_mi / (duration_sec / 3600)) # TODO potentially remove
  
  # calculate average time and speed
  stop_segment_speeds_avg <- stop_segment_times %>%
    group_by(stop_id, next_stop_id, stop_sequence_adj, route_id) %>%
    summarize(distance_mi = first(distance_mi),
              avg_duration_sec = mean(duration_sec),
              avg_speed_mph = distance_mi / (avg_duration_sec / 3600), # mean(speed_mph), TODO delete comment
              geometry = first(geometry)) %>%
    ungroup() %>%
    arrange(stop_sequence_adj, route_id)
  
  # output
  return(stop_segment_speeds_avg)
}

# function to compare new and old stop segments
compare_new_vs_old_stop_segments <- function(new, old) {
  stop_segment_comparison <- new %>%
    inner_join(old %>%
                 as_tibble() %>%
                 select(stop_id, next_stop_id, stop_sequence_adj, route_id,
                        avg_duration_sec, avg_speed_mph),
               by = c("stop_id", "next_stop_id", "stop_sequence_adj", 
                      "route_id"),
               suffix = c("_new", "_old"))
  stop_segment_comparison <- stop_segment_comparison %>%
    mutate(avg_duration_sec_diff = avg_duration_sec_new - avg_duration_sec_old,
           avg_duration_sec_diff_perc = avg_duration_sec_diff / avg_duration_sec_old,
           avg_speed_mph_diff = avg_speed_mph_new - avg_speed_mph_old,
           avg_speed_mph_diff_perc = avg_speed_mph_diff / avg_speed_mph_old) %>%
    relocate(geometry, .after = last_col()) %>%
    arrange(stop_sequence_adj)
  return(stop_segment_comparison)
}

# standardize stop sequences
stop_segments_nb <- standardize_stop_sequences(stop_segments_d1_nb,
                                               stop_segments_d2_nb)
stop_segments_d1_nb <- stop_segments_nb$d1
stop_segments_d2_nb <- stop_segments_nb$d2
stop_segments_sb <- standardize_stop_sequences(stop_segments_d1_sb,
                                               stop_segments_d2_sb)
stop_segments_d1_sb <- stop_segments_sb$d1
stop_segments_d2_sb <- stop_segments_sb$d2

# calculate distance between stops
stop_segments_nb <- calculate_dist_btwn_stops(stop_segments_d1_nb,
                                              stop_segments_d2_nb)
stop_segments_sb <- calculate_dist_btwn_stops(stop_segments_d1_sb,
                                              stop_segments_d2_sb)

# calculate average time and speed
new_stop_segment_speeds_avg_nb <- calculate_avg_speed(stop_segments_nb, 
                                                      stop_pairs_new)
old_stop_segment_speeds_avg_nb <- calculate_avg_speed(stop_segments_nb, 
                                                      stop_pairs_old)
new_stop_segment_speeds_avg_sb <- calculate_avg_speed(stop_segments_sb, 
                                                      stop_pairs_new)
old_stop_segment_speeds_avg_sb <- calculate_avg_speed(stop_segments_sb, 
                                                      stop_pairs_old)

# compare
stop_segment_comparison_nb <- compare_new_vs_old_stop_segments(new_stop_segment_speeds_avg_nb,
                                                               old_stop_segment_speeds_avg_nb)
stop_segment_comparison_sb <- compare_new_vs_old_stop_segments(new_stop_segment_speeds_avg_sb,
                                                               old_stop_segment_speeds_avg_sb)
stop_segment_comparison_all <- bind_rows(stop_segment_comparison_nb %>%
                                           mutate(direction = "northbound"),
                                         stop_segment_comparison_sb %>%
                                           mutate(direction = "southbound")) %>%
  relocate(direction, .after = next_stop_id) %>%
  relocate(geometry, .after = last_col()) %>%
  arrange(direction, stop_sequence_adj, route_id)


# 3.D. Visualize ----------------------------------------------------------

# grade crossings
gc <- gc %>%
  st_as_sf(coords = c("lon", "lat"), crs = GCS)

# Function to visualize time difference via map TODO make consistent and add grade crossing points and median running buffers
visualize_map <- function(stop_segment_comparison, heading, grade_crossings, street_running, metric) {
  # error handling
  if (is.na(heading) | (heading != "northbound" & heading != "southbound")) {
    stop("Incorrect heading. Must be 'northbound' or 'southbound'.")
  }
  
  # filter to direction
  stop_segment_comparison <- stop_segment_comparison %>% 
    filter(direction == heading)
  
  # add street running buffer TODO
  
  # map
  tm_shape(stop_segment_comparison) +
    tm_lines(col = metric,
             col.scale = tm_scale(values = "magma"),
             lwd = 2) +
    tm_shape(grade_crossings) +
    tm_dots(fill = "notes")
}

# create maps TODO there should be a segment with 0 difference but it's not showing up. why?
visualize_map(stop_segment_comparison_all, "northbound", gc, sr, "avg_duration_sec_diff_perc") # "avg_speed_mph_diff_perc
visualize_map(stop_segment_comparison_all, "southbound", gc, sr, "avg_duration_sec_diff_perc") # "avg_speed_mph_diff_perc


heading <- "northbound"
line <- "D1"

# TODO delete
# stop_segment_comparison <- stop_segment_comparison_all %>%
#   filter(direction == heading,
#          route_id == line | route_id == "Both")
# stop_segment_comparison <- stop_segment_comparison %>%
#   add_row(stop_id = stop_segment_comparison$next_stop_id[nrow(stop_segment_comparison)],
#           direction = stop_segment_comparison$direction[nrow(stop_segment_comparison)],
#           route_id = stop_segment_comparison$route_id[nrow(stop_segment_comparison)],
#           avg_duration_sec_new = stop_segment_comparison$avg_duration_sec_new[nrow(stop_segment_comparison)],
#           avg_duration_sec_old = stop_segment_comparison$avg_duration_sec_old[nrow(stop_segment_comparison)]) %>%
#   mutate(cum_distance_mi = cumsum(distance_mi),
#          lag_cum_distance_mi = lag(cum_distance_mi),
#          lag_cum_distance_mi = if_else(is.na(lag_cum_distance_mi), 
#                                       0, 
#                                       lag_cum_distance_mi))
# 
# p <- ggplot(stop_segment_comparison) +
#   geom_step(aes(x=lag_cum_distance_mi, y = avg_duration_sec_new),
#             colour = "red") +
#   geom_step(aes(x=lag_cum_distance_mi, y = avg_duration_sec_old),
#             colour = "green") +
#   geom_ribbon(aes(x = lag_cum_distance_mi, ymin = avg_duration_sec_old, ymax = avg_duration_sec_new),
#               fill = "red", alpha = 0.5)
# ggplotly(p)

stop_segment_comparison <- stop_segment_comparison_all %>%
  filter(direction == heading,
         route_id == line | route_id == "Both")
stop_segment_comparison <- stop_segment_comparison %>%
  add_row(stop_id = stop_segment_comparison$next_stop_id[nrow(stop_segment_comparison)],
          direction = stop_segment_comparison$direction[nrow(stop_segment_comparison)],
          route_id = stop_segment_comparison$route_id[nrow(stop_segment_comparison)],
          avg_duration_sec_new = stop_segment_comparison$avg_duration_sec_new[nrow(stop_segment_comparison)],
          avg_duration_sec_old = stop_segment_comparison$avg_duration_sec_old[nrow(stop_segment_comparison)],
          avg_speed_mph_new = stop_segment_comparison$avg_speed_mph_new[nrow(stop_segment_comparison)],
          avg_speed_mph_old = stop_segment_comparison$avg_speed_mph_old[nrow(stop_segment_comparison)]) %>%
  mutate(cum_distance_mi = cumsum(distance_mi),
         lag_cum_distance_mi = lag(cum_distance_mi),
         lag_cum_distance_mi = if_else(is.na(lag_cum_distance_mi),
                                       0,
                                       lag_cum_distance_mi)) %>%
  as_tibble() %>%
  select(stop_id, next_stop_id, avg_duration_sec_new, avg_duration_sec_old,
         avg_speed_mph_new, avg_speed_mph_old,
         cum_distance_mi, lag_cum_distance_mi)
stop_segment_comparison <- stop_segment_comparison %>%
  bind_rows(stop_segment_comparison) %>%
  group_by(stop_id, next_stop_id) %>%
  arrange(lag_cum_distance_mi) %>%
  ungroup() %>%
  mutate(id = row_number(),
         x = if_else(id %% 2 == 0, cum_distance_mi, lag_cum_distance_mi)) %>%
  filter(!is.na(x)) # remove last observation

ggplot(stop_segment_comparison) +
  geom_ribbon(aes(x = x, ymin = avg_speed_mph_new, ymax = avg_speed_mph_old),
              fill = "red", alpha = 0.5) +
  geom_line(aes(x=x, y = avg_speed_mph_new),
            colour = "red") +
  geom_line(aes(x=x, y = avg_speed_mph_old),
            colour = "green")
ggplot(stop_segment_comparison) +
  geom_ribbon(aes(x = x, ymin = avg_duration_sec_old, ymax = avg_duration_sec_new),
              fill = "red", alpha = 0.5) +
  geom_line(aes(x=x, y = avg_duration_sec_new),
            colour = "red") +
  geom_line(aes(x=x, y = avg_duration_sec_old),
            colour = "green")


# Output ------------------------------------------------------------------

# write to excel
excel_list <- list("route_shape_direction_day_comp" = trip_time_comparison,
                   "route_shape_direction_comp" = trip_time_comparison2)
write_xlsx(excel_list, TRIP_TIME_COMP_PATH)

# write to shapefile
st_write(stop_segment_comparison_all, STOP_SEG_COMP_PATH, 
         layer = "stop_segment_comp")
