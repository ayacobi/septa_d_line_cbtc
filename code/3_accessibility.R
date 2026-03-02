#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
# Title:   Accessibility
# Author:  Aden Yacobi
# Date:    2026/03/02
#
# Calculate accessibility to jobs and map isochrones. The algorithm sees how 
# long it takes to reach the destination from the origin every minute, assuming
# the trip clock begins immediately, between 7am and 9am on a Wednesday. The 
# median time it takes to travel for each origin-destination pair is then 
# compared to the 45 minute cutoff to determine if it is accessible or not.
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
p_load(tidyverse, rstudioapi, r5r, sf, data.table, tmap, janitor, writexl)

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# coordinate reference systems
GCS <- 4326 # WGS84
PCS <- 2272 # NAD83 / Pennsylvania South (ftUS)

# routing inputs
MODE <- c("WALK", "TRANSIT")
MAX_WALK_TIME <- 30      # in minutes
MAX_TRIP_DURATION <- 45  # in minutes
TIME_WINDOW <- 120       # in minutes
DEPARTURE_DATETIME_OLD <- as.POSIXct("18-02-2026 7:00:00",
                                     format = "%d-%m-%Y %H:%M:%S")
DEPARTURE_DATETIME_NEW <- as.POSIXct("25-02-2026 7:00:00",
                                     format = "%d-%m-%Y %H:%M:%S")
PERCENTILES <- c(50) # median accessibility only

# relevant counties (filter to lessen memories. I checked that this is inclusive
# of 45 minute transit travel times under median time scenario.)
COUNTIES <- c(
  "017", # Bucks
  "029", # Chester
  "045", # Delaware
  "091", # Montgomery
  "101")  # Philadelphia

# origins for mapping (from Google Maps)
ORIGINS <- tibble(id = c("Orange St/Media", "Sharon Hill", 
                         "Drexel Hill Junction", "69th St Transit Center"), 
                  lon = c(-75.3938792551593, -75.27806969923209, 
                          -75.29283481754955, -75.2596839468921), 
                  lat = c(39.918535820576224, 39.90649575288983, 
                          39.94699948308914, 39.96216420913261))

# coordinates to delete metro lines nearby from map (from Google Maps)
TROLLEY_DIVERSION_FILTERS <- tibble(id = c("40th/Market", 
                                           "40th/Walnut", 
                                           "49th/Greenway"),
                                    lon = c(-75.20186440305577, 
                                            -75.20259204009699, 
                                            -75.2138537663788),
                                    lat = c(39.95788702742263, 
                                            39.95414202021738, 
                                            39.94193301079272))

# read paths
# Note that the network uses the "new" GTFS data from Script 1. As the comments
# in that script say, I confirmed the D1 and D2 service are the same between the
# old GTFS data and the pre-Feb 22 part of the new GTFS script. So I use just
# the new GTFS data to make the network that way I only need to make one network
# and it is easier to follow along.
ND_PATH <- "./input/Network Dataset"
JOBS_PATH <- "./input/OnTheMap/philly_metro_all_jobs_2023/points_2023.shp"
BLOCKS_PATH <- paste0("./input/Census TIGER Line Shapefiles/", 
                      "tl_2025_42_tabblock20/tl_2025_42_tabblock20.shp")

# write paths
ACCESSIBILITY_PATH <- "./intermediate/accessibility.xlsx"

# map output variables
MAP_FOLDER <- "./output/maps/"
MAP_FILE_TYPE <- ".png"
MAP_WIDTH <- 6.0
MAP_HEIGHT <- 7.5
D_MAP_WIDTH <- 6.000000
D_MAP_HEIGHT <- 5.018987


# Read --------------------------------------------------------------------

# build network - no elevation raster
nd_path_normalized <- normalizePath(ND_PATH)
r5r_network <- r5r::build_network(ND_PATH)

# jobs (philly metro area from LODES)
jobs <- st_read(JOBS_PATH) %>%
  st_transform(crs = GCS)

# census blocks (pennsylvania)
blocks <- st_read(BLOCKS_PATH) %>%
  st_transform(crs = GCS)


# Access to Jobs ----------------------------------------------------------

# Function to calculate travel time matrices and add jobs
# Input: nd - r5r network
#        destinations - destinations to calculate travel time for
#        post_cbtc - T if examining post-CBTC situation, F if not
# Output: travel time matrix with number of jobs at destination
calculate_ttm <- function(nd, destinations, jobs_df, post_cbtc) {
  departure_datetime <- DEPARTURE_DATETIME_OLD
  if (post_cbtc) {
    departure_datetime <- DEPARTURE_DATETIME_NEW
  }
  ttm <- travel_time_matrix(nd,
                            origins = ORIGINS,
                            destinations = destinations,
                            mode = MODE,
                            departure_datetime = departure_datetime,
                            max_walk_time = MAX_WALK_TIME,
                            max_trip_duration = MAX_TRIP_DURATION,
                            time_window = TIME_WINDOW,
                            percentiles = PERCENTILES,
                            progress = FALSE)
  jobs_df <- jobs_df %>%
    st_drop_geometry() %>%
    select(id, num_jobs = c000)
  ttm <- ttm %>%
    left_join(jobs_df, by = c("to_id" = "id")) %>%
    mutate(num_jobs = if_else(is.na(num_jobs), 0, num_jobs))
  return(ttm)
}

# turn census blocks into points
blocks_centroid <- blocks %>%
  filter(COUNTYFP20 %in% COUNTIES) %>%
  select(GEOID20, geometry) %>%
  st_centroid()
blocks_lat_lon <- blocks_centroid %>%
  cbind(st_coordinates(.)) %>%
  rename(id = GEOID20, lon = X, lat = Y) %>%
  st_drop_geometry()

# calculate travel time matrices
ttm_old <- calculate_ttm(r5r_network, blocks_lat_lon, jobs, FALSE)
ttm_new <- calculate_ttm(r5r_network, blocks_lat_lon, jobs, TRUE)

# sum up all jobs and compare
jobs_old <- ttm_old %>%
  group_by(from_id) %>%
  summarize(num_jobs = sum(num_jobs))
jobs_new <- ttm_new %>%
  group_by(from_id) %>%
  summarize(num_jobs = sum(num_jobs))
jobs_comp <- jobs_new %>%
  inner_join(jobs_old, by = "from_id", suffix = c("_new", "_old")) %>%
  mutate(jobs_diff_num = num_jobs_new - num_jobs_old,
         jobs_diff_perc = jobs_diff_num / num_jobs_old) %>%
  arrange(jobs_diff_perc)
print(jobs_comp)

# save jobs_comp
accessibility_excel_list <- list("jobs_comp" = jobs_comp)
write_xlsx(accessibility_excel_list, ACCESSIBILITY_PATH)


# Map Accessibility -------------------------------------------------------
# Map the census blocks whose centroids are accessible within 
# max_tip_duration minutes pre- and post-CBTC.

# compare which census blocks are lost/gained after CBTC and add in their shape
change_order <- c("Lost", "No Change", "Gained")
ttm_comp <- ttm_new %>%
  full_join(ttm_old, by = c("from_id", "to_id"), suffix = c("_new", "_old")) %>%
  mutate(change = case_when(is.na(num_jobs_new) & !is.na(num_jobs_old) ~ "Lost",
                            !is.na(num_jobs_new) & is.na(num_jobs_old) ~ "Gained",
                            !is.na(num_jobs_new) & !is.na(num_jobs_old) ~ "No Change",
                            TRUE ~ NA_character_),
         change = factor(change, levels = change_order),
         num_jobs = if_else(is.na(num_jobs_new), num_jobs_old, num_jobs_new)) %>%
  select(from_id, to_id, num_jobs, change) %>%
  inner_join(blocks %>% 
               select(GEOID20, geometry),
             by = c("to_id" = "GEOID20")) %>%
  st_as_sf(crs = GCS)

# metro network
transit_net <- transit_network_to_sf(r5r_network)
metro_routes <- subset(transit_net$routes, mode %like% 'TRAM|SUBWAY')

# remove diversion routes
trolley_diversion_filters <- TROLLEY_DIVERSION_FILTERS %>%
  st_as_sf(coords = c("lon", "lat"), crs = GCS) %>%
  st_transform(crs = PCS) %>%
  st_buffer(dist = 200) %>%
  st_transform(crs = GCS)
remove_routes <- metro_routes %>% 
  st_intersects(trolley_diversion_filters, sparse = FALSE)
metro_routes <- metro_routes %>%
  mutate(id = row_number(),
         remove_flag_1 = remove_routes[,1],
         remove_flag_2 = remove_routes[,2],
         remove_flag_3 = remove_routes[,3]) %>%
  filter(!remove_flag_1 & !remove_flag_2 & !remove_flag_3)

# order layering of routes
metro_order <- c("D1", "D2", "B1", "B2", "B3", "L1", "M1", "T1", "T2", "T3", 
                 "T4", "T5", "G1")
metro_routes <- metro_routes %>%
  mutate(route_id = factor(route_id, levels = metro_order)) %>%
  arrange(desc(route_id))

# consistent bounding box for map
bounding_box <- st_bbox(ttm_comp, crs = GCS)

# define palettes
isochrone_palette <- c("Lost" = "red",
                       "No Change" = "grey",
                       "Gained" = "blue")
metro_palette <- c("B1" = "#F26722",
                   "B2" = "#F26722",
                   "B3" = "#F26722",
                   "D1" = "#E5417B",
                   "D2" = "#E5417B",
                   "G1" = "#FCD600",
                   "L1" = "#1A9AD6",
                   "M1" = "#613393",
                   "T1" = "#61A744",
                   "T2" = "#61A744",
                   "T3" = "#61A744",
                   "T4" = "#61A744",
                   "T5" = "#61A744"
                   )

# credits
credits <- paste0("Note: Isochrone created using Conveyal R\U2075 algorithm. ",
                  "The travel time between each origin and census block ",
                  "centroid\npair is calculated every minute between 7am and ",
                  "9am on a weekday. If the median transit travel time is ",
                  "less than or\nequal to 45 minutes, the census block is ",
                  "considered accessible from the origin. A small part of the ",
                  "change in\naccessibility is caused by new bus schedules, ",
                  "which rolled out at the same time CBTC was activated ",
                  "on the D1 and\nD2. One consequence of this is that some ",
                  "some census blocks became more accessible.\n",
                  "Source: SEPTA, US Census Bureau, Esri | Map: Aden Yacobi")

# Function to map change in access from origin point.
# Input: isochrones - sf with all census blocks accessible in either pre-CBTC
#                     or post-CBTC scenario
#        origin - character string identifying the station to begin at
#        metro_routes - sf of SEPTA metro lines
#        metro_palette - character vector of SEPTA metro colors
#        isochrone_palette - character vector of isochrone colors
#        bounding_box - bbox that defines map frame
#        credits - character string of sources and notes
map_isochrone <- function(isochrones, origin, metro_routes, metro_palette,
                          isochrone_palette, bounding_box, credits) {
  # select origin
  origin <- ORIGINS %>%
    filter(id == origin_id) %>%
    st_as_sf(coords = c("lon", "lat"), crs = GCS)
  
  # filter data for origin
  ttm_comp_sub <- isochrones %>% 
    filter(from_id == origin_id) %>%
    group_by(change) %>%
    summarize(do_union = TRUE) %>%
    ungroup()
  
  # map
  p <- tm_shape(metro_routes, bbox = bounding_box) +
    tm_lines(col = "route_id",
             col.scale = tm_scale_categorical(values = metro_palette),
             col.legend = tm_legend(show = FALSE),
             lwd = 2) +
    tm_shape(ttm_comp_sub) +
    tm_polygons(fill = "change",
                fill_alpha = 0.6,
                fill.scale = tm_scale_categorical(values = isochrone_palette),
                fill.legend = tm_legend(title = "Change in Access",
                                        position = tm_pos_in("right", "top"))) +
    tm_shape(origin) +
    tm_dots(fill = "id",
            size = 0.5,
            fill.scale = tm_scale_categorical(values = "black"),
            fill.legend = tm_legend(show = FALSE)) +
    tm_basemap("Esri.WorldTopoMap") +
    tm_title(origin_id) +
    tm_credits(credits,
               position = tm_pos_out("center", "bottom"))
  
  return(p)
}

# tmap options
tmap_mode("plot")
tmap_options(component.autoscale = FALSE)
tmap_options(text.fontfamily = "serif")

# create and save maps
for (origin_id in ORIGINS$id) {
  p <- map_isochrone(ttm_comp, origin_id, metro_routes, metro_palette, 
                     isochrone_palette, bounding_box, credits)
  map_name <- make_clean_names(origin_id)
  map_path <- paste0(MAP_FOLDER, map_name, MAP_FILE_TYPE)
  tmap_save(p, map_path, width = MAP_WIDTH, height = MAP_HEIGHT)
}


# Map D Line --------------------------------------------------------------

# get bounding box of D1 and D2
d_routes <- metro_routes %>% 
  filter(route_id %in% c("D1", "D2"))
bounding_box_d <- st_bbox(d_routes)

# adjust bounding box
delta <- 0.025
bounding_box_d_alt <- as.numeric(bounding_box_d)
bounding_box_d_alt[1] <- bounding_box_d_alt[1] - delta
bounding_box_d_alt[2] <- bounding_box_d_alt[2] - delta
bounding_box_d_alt[3] <- bounding_box_d_alt[3] + delta
bounding_box_d_alt[4] <- bounding_box_d_alt[4] + delta
names(bounding_box_d_alt) <- names(bounding_box_d)
bounding_box_d_alt <- st_bbox(bounding_box_d_alt, crs = GCS)

# caption
credits_d <- paste0("Note: The D1 begins its northbound journey in Media ",
                    "while the D2 begins its northbound journey\nin Sharon ",
                    "Hill. They merge together at Drexel Hill and continue ",
                    "to 69th St Transit Center with\nconnections to the L and ",
                    "M.\n",
                    "Source: SEPTA, ESRI | Map: Aden Yacobi")

# map
d_line_map <- tm_shape(metro_routes, bbox = bounding_box_d_alt) + 
  tm_lines(col = "route_id",
           col.scale = tm_scale_categorical(values = metro_palette),
           col.legend = tm_legend(show = FALSE),
           lwd = 3) +
  tm_credits("D1",
             position = c(0.07, 0.385),
             fontface = "bold",
             color = "white",
             size = 1,
             bg = TRUE,
             bg.color = "#E5417B",
             bg.alpha = 1,
  ) +
  tm_credits("D2",
             position = c(0.75, 0.23),
             fontface = "bold",
             color = "white",
             size = 1,
             bg = TRUE,
             bg.color = "#E5417B",
             bg.alpha = 1,
  ) +
  tm_basemap("Esri.WorldTopoMap") +
  tm_title("D1 and D2 Trolley Lines") +
  tm_credits(credits_d,
             position = tm_pos_out("center", "bottom"))

# output map
d_line_map_path <- paste0(MAP_FOLDER, "d_line", MAP_FILE_TYPE)
tmap_save(d_line_map, d_line_map_path, 
          width = D_MAP_HEIGHT, height = D_MAP_HEIGHT)


# Clean up after usage ----------------------------------------------------

r5r::stop_r5(r5r_network)
rJava::.jgc(R.gc = TRUE)

