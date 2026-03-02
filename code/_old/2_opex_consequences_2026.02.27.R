## Operating Consequences


# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

# load packages
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse, rstudioapi, readxl, magrittr)

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# constants
MINS_IN_HOUR <- 60
WEEKS_IN_YEAR <- 52
DAYS_IN_YEAR <- 365
TRIPS_PER_DAY <- 2
YEAR <- 2024

# read paths
ROUTE_STATISTICS_PATH <- "./input/Route Statistics/Route Statistics_D1_D2.csv"
COMPARISONS_PATH <- "./intermediate/trip_time_comparisons.xlsx"
SHEET <- "route_shape_direction_comp"

# write paths


# Read --------------------------------------------------------------------

route_statistics <- read_csv(ROUTE_STATISTICS_PATH, skip = 1, guess_max = Inf)
trip_comparisons <- read_xlsx(COMPARISONS_PATH, sheet = SHEET, guess_max = Inf)


# Opex --------------------------------------------------------------------

# calculate time added travelling
time_added <- trip_comparisons %>%
  mutate(total_trip_time_added_min_per_week = 
           avg_duration_min_diff * num_trips_per_week) %>%
  group_by(route_id) %>%
  summarize(total_trip_time_added_hr = 
              sum(total_trip_time_added_min_per_week) / MINS_IN_HOUR * WEEKS_IN_YEAR) %>%
  ungroup()

# calculate additional opex had 2024 had the additional vrh
additional_costs <- route_statistics %>%
  filter(`Fiscal Year` == YEAR) %>%
  inner_join(time_added, by = c("Route ID" = "route_id")) %>%
  mutate(cost_per_vrh = `Variable Expenses` / `Vehicle Hours`, # `Fully Allocated Expenses` / `Vehicle Hours`,
         additional_cost = cost_per_vrh * total_trip_time_added_hr,
         ) %>%
  select(route_id = `Route ID`, additional_cost)


# Lost Value --------------------------------------------------------------

hourly_wage <- 7.25 # TODO get better estimate
value_of_hour <- hourly_wage / 2

# calculate average route speeds
avg_route_speeds <- trip_comparisons %>%
  mutate(total_distance_per_week = distance_mi * num_trips_per_week,
         total_time_new_per_week = avg_duration_min_new * num_trips_per_week,
         total_time_old_per_week = avg_duration_min_old * num_trips_per_week) %>%
  group_by(route_id) %>%
  summarize(across(c(num_trips_per_week, total_distance_per_week,
                     total_time_new_per_week, total_time_old_per_week),
                   sum)) %>%
  ungroup() %>%
  mutate(avg_speed_mph_new = total_distance_per_week / (total_time_new_per_week / MINS_IN_HOUR),
         avg_speed_mph_old = total_distance_per_week / (total_time_old_per_week / MINS_IN_HOUR)) %>%
  select(route_id, avg_speed_mph_new, avg_speed_mph_old)
  
# calculate average additional time and cost spent on train
added_time_costs <- route_statistics %>%
  filter(`Fiscal Year` == YEAR) %>%
  select(`Route ID`, `Avearge Trip Length`, `Annual Passengers`) %>%
  inner_join(avg_route_speeds, by = c("Route ID" = "route_id")) %>%
  mutate(avg_trip_time_new_min = `Avearge Trip Length` / avg_speed_mph_new * MINS_IN_HOUR,
         avg_trip_time_old_min = `Avearge Trip Length` / avg_speed_mph_old * MINS_IN_HOUR,
         avg_additional_time_min = 9, #avg_trip_time_new_min - avg_trip_time_old_min,
         added_time_cost_per_pax = avg_additional_time_min * value_of_hour / MINS_IN_HOUR,
         added_time_cost_per_pax_day = added_time_cost_per_pax * TRIPS_PER_DAY,
         added_time_cost_per_pax_year = added_time_cost_per_pax_day * DAYS_IN_YEAR,
         added_time_cost_total_year = added_time_cost_per_pax * `Annual Passengers`) %>%
  select(route_id = `Route ID`, avg_trip_time_new_min:added_time_cost_total_year)
added_time_costs %$% sum(added_time_cost_total_year)
