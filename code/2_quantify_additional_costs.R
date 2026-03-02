#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
# Title:   Quantify Additional Costs
# Author:  Aden Yacobi
# Date:    2026/03/02
#
# Quantify additional operating costs to SEPTA and time costs to passengers.
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
p_load(tidyverse, rstudioapi, readxl, magrittr, writexl)

setwd(dirname(dirname(getActiveDocumentContext()$path)))


# INPUTS ------------------------------------------------------------------

# constants
HOURLY_WAGE <- 24.89 # U.S. BLS Occupational Employment and Wage Statistics
SEPTA_ADDED_TIME_MIN <- 7
SEPTA_ADDED_TIME_MAX <- 9
MINS_IN_HOUR <- 60
WEEKS_IN_YEAR <- 52
DAYS_IN_YEAR <- 365
WORKDAY_PROP <- 5 / 7
TRIPS_PER_DAY <- 2
YEAR <- 2024

# read paths
ROUTE_STATISTICS_PATH <- paste0("./input/SEPTA/Route Statistics/", 
                                "Route Statistics_D1_D2.csv")
COMPARISONS_PATH <- "./intermediate/trip_time_comparisons.xlsx"
SHEET <- "route_shape_direction_comp"

# write paths
WRITE_PATH <- "./intermediate/added_costs.xlsx"


# Read --------------------------------------------------------------------

route_statistics <- read_csv(ROUTE_STATISTICS_PATH, skip = 1, guess_max = Inf)
trip_comparisons <- read_xlsx(COMPARISONS_PATH, sheet = SHEET, guess_max = Inf)


# Opex --------------------------------------------------------------------

# calculate time added travelling across all trips, including non-full-length 
# trips
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
  mutate(cost_per_vrh = `Variable Expenses` / `Vehicle Hours`,
         diff_cost = cost_per_vrh * total_trip_time_added_hr,
         diff_perc_cost = diff_cost / `Fully Allocated Expenses`,
         new_cost = `Fully Allocated Expenses` + diff_cost,
         old_cost_per_upt = `Fully Allocated Expenses` / `Annual Passengers`,
         new_cost_per_upt = new_cost / `Annual Passengers`,
         diff_cost_per_upt = new_cost_per_upt - old_cost_per_upt,
         diff_perc_cost_per_upt = diff_cost_per_upt / old_cost_per_upt
         ) %>%
  select(route_id = `Route ID`, cost_per_vrh, 
         new_cost, original_cost = `Fully Allocated Expenses`,
         diff_cost, diff_perc_cost,
         new_cost_per_upt, old_cost_per_upt,
         diff_cost_per_upt, diff_perc_cost_per_upt)

# total d1+d2 metrics
additional_costs_total <- route_statistics %>%
  filter(`Fiscal Year` == YEAR) %>%
  summarize(`Route ID` = "Total",
            across(c(`Variable Expenses`,
                     `Vehicle Hours`, 
                     `Fully Allocated Expenses`,
                     `Annual Passengers`), 
            sum))
time_added_total <- time_added %>%
  summarize(route_id = "Total",
            total_trip_time_added_hr = sum(total_trip_time_added_hr))
additional_costs_total <- additional_costs_total %>%
  inner_join(time_added_total, by = c("Route ID" = "route_id")) %>%
  mutate(cost_per_vrh = `Variable Expenses` / `Vehicle Hours`,
         diff_cost = cost_per_vrh * total_trip_time_added_hr,
         diff_perc_cost = diff_cost / `Fully Allocated Expenses`,
         new_cost = `Fully Allocated Expenses` + diff_cost,
         old_cost_per_upt = `Fully Allocated Expenses` / `Annual Passengers`,
         new_cost_per_upt = new_cost / `Annual Passengers`,
         diff_cost_per_upt = new_cost_per_upt - old_cost_per_upt,
         diff_perc_cost_per_upt = diff_cost_per_upt / old_cost_per_upt
  ) %>%
  select(route_id = `Route ID`, cost_per_vrh, 
         new_cost, original_cost = `Fully Allocated Expenses`,
         diff_cost, diff_perc_cost,
         new_cost_per_upt, old_cost_per_upt,
         diff_cost_per_upt, diff_perc_cost_per_upt)

# add in totals
additional_costs <- additional_costs %>%
  bind_rows(additional_costs_total)
print(additional_costs)
print("Total additional operating costs:")
print(additional_costs_total$diff_cost)


# Lost Value --------------------------------------------------------------

# function to calculate additional time and cost
# Input: rs - route statistics
#        added_time_per_trip - minutes or NA if using estimate based on uniform
#                              speed
# Output: tibble with additional time and costs for each route
calc_additional_time_and_cost <- function(rs, added_time_per_trip) {
  # add in route statistcs
  added_time_costs <- route_statistics %>%
    filter(`Fiscal Year` == YEAR) %>%
    select(`Route ID`, `Avearge Trip Length`, `Annual Passengers`) %>%
    inner_join(avg_route_speeds, by = c("Route ID" = "route_id"))
  
  # use uniform estimate or SEPTA-provided figures
  if (is.na(added_time_per_trip)) {
    added_time_costs <- added_time_costs %>%
      mutate(avg_trip_time_new_min = `Avearge Trip Length` / avg_speed_mph_new * MINS_IN_HOUR,
             avg_trip_time_old_min = `Avearge Trip Length` / avg_speed_mph_old * MINS_IN_HOUR,
             avg_additional_time_min = avg_trip_time_new_min - avg_trip_time_old_min)
  } else {
    added_time_costs <- added_time_costs %>%
      mutate(avg_trip_time_new_min = NA_real_,
             avg_trip_time_old_min = NA_real_,
             avg_additional_time_min = added_time_per_trip)
  }
  
  # calcualate costs
  added_time_costs <- added_time_costs %>%
    mutate(added_time_cost_per_pax = avg_additional_time_min * value_of_hour / MINS_IN_HOUR,
           added_time_cost_per_pax_day = added_time_cost_per_pax * TRIPS_PER_DAY,
           added_time_cost_per_pax_year = added_time_cost_per_pax_day * DAYS_IN_YEAR * WORKDAY_PROP,
           added_time_cost_total_year = added_time_cost_per_pax * `Annual Passengers`) %>%
    select(route_id = `Route ID`, 
           annual_passengers = `Annual Passengers`,
           avg_trip_time_new_min:added_time_cost_total_year)
  
  return(added_time_costs)
}

# value of hour is 1/2*wage rate
value_of_hour <- round(HOURLY_WAGE / 2, 2)

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

# min
added_time_costs_min <- calc_additional_time_and_cost(route_statistics, 
                                                      SEPTA_ADDED_TIME_MIN)
print("Min travel time costs:")
print(added_time_costs_min %$% sum(added_time_cost_total_year))

# max
added_time_costs_max <- calc_additional_time_and_cost(route_statistics, 
                                                      SEPTA_ADDED_TIME_MAX)
print("Max travel time costs:")
print(added_time_costs_max %$% sum(added_time_cost_total_year))

# uniform speed estimate
added_time_costs_estimate <- calc_additional_time_and_cost(route_statistics, 
                                                           NA)
print("Uniform speed estimate travel time costs:")
print(added_time_costs_estimate %$% sum(added_time_cost_total_year))


# Write -------------------------------------------------------------------

excel_list <- list("delta_opex" = additional_costs,
                   "delta_time_costs_min" = added_time_costs_min,
                   "delta_time_costs_max" = added_time_costs_max,
                   "delta_time_costs_estimate" = added_time_costs_estimate)
write_xlsx(excel_list, WRITE_PATH)

