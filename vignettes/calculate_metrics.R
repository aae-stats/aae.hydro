## ----setup, include = FALSE---------------------------------------------------
# display code by default
knitr::opts_chunk$set(echo = TRUE)

# load some packages (won't need first 3 once package loads correctly)
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)

# source some scripts (won't need once package loads correctly)
source("R/db_queries.R")
source("R/fetch_data.R")
source("R/calculate.R")

# get some data to work with in the vignette
flow_data <- fetch_data(
  sites = c("405232", "406201", "406202", "406276", "406278"),
  start = "2004-01-01",
  end = "2020-04-21",
  variables = c("flow", "temp", "depth")
)

# and convert it to a wider format
flow_wide <- flow_data %>% pivot_wider(
  id_cols = c(date_formatted, site_name, site_code),
  names_from = variable_name,
  values_from = c(value, units, quality_code)
)


## ----calculate-basic-examples-------------------------------------------------
# first, pull out the data for a single site (Goulburn @ McCoys Bridge)
#    (we can avoid this step, see *Working with many variables or systems*, below)
flow_405232 <- flow_wide %>% filter(site_code == "405232")

# calculate the median spring flow (months 9-11) for 2009-2015 at this site
#    - this function takes in the daily discharge, the dates, a function
#        ("survey", here) that defines the resolution of our flow metric,
#        and a function ("fun") that tells calculate how to summarise the
#        data
spring_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = survey(season = 9:11, lag = 0, subset = 2009:2015),
  fun = median,
  na.rm = TRUE
)


## ----print-flow-example, echo = FALSE-----------------------------------------
spring_flow %>% as_tibble


## ----resolution-examples------------------------------------------------------
# calculate the median weekly flow from 2004-2006
weekly_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = weekly(subset = 2004:2006),
  fun = median,
  na.rm = TRUE
)

# calculate the median monthly flow from 2005-2011
monthly_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = monthly(subset = 2005:2011),
  fun = median,
  na.rm = TRUE
)

# calculate the median annual flow from 2009-2018
annual_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = annual(subset = 2009:2018),
  fun = median,
  na.rm = TRUE
)

# calculate the overall median flow from 2004-2020
baseline_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = baseline(subset = 2004:2006),
  fun = median,
  na.rm = TRUE
)


## ----more-calculate-examples--------------------------------------------------
# calculate the maximum annual flow from 2005-2019
annual_max <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = annual(subset = 2005:2019),
  fun = max,
  na.rm = TRUE
)

# or the minimum
annual_min <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = annual(subset = 2005:2019),
  fun = min,
  na.rm = TRUE
)

# there's even some custom functions
# including one for rolling ranges
flow_variability <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = annual(subset = 2005:2019),
  fun = rolling_range,
  lag = 3
)

# and one to count days below a threshold (the baseline 10th percentile, here)
longterm_q10 <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = baseline(subset = 2004:2020),
  fun = quantile,
  p = 0.1
)
low_flow_days <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = annual(subset = 2005:2019),
  fun = days_below,
  threshold = longterm_q10
)

# it's easy to add lags, which can be especially useful for short-term flows
#   (e.g. average weekly flow one week prior to sampling)
lagged_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = weekly(subset = 2005:2006, lag = 1),
  fun = median
)

# the lag defaults to a semi-logical timeframe (weeks for weekly, years for annual)
#   but you can also specify this explicitly if needed
same_lagged_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = weekly(subset = 2005:2006, lag = days(7)),
  fun = median
)


## ----calculate-rescale-examples-----------------------------------------------
# calculate median spring flow from 2009-2015, but scale it by the long-term 
#   median flow from 2004-2020
spring_flow_standardised <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = survey(season = 9:11, lag = 0, subset = 2009:2015),
  rescale = list(subset = 2004:2020, fun = median, season = 1:12, args = list(na.rm = TRUE)),
  na.rm = TRUE
)


## ----calculate-survey-examples------------------------------------------------
# summer starting the year before a survey and ending in the survey year
summer_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = survey(season = 12:14, lag = 0, subset = 2009:2015),
  fun = median,
  na.rm = TRUE
)

# the 12 months prior to a May survey
summer_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = survey(season = 5:16, lag = 0, subset = 2009:2015),
  fun = median,
  na.rm = TRUE
)


## ----compare-survey-and-annual------------------------------------------------
# survey resolution for spring in the year prior to a survey
spring_flow <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = survey(season = 9:11, lag = 0, subset = 2009:2015),
  fun = median,
  na.rm = TRUE
)

# annual resolution for this same time period
spring_flow_lagged <- calculate(
  value = flow_405232$value_stream_discharge_mld,
  date = flow_405232$date_formatted,
  resolution = annual(season = 9:11, lag = 1, subset = 2009:2015),
  fun = median,
  na.rm = TRUE
)


## ----calculate-grouped--------------------------------------------------------
# data from multiple sites but only one variable
flow_by_site <- flow_wide %>%
  group_by(site_name, site_code) %>%
  do(
    calculate(
      value = .$value_stream_discharge_mld,
      date = .$date_formatted,
      resolution = survey(season = 9:11),
      na.rm = TRUE
    )
  ) %>%
  ungroup

# we can put this in a wider format if we want (each site has its own column)
flow_by_site_wide <- flow_by_site %>%
  pivot_wider(id_cols = date,
              names_from = site_code,
              values_from = metric)

# data from multiple variables and sites
variable_by_site <- flow_data %>%
  group_by(site_name, site_code, variable_name) %>%
  do(
    calculate(
      value = .$value,
      date = .$date_formatted,
      resolution = survey(season = 9:11),
      na.rm = TRUE
    )
  ) %>%
  ungroup

# we can put this in a wider format if we want (each variable has its own column)
variable_by_site_wide <- variable_by_site %>%
  pivot_wider(id_cols = c(site_name, site_code, date),
              names_from = variable_name,
              values_from = metric)

# or give each site and variable its own column
variable_by_site_wider <- variable_by_site %>%
  pivot_wider(id_cols = date,
              names_from = c(variable_name, site_code),
              values_from = metric)

# the following won't work now but is touted to work in dplyr v.1.0.0 and
#   seems much cleaner
# flow_data %>%
#   filter(variable_name == "stream_discharge_mld") %>%
#   group_by(site_name) %>%
#   summarise(
#     spring_flow = calculate(value, date_formatted, survey(season = 9:11), na.rm = TRUE)$metric,
#     summer_flow = calculate(value, date_formatted, survey(season = 12:3), na.rm = TRUE)$metric
#   )

