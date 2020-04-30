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


## ----flow-download------------------------------------------------------------
# Download data for five sites, based on their gauge numbers:
#   - 405232 (Goulburn @ McCoys Bridge)
#   - 406201 (Campaspe @ Barnadown)
#   - 406202 (Campaspe @ Rochester D/S Waranga Western Ch Syphn)
#   - 406276 (Campaspe @ Fehrings Lane)
#   - 406278 (Campaspe @ Backhaus Road)
# `start` and `end` bound the dates we want, in any reasonable format (yyyy-mm-dd, dd-mm-yy, etc.).
# `variables` define the variables we want, with somewhat flexible names (e.g. discharge, streamflow,
#   flow, temp, temperature, water_temperature). Can define variables by WMIS code if you know it.
flow_data <- fetch_data(
  sites = c("405232", "406201", "406202", "406276", "406278"),
  start = "2004-01-01",
  end = "2020-04-21",
  variables = c("flow", "temp", "depth")
)


## ----view-data, echo = FALSE--------------------------------------------------
flow_data %>% as_tibble


## ----check-quality------------------------------------------------------------
flow_data %>% check_quality %>% as_tibble


## ----flow-formatting----------------------------------------------------------
# let's put the three variables in their own columns, along with information
#   on their quality codes and unitsflow_wide <- flow_data %>% pivot_wider(
  id_cols = c(date_formatted, site_name, site_code),
  names_from = variable_name,
  values_from = c(value, units, quality_code)
)


