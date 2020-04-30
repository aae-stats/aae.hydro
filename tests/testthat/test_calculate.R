context("calculate")

test_that("calculate returns correct values without subsetting or lags", {

  nyear <- years(5)
  start <- dmy("01012010")
  flow_sim <- data.frame(
    date = seq(start, start + nyear, by = 1)
  )
  flow_sim$value <- exp(rnorm(nrow(flow_sim)))

})


# # test calculate_metrics
#
# # check all resolutions with and without subsets
# #  check annual, baseline, and survey with seasons
# #  check lags
# spring_flow <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = survey(season = 9:11, lag = 0, subset = 2010:2016),
#   na.rm = TRUE
# )
#
# test_lag <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = annual(season = 9:11, lag = 1, subset = 2010:2016),
#   na.rm = TRUE
# )
# test_no_lag <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = annual(season = 9:11, lag = 0, subset = 2009:2015),
#   na.rm = TRUE
# )
#
# spring_flow_lt <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = survey(season = 9:11, lag = 0, subset = 2009:2015),
#   rescale = list(subset = 2004:2020),
#   na.rm = TRUE
# )
#
# baseline_flow <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = baseline(subset = 2004:2020),
#   na.rm = TRUE
# )
#
# annual_ave <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = annual(subset = 2009:2015),
#   na.rm = TRUE
# )
#
# monthly_ave <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = monthly(subset = 2009:2015),
#   na.rm = TRUE
# )
#
# weekly_ave <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = weekly(subset = 2009:2015),
#   na.rm = TRUE
# )
#
# annual_max <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = annual(subset = 2009:2015),
#   fun = max,
#   na.rm = TRUE
# )
#
# q10_est <- calculate(
#   flow_405232$value_stream_discharge_mld,
#   flow_405232$date_formatted,
#   resolution = baseline(season = 5:7),
#   fun = quantile,
#   p = 0.1,
#   na.rm = TRUE
# )
# low_flow_days <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = survey(),
#   fun = days_below,
#   threshold = q10_est$metric,
#   na.rm = TRUE
# )
#
# flow_var <- calculate(
#   value = flow_405232$value_stream_discharge_mld,
#   date = flow_405232$date_formatted,
#   resolution = survey(season = 10:12, subset = 2007:2019),
#   fun = rolling_range,
#   lag = 3,
#   na.rm = TRUE
# )
#
#
# # multiple sites
# test_multi <- flow_data %>%
#   group_by(site_name) %>%
#   group_map(
#     calculate,
#     value = .$value_stream_discharge_mld,
#     date = .$date_formatted,
#     resolution = survey(season = 9:11),
#     na.rm = TRUE
#   ) %>%
#   ungroup
#
# test_multi <- flow_data %>%
#   group_by(site_name, site_code) %>%
#   do(
#     calculate(
#       value = .$value_stream_discharge_mld,
#       date = .$date_formatted,
#       resolution = survey(season = 9:11),
#       na.rm = TRUE
#     )
#   ) %>%
#   ungroup
#
# test_multi <- flow_long %>%
#   group_by(site_name, site_code, variable_name) %>%
#   do(
#     calculate(
#       value = .$value,
#       date = .$date_formatted,
#       resolution = survey(season = 9:11),
#       na.rm = TRUE
#     )
#   ) %>%
#   ungroup
#
# # several options
# test_multi_wide <- test_multi %>%
#   pivot_wider(id_cols = date,
#               names_from = c(variable_name, site_code),
#               values_from = metric)
#
# test_multi_wide <- test_multi %>%
#   pivot_wider(id_cols = c(site_name, site_code, date),
#               names_from = variable_name,
#               values_from = metric)
#
# test_multi_wide <- test_multi %>%
#   pivot_wider(id_cols = date,
#               names_from = c(variable_name, site_code),
#               values_from = metric)
#
# test_multi <- flow_long %>%
#   filter(variable_name == "stream_discharge_mld") %>%
#   group_by(site_name, site_code) %>%
#   do(
#     calculate(
#       value = .$value,
#       date = .$date_formatted,
#       resolution = survey(season = 9:11),
#       na.rm = TRUE
#     )
#   ) %>%
#   ungroup
#
#
# # not working now but will work in dplyr v.1.0.0
# flow_long %>%
#   filter(variable_name == "stream_discharge_mld") %>%
#   group_by(site_name) %>%
#   summarise(
#     spring_flow = calculate(value, date_formatted, survey(season = 9:11), na.rm = TRUE)$metric,
#     summer_flow = calculate(value, date_formatted, survey(season = 12:3), na.rm = TRUE)$metric
#   )
#
