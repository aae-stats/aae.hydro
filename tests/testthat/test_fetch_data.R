# # basic tests
#
# # choose two sites, download actual data for a short period, check we get same data
# #   check dims of output when include_missing = T/F
# #   check warnings printed
# #   check check_quality
#
# # one variable
# test <- fetch_data(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
#                    variables = "streamflow")
#
# # two variables
# test <- fetch_data(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
#                    variables = c("streamflow", "watertemp"))
#
# # one variable repeated twice
# test <- fetch_data(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
#                    variables = c("streamflow", "flow"))
#
# # one variable one site
# test <- fetch_data(sites = c(409202), start = "2019-04-02", end = "2019-04-24",
#                    variables = c("streamflow"))
#
# # three variables two sites
# test <- fetch_data(sites = c(409202, 409204), start = "2019-04-02", end = "2019-04-24",
#                    variables = c("depth", "streamflow", "temp"))
#
# # different date ranges: overlapping, unavailable, etc. (one var missing, others available)
#
# # test where one site has data and another does not
#
# # test gapfill when sites have inconsistent data
#
# # test errors/warnings
#
#
# # one site has no temperature data, what does it do?
# test <- fetch_data(sites = c(409202, 409204, 405204), start = "2019-04-02", end = "2019-04-24",
#                    variables = c("depth", "streamflow", "temp"), include_missing = TRUE,
#                    data_source = "A")
#
# # multiple data sources (error)
#
