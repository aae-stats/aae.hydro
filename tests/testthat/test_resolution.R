context("resolution")

test_that("resolution works", {

  nyear <- years(5)
  start <- dmy("01012010")
  flow_sim <- data.frame(
    date = seq(start, start + nyear, by = 1)
  )
  flow_sim$value <- exp(rnorm(nrow(flow_sim)))

})

