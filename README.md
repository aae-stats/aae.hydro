## We have access to lots of data

We have so much data. But it's hard to navigate and work with everything, especially when we want to bring together data from different places. The `aae.hydro` R package is an attempt to simplify this process. The main aim of the package is to provide an interface to publicly available streamflow data. Down the track, the goal is to link `aae.hydro` with other packages to access data collected, curated, or regularly used by AAE staff.

## Installing the package

You can install the `aae.hydro` package from GitHub. Although the package doesn't publicly share any AAE data sets, it will not be submitted to CRAN given the limited set of users. To install from GitHub, you'll need to install the `remotes` R package and use the following lines of code:

```{r install-packages, eval = FALSE}
# install the remotes package if not already installed
install.packages("remotes")

# install the aae.hydro package from GitHub
remotes::install_github("aae.stats/aae.hydro")
```

Once completed, you should be able to load the `aae.hydro` package with `library(aae.hydro)`.

## Accessing flow data

Getting data from R looks a bit like this:

```{r}
# load the aae.hydro package
library(aae.hydro)

# Download data for five sites, based on their gauge numbers:
#   - 405232 (Goulburn @ McCoys Bridge)
#   - 406201 (Campaspe @ Barnadown)
#   - 406202 (Campaspe @ Rochester D/S Waranga Western Ch Syphn)
#   - 406276 (Campaspe @ Fehrings Lane)
#   - 406278 (Campaspe @ Backhaus Road)
# `start` and `end` bound the dates we want, in any reasonable format (yyyy-mm-dd, dd-mm-yy, etc.).
# `variables` define the variables we want, with somewhat flexible names (e.g. discharge, streamflow,
#   flow, temp, temperature, water_temperature). Can define variables by WMIS code if you know it.
# `state` specifies whether we want data from Victoria, New South Wales, or Queensland. Will be
#   matched partially and is not case-sensitive (e.g. "VIC", "Victoria", and "vic" are equivalent).
flow_data <- fetch_hydro(
  sites = c("405232", "406201", "406202", "406276", "406278"),
  start = "2004-01-01",
  end = "2020-04-21",
  variables = c("flow", "temp", "depth"),
  state = "vic"
)
```

## Summarising flow data

Once we have access to flow data, the next challenge is to convert flow data into relevant metrics or summary statistics. The `aae.hydro` package aims to make this easy with a single function: `calculate`. The `calculate` function takes data on any variable, along with dates of observations, and summarises this at a resolution of your choosing. 

```{r}
# load some extra R packages we'll need
library(dplyr)
library(tidyr)

# convert flow data to a wider format
flow_wide <- flow_data %>% pivot_wider(
  id_cols = c(date_formatted, site_name, site_code),
  names_from = variable_name,
  values_from = c(value, units, quality_code)
)

# pull out the data for a single site (Goulburn @ McCoys Bridge)
#    (we can avoid this step, see the Calculating metrics from streamflow data vignette)
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
```

The vignettes contain several more detailed examples.

Please leave feedback, bug reports or feature requests at the GitHub [issues page](https://github.com/aae-stats/aae.hydro/issues). 

[![build status](https://travis-ci.org/aae-stats/aae.hydro.svg?branch=master)](https://travis-ci.org/aae-stats/aae.hydro) [![codecov.io](https://codecov.io/github/aae-stats/aae.hydro/coverage.svg?branch=master)](https://codecov.io/github/aae-stats/aae.hydro?branch=master) [![license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
