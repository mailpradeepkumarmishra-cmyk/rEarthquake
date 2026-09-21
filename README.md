# rEarthquake

<!-- badges: start -->
<!-- Travis CI badge will be added after Travis is configured -->
<!-- badges: end -->

## Overview

`rEarthquake` is an R package for working with NOAA significant
earthquake data.

The package provides functions for cleaning earthquake data,
visualizing earthquake activity over time, creating earthquake
labels, and mapping earthquake epicenters.

## Main functions

The package provides six exported functions:

- `eq_clean_data()` — cleans NOAA earthquake data and creates
  a `DATE` variable.
- `eq_location_clean()` — cleans earthquake location names.
- `geom_timeline()` — creates an earthquake timeline using
  `ggplot2`.
- `geom_timeline_label()` — adds labels to selected earthquakes
  on a timeline.
- `eq_map()` — creates an interactive earthquake map using
  `leaflet`.
- `eq_create_label()` — creates HTML labels for earthquake
  map popups.

## Installation

The package can be installed from GitHub using:

```r
# install.packages("devtools")
devtools::install_github(
  "mailpradeepkumarmishra-cmyk/rEarthquake"
)
```

## Example

The following example reads the NOAA significant earthquake data
included with the package and cleans it:

```r
library(rEarthquake)

data <- readr::read_delim(
  system.file(
    "extdata",
    "signif.txt",
    package = "rEarthquake"
  ),
  delim = "\t",
  show_col_types = FALSE
)

clean_data <- eq_clean_data(data)

head(clean_data)
```

An earthquake timeline can then be created using:

```r
library(ggplot2)

ggplot(
  clean_data,
  aes(
    x = DATE,
    y = COUNTRY
  )
) +
  geom_timeline()
```

An interactive earthquake map can be created using:

```r
eq_map(
  clean_data,
  annot_col = "LOCATION_NAME"
)
```

## Data

The package uses the NOAA Significant Earthquake Database
data supplied with the course project.

The included historical data file is located in:

```text
inst/extdata/signif.txt
```

## Vignette

A detailed introduction to the package is provided in the
package vignette:

**Working with NOAA Significant Earthquake Data**

The vignette demonstrates the complete workflow from data
cleaning through timeline visualization and interactive mapping.

## Testing

The package uses the `testthat` package for unit testing.

Tests can be run with:

```r
devtools::test()
```

The package can also be checked using:

```r
devtools::check()
```

## Author

Pradeep Mishra