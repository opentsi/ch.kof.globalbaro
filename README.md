
# ch.kof.globalbaro

The ch.kof.globalbaro package provides versioned time series data
and their meta information for scientific research.
In addition, the package contains the
extract-transform-load (ETL) functionality that
sources the data from its original provider.

## Browse Time Series Data

## Basic Usage Via opentimeseries


```r
ts <- read_open_ts(
  "ch.kof.globalbaro",
  archive= "opentsi" # or your organisation
)

ts
```

## The ch.kof.globalbaro Data R Package


