#' Process KOF Vintage Data into Time Series List (tsl)
#'
#' Fetches vintage data from the kofdata R package, filters for the most recent vintage of specific
#' indicators, and cleans the resulting names.
#'
#' @importFrom kofdata get_collection
#' @param key A character string of the KOF set key to fetch.
#' @param ids A character vector of indicator IDs to extract (e.g., c("coincident", "leading")).
#'
#' @return A named list of ts objects (tsl) with cleaned names in the key.id format
#' @export
process_data <- function(key, ids = NULL) {
  # fetch initial tsl
  tsl <- list()

  for (i in ids){
    tsk <- paste0(key,".", i)
    # print(tsk)
    ts <- kofdata::get_time_series(tsk)
    name <- names(ts)
    # print(names)
    tsl[name] <- ts
  }

  # print(tsl)

  # Write each time series to series.csv in the corresponding directory
  lapply(names(tsl), function(k) {
    ts_obj <- tsl[[k]]

    # Convert ts object to data frame with time and value columns
    # Extract time index and values from the ts object
    values <- as.numeric(ts_obj)

    ts_time <- time(ts_obj)

    freq <- frequency(ts_obj)

    # For monthly data (freq = 12)
    if (freq == 12) {
      years  <- floor(ts_time)
      months <- round((ts_time - years) * 12) + 1
      ts_dates <- as.Date(sprintf("%d-%02d-01", years, months))
    } else {
      stop("Unsupported frequency")
    }
  
    # Create data frame in the format of the input file
    ts_df <- data.frame(
      time = as.Date(ts_dates),
      value = values
    )

    # k is last suffix
    suffix <- sub(".*\\.", "", k)

    # Create path to write file
    output_path <- file.path(".", "data-raw", "csv", paste0(suffix, ".csv"))

    # Write to CSV without row names
    write.csv(ts_df, file = output_path, row.names = FALSE)
    message(sprintf("latest vintage of %s: %s written to %s", key, k, output_path))

  })
  
}

# process_data("ch.kof.globalbaro", ids = c("coincident", "leading"))

