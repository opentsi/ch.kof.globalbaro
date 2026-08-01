#' Process KOF Global Barometer Data into Time Series List (tsl)
#'
#' Fetches the most recent vintage of specific indicators from the KOF Time
#' Series Database and writes each to its key.csv
#'
#' @importFrom tsdbapi read_ts set_config
#' @param key API key for the KOF Time Series Database.
#' @param ids A character vector of indicator IDs to extract (e.g., c("coincident", "leading")).
#'
#' @return Invisibly returns a character vector of output file paths.
#' @export
process_data <- function(key, ids = NULL) {
  tsdbapi::set_config(api_key = key)

  ts_keys <- paste0("ch.kof.globalbaro.", ids)
  tsl <- tsdbapi::read_ts(ts_keys)

  out_paths <- lapply(names(tsl), function(k) {
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
      stop(sprintf("Unsupported frequency: %d", freq))
    }

    # Create data frame in the format of the input file
    ts_df <- data.frame(
      time = as.Date(ts_dates),
      value = values
    )

    # remove prefix so it matches with current data
    suffix <- sub("^ch\\.kof\\.globalbaro\\.", "", k)

    # Create path to write file
    output_path <- file.path(".", "data-raw", "csv", paste0(suffix, ".csv"))

    # Write to CSV without row names
    write.csv(ts_df, file = output_path, row.names = FALSE, quote = FALSE)
    message(sprintf("Written: %s", output_path))
    output_path
  })

  invisible(unlist(out_paths))
}
