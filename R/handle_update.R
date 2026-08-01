#' Handle Data Update
#'
#' Orchestrates the update process: checks if update is needed,
#' processes data, writes output, and stores the new checksum.
#'
#' @importFrom opentimeseries is_update_needed update_checksum
#' @importFrom digest digest
#' @param key API key for the KOF Time Series Database.
#' @export
handle_update <- function(key = key) {

  checksum_input <- generate_checksum_input(key = key)

  print(is_update_needed(checksum_input))
  if (!is_update_needed(checksum_input)) {
    message("No update needed, series up-to-date.")
    return(invisible(NULL))
  }
  # Edit R/process_data.R and enter a function
  # that returns the most recent version of a time series
  # from its original provider
  # Store checksum after successful update

  new_hash <- digest::digest(checksum_input, algo = "sha256")

  # print(checksum)
  upd <- update_checksum(new_hash)
  if(upd){
    process_data(key = key, ids = c("coincident", "leading"))
  } else {
    message("Checksum initialized. Data untouched.")
  }
  message("Update complete, checksum stored.")
}


#' User Written Function to Create Input for Checksum Comparison
#'
#' This function generates input for computation of checksums to identify
#' outdated content. Good inputs are either publication dates extracted from
#' official publisher sites or APIs or any single time series from a database,
#' because opentsi definition all time series of the same dataset must
#' have the same publication date.
#' @importFrom tsdbapi set_config read_ts
#' @param key API key for the KOF Time Series Database.
generate_checksum_input <- function(key = key){
  set_config(api_key = key)
  # since we dont know the publication date, we just fetch the most
  # granular data we can find
  # if coincident is updated, so is leading
  global <- read_ts(ts_keys = "ch.kof.globalbaro.coincident")[[1]]
  return(global)
}
