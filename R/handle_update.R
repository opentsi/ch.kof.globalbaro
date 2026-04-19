#' Handle Data Update
#'
#' Orchestrates the update process: checks if update is needed,
#' processes data, writes output, and stores the new checksum.
#'
#' @importFrom opentimeseries write_open_ts is_update_needed update_checksum
#' @importFrom digest digest
#' @export
handle_update <- function() {

  checksum_input <- generate_checksum_input()

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
    (process_data("ch.kof.globalbaro", ids = c("coincident", "leading")))
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
#' @importFrom kofdata get_time_series
generate_checksum_input <- function(){
  # since we dont know the publication date, we just fetch the most 
  # granual data we can find
  # if coincident is updated, so is leading
  global <- get_time_series("ch.kof.globalbaro.coincident")[[1]]
  return(global)
}
