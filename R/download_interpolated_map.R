#' Download an interpolated raster map collected by a specific sensor on a selected acquisition date and variable
#'
#' Download a ZIP file containing interpolated raster data for a specific sensor/date/variable.
#' @importFrom httr GET write_disk add_headers status_code content
#' @importFrom utils unzip
#' @param id_sensor The id sensor that you want to analyze
#' @param date The date of the data acquired (string)
#' @param variable Can be count, height, width, area
#' @param filename Path + filename where the ZIP will be saved
#' @param token The Web JSON Token API (Bearer token)
#' @return A list with:
#'   - status_code
#'   - zip_file (absolute path or NULL)
#'   - raw_response (text in case of error)
#' @export
download_interpolated_map <- function(id_sensor,
                                      date,
                                      variable,
                                      filename,
                                      token) {

  # Ensure .zip extension
  if (!grepl("\\.zip$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".zip")
  }

  # Ensure directory exists
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)

  # API endpoint
  domain   <- "https://backend.fruitkount.com/"
  endpoint <- "sensors/download-interpolated-map/"
  api_url  <- paste0(domain, endpoint, id_sensor, "/", date, "/", variable)

  # Perform GET request (binary download)
  response <- GET(
    api_url,
    add_headers(Authorization = paste("Bearer", token)),
    write_disk(filename, overwrite = TRUE)
  )

  status <- status_code(response)
  message(sprintf("Status Code: %s", status))

  result <- list(
    status_code  = status,
    zip_file     = NULL,
    raw_response = NULL
  )

  # Handle error responses
  if (status != 200) {
    txt <- try(content(response, as = "text", encoding = "UTF-8"), silent = TRUE)
    result$raw_response <- if (inherits(txt, "try-error")) NULL else txt
    return(result)
  }

  # Validate ZIP integrity
  zip_ok <- TRUE
  tryCatch({
    unzip(filename, list = TRUE)
  }, error = function(e) {
    zip_ok <<- FALSE
  })

  if (!zip_ok) {
    warning("Downloaded file is not a valid ZIP archive.")
    return(result)
  }

  abs_path <- normalizePath(filename)
  message(sprintf("ZIP file saved to: %s", abs_path))

  result$zip_file <- abs_path
  return(result)
}
