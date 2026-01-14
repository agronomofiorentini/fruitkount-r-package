#' Download a ZIP file containing the prescription map in wgs84 related to a specific sensor on a selected acquisition date
#'
#' Download a ZIP file containing the prescription map in wgs84 related to a specific sensor on a selected acquisition date
#' @importFrom httr GET write_disk add_headers status_code content
#' @importFrom utils unzip
#' @param id_sensor The id sensor that you want to analyze
#' @param date The date of the data acquired (string)
#' @param automatic  Must be set as True or False. If True means that number and position of zone management are setted automatically; If False means that number of zone management are setted by the user by definig the zone parameter
#' @param zone if AutoPrescription is setted False, here you can specify the number of zone
#' @param fertilizer set the amount of fertilizer to provide on avarage to the field
#' @param strategy Can be set as highwherehigh or highwherelow. If highwherehigh means provide an higher amout of fertilizer where the production level is higher. If highwherelow means provide an higher amout of fertilizer where the production levels is lower
#' @param percentage Set the difference percentange of the dose that you want to apply between the zones. you can set a value between 0 to 100. if you set 0 an automatic process will define the difference of the dose based on production levels.
#' @param filename Path + filename where the ZIP will be saved
#' @param token The Web JSON Token API (Bearer token)
#' @return Download a ZIP file containing the prescription map in wgs84 related to a specific sensor on a selected acquisition date
#' @export
download_prescription_map <- function(id_sensor,
                                      date,
                                      automatic,
                                      zone,
                                      fertilizer,
                                      strategy,
                                      percentage,
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
  endpoint <- "sensors/download-prescription-map/"
  api_url  <- paste0(domain, endpoint, id_sensor, "/", date, "/", automatic, "/", zone, "/", fertilizer, "/", strategy, "/", percentage)

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
