#' Retrieve all data collected by a specific sensor on a selected acquisition date.
#'
#' Retrieve all data collected by a specific sensor on a selected acquisition date.
#' @importFrom httr GET content content_type_json add_headers
#' @importFrom jsonlite fromJSON
#' @param id_sensor The id sensor that you want to analyze
#' @param date The date of the data acquired
#' @param token The Web JSON Token API
#' @return Returns a Dataframe with the sensor data related to a specific sensor on a selected acquisition date.
#' @export
get_sensor_data_by_date<-function(id_sensor,
                                  date,
                                  token) {

  # Set the domain
  domain <- "https://backend.fruitkount.com/"

  # Set the endpoint
  endpoint <- "sensors/sensor-data-by-date/"

  # Create the API URL
  api_url <- paste0(domain, endpoint, id_sensor, "/", date)

  # Make the POST request
  response <- GET(
    api_url,
    add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    encode = "json", # Ensures the body is sent as JSON
    content_type_json() # Sets the Content-Type header to application/json
  )

  # Check the status code of the response
  print("Status Code")
  print(response$status_code)

  # Check the content
  cont <- content(response, as = "text", type = "application/json", encoding="UTF-8")
  cont<-fromJSON(cont) %>% as.data.frame

  return(cont)
  # The following is the Bearer code that you have to use for each request
  # print(cont$token)
}

