#' Get the descriptive statiscs related to one sensor connected to your account
#'
#' Get the descriptive statiscs related to one sensor connected to your account
#' @importFrom httr GET content content_type_json add_headers
#' @importFrom jsonlite fromJSON
#' @param id_sensor The id sensor that you want to analyze
#' @param variable Can be "count", "height", "width", "area"
#' @param token The Web JSON Token API
#' @return Returns a list of the statistics
#' @export
get_statistics<-function(id_sensor,
                         variable,
                         token) {

  # Set the domain
  domain <- "https://backend.fruitkount.com/"

  # Set the endpoint
  endpoint <- "sensors/statistics/"

  # Create the API URL
  api_url <- paste0(domain, endpoint, id_sensor, "/", variable)

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

