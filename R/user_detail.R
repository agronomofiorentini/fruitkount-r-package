#' Get the User Details
#'
#' This function allows you to get the API User Details
#' @importFrom httr POST content content_type_json
#' @importFrom jsonlite fromJSON
#' @param token Email of the user
#' @return The dataframe of the user detail
#' @export
user_detail<-function(token) {

  # Set the domain
  domain <- "https://backend.fruitkount.com/"

  # Set the endpoint
  endpoint <- "authentication/user-detail/"

  # Create the API URL
  api_url <- paste0(domain, endpoint)

  # Create the JSON body with email and password
  json_body <- list(email = email,
                    password = password)

  # Make the POST request
  response <- POST(
    api_url,
    body = json_body,
    encode = "json", # Ensures the body is sent as JSON
    content_type_json() # Sets the Content-Type header to application/json
  )

  # Check the status code of the response
  print("Status Code")
  print(response$status_code)

  # Check the content
  cont <- content(response, as = "text", type = "application/json", encoding="UTF-8")
  cont<-fromJSON(cont) %>% as.data.frame

  print("The User Details are:")
  return(cont)
  # The following is the Bearer code that you have to use for each request
  # print(cont$token)
}

