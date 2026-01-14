#' Get the API Token to interact with the FruitKount API Service
#'
#' This function allows you to get the API Token to interact with the FruitKount API Service
#' @param email Email of the user
#' @param password Password of the user
#' @return The API Token String
#' @export
get_token<-function(email,
                    password) {

  # Set the domain
  domain <- "https://backend.fruitkount.com/"

  # Set the endpoint
  endpoint <- "authentication/token/"

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

  return(cont$token)
  # The following is the Bearer code that you have to use for each request
  # print(cont$token)
}

