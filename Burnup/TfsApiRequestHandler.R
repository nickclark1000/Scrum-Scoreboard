library(httr)
library(jsonlite)


TfsApiGet <- function(path) {
  url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path=path)
  resp<-GET(url, authenticate("TEN\\U6033371","Monday.123456",type="ntlm"))
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"))
}

TfsApiPost <- function(path, query) {
  url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
  resp<-POST(url, authenticate("TEN\\U6033371","Monday.123456",type = "ntlm"), body = list(query = query), encode = "json")
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  jsonlite::fromJSON(content(resp, "text"))
}


