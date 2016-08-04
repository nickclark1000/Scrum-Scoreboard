library(httr)
library(jsonlite)


tfs_api <- function(path) {
  url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path=path)
  resp<-GET(url, authenticate("TEN\\U6033371","Monday.123456",type="ntlm"))
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"))
}

tfs_api_post <- function(path, query) {
  url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
  resp<-POST(url, authenticate("TEN\\U6033371","Monday.123456",type = "ntlm"), body = list(query = query), encode = "json")
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  jsonlite::fromJSON(content(resp, "text"))
}


GetCurrentRelease <- function() {
  # Computes the current release based on today's date.
  #
  # Args:
  #   None
  #
  # Returns:
  #   The current release as a list.
  releases <- tfs_api("/tfs/FinancialReportingCollection/FinancialReportingProject/_apis/wit/classificationNodes/iterations?$depth=1")
  length(releases$children)
  today <- format(Sys.Date())
  for (i in 1:nrow(releases$children)){
    child <- releases$children[i, ]
    if (!is.na(child$attributes$startDate) || !is.na(child$attributes$finishDate)) {
      if (today > child$attributes$startDate && today < child$attributes$finishDate)
        current.release <- child
    } else {
      cat("Release:", child$name, "is missing start or finish dates.\n")
    }
  }
  cat("Current Release:",current.release$name,"\n")
  return(current.release)
}


GetReleaseSprints <- function(release) {
  # Returns a list of sprints in the release.
  #
  # Args:
  #   release: Release list.
  #
  # Returns:
  #   sprints: List of children sprints in the release.
  url <- paste("/tfs/FinancialReportingCollection/FinancialReportingProject/_apis/wit/classificationNodes/iterations/",URLencode(release$name),"?$depth=1", sep="")
  cat("Request URL:", url, "\n")
  sprints <- tfs_api(url)
  return(sprints$children)
}

CreateReleaseSummaryTable <- function() {
  current.release <- GetCurrentRelease()
  current.release
  sprints <- GetReleaseSprints(current.release)
  release.table <- data.frame(RELEASE_ITERATION_ID=current.release$id, RELEASE_NAME=current.release$name, SPRINT_ITERATION_ID=sprints$id, SPRINT_NAME=sprints$name, SPRINT=c(1:length(sprints)), START_DATE=as.Date(sprints$attributes$startDate), END_DATE=as.Date(sprints$attributes$finishDate))
  return(release.table)
}
CreateReleaseSummaryTable()

GetReleaseWorkItems <- function() {
  #Returns list of work item IDs.
  query <- "Select [System.Id]
            From WorkItems
            Where [Iteration Id] = 82"
           
  work.items<-tfs_api_post("/tfs/FinancialReportingCollection/FinancialReportingProject/_apis/wit/wiql?api-version=1.0",query)
  return(work.items)
}
work.items<-GetReleaseWorkItems()
work.item.list <- paste(as.character(work.items$workItems$id), collapse=",")
url<-paste("/tfs/FinancialReportingCollection/_apis/wit/workitems?ids=",work.item.list,"&api-version=1.0",sep="")
b<-tfs_api(url)
View(b$value)