source("TfsApiRequestHandler.R")
source("TfsCollectionProjectTeam.R")


GetCurrentRelease <- function() {
  # Computes the current release based on today's date.
  #
  # Args:
  #   None
  #
  # Returns:
  #   The current release as a list.
  releases <- TfsApiGet("/tfs/FinancialReportingCollection/FinancialReportingProject/_apis/wit/classificationNodes/iterations?$depth=1")
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
  sprints <- TfsApiGet(url)
  return(sprints$children)
}

CreateReleaseSummaryTable <- function() {
  current.release <- GetCurrentRelease()
  current.release
  sprints <- GetReleaseSprints(current.release)
  release.table <- data.frame(RELEASE_ITERATION_ID=current.release$id, RELEASE_NAME=current.release$name, SPRINT_ITERATION_ID=sprints$id, SPRINT_NAME=sprints$name, SPRINT=c(1:length(sprints)), START_DATE=as.Date(sprints$attributes$startDate), END_DATE=as.Date(sprints$attributes$finishDate))
  return(release.table)
}
current.release <- GetCurrentRelease()
release.summary <- CreateReleaseSummaryTable()

GetReleaseWorkItems <- function(iteration.ids) {
  #Returns list of work item IDs.
  query <- paste("Select [System.Id] 
                 From WorkItems 
                 Where [System.WorkItemType] in ('Product Backlog Item', 'Bug', 'Work Order') 
                 AND [System.IterationId] in (",iteration.ids,") 
                 AND [System.State] <> 'Removed'")
  
  work.items<-TfsApiPost("/tfs/FinancialReportingCollection/FinancialReportingProject/_apis/wit/wiql?api-version=1.0",query)
  return(work.items)
}

iteration.ids <- paste(c(current.release$id,as.character(release.summary$SPRINT_ITERATION_ID)), collapse=",")
work.items<-GetReleaseWorkItems(iteration.ids)
work.item.list <- paste(as.character(work.items$workItems$id), collapse=",")
return.fields <- 'System.Id, 
                  System.Title, 
                  System.WorkItemType, 
                  System.IterationPath, 
                  System.IterationId, 
                  System.State,
                  System.CreatedDate,
                  Microsoft.VSTS.Scheduling.Effort,
                  Microsoft.VSTS.Common.Severity,
                  TR.Elite.BugType,
                  Microsoft.VSTS.Common.ClosedDate,
                  System.AreaPath'
url <- paste("/tfs/FinancialReportingCollection/_apis/wit/workitems?ids=",work.item.list,"&fields=",gsub("[\n ]","",return.fields),"&api-version=1.0",sep="")
import.table <- TfsApiGet(url)


release.summary

#TfsApiGet("/tfs/FinancialReportingCollection/FinancialReportingProject/_apis/wit/queries/Shared%20Queries/2.1%20All%20Work%20Items?$expand=wiql")

