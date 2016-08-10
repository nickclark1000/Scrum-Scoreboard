tfs.collection <- ''
tfs.project <- ''
tfs.team <- ''
team.default.area.path <- ''


GetTfsCollections <- function() {
  
}

SetTfsCollection <- function() {
 # tfs.collection <<- 'FinancialReportingCollection'
  tfs.collection <<- 'DTSCollection'
}

GetTfsProjects <- function(tfs.collection) {
  
}

SetTfsProject <- function() {
 # tfs.project <<- 'FinancialReportingProject'
  tfs.project <<- 'DesignGalleryProject'
}

GetTfsTeams <- function(tfs.project) {
  
}

SetTfsTeam <- function() {
 # tfs.team <<- 'FinancialReporting Team'
  tfs.team <<- 'VisualDesignerPrototypeTeam'
}

GetDefaultTeamAreaPath <- function() {
  url <- paste("/tfs/",tfs.collection,"/",tfs.project,"/",tfs.team,"/_apis/Work/TeamSettings/TeamFieldValues",sep="")
  cat("Request URL:", url, "\n")
  team.default.area.path <<- TfsApiGet(url)$defaultValue
}
