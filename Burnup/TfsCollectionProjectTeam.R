tfs.collection <- ''
tfs.project <- ''
tfs.team <- ''
team.default.area.path <- ''


GetTfsCollections <- function() {
  
}

SetTfsCollection <- function() {
 # tfs.collection <<- 'FinancialReportingCollection'
 # tfs.collection <<- 'DTSCollection'
}

GetTfsProjects <- function(tfs.collection) {
  url <- paste("/tfs/", URLencode(tfs.collection), "/_apis/projects", sep="")
  projects <- TfsApiGet(url)
  return(projects)
}

SetTfsProject <- function() {
#  tfs.project <<- 'FinancialReportingProject'
 # tfs.project <<- 'DesignGalleryProject'
}

GetTfsTeams <- function(tfs.collection, tfs.project) {
  url <- paste("/tfs/", URLencode(tfs.collection), "/_apis/projects/", URLencode(tfs.project), "/teams", sep="")
  teams <- TfsApiGet(url)
  return(teams)
}

SetTfsTeam <- function() {
 # tfs.team <<- 'FinancialReporting Team'
 # tfs.team <<- 'VisualDesignerPrototypeTeam'
 # tfs.team <<- 'BackEndTeam'
}

GetDefaultTeamAreaPath <- function() {
  url <- paste("/tfs/", URLencode(tfs.collection), "/", URLencode(tfs.project),"/",URLencode(tfs.team), "/_apis/Work/TeamSettings/TeamFieldValues", sep="")
  team.default.area.path <<- TfsApiGet(url)$defaultValue
}
