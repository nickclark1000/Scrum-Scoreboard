###load dplyr for data wrangling
library(dplyr)
###load TTR for calculating moving averages
library(TTR)

source("C:/Users/u6033371/Documents/Agile Resources/AgileData/Burnup/TfsApiRequestHandler.R")
source("C:/Users/u6033371/Documents/Agile Resources/AgileData/Burnup/TfsCollectionProjectTeam.R")
source("C:/Users/u6033371/Documents/Agile Resources/AgileData/Burnup/TfsReleaseSummary.R")

SetTfsCollection()
SetTfsProject()
SetTfsTeam()
default.area.path <- GetDefaultTeamAreaPath()

current.release <- GetCurrentRelease()
current.major.release <- current.release$current.major.release
current.minor.release <- current.release$current.minor.release
if(is.null(current.minor.release)) {
  current.release <- current.release$current.major.release
} else {
  current.release <- current.release$current.minor.release
}

release.summary.df <- data.frame(RELEASE_ITERATION_ID=current.release$id, RELEASE_NAME=current.release$name, SPRINT_ITERATION_ID=current.release$children[[1]]$id, SPRINT_NAME=current.release$children[[1]]$name, SPRINT_INDEX=c(1:nrow(current.release$children[[1]])), START_DATE=as.Date(current.release$children[[1]]$attributes$startDate), END_DATE=as.Date(current.release$children[[1]]$attributes$finishDate))

today <- format(Sys.Date())
for(i in 1 : nrow(release.summary.df)) {
  if(release.summary.df$START_DATE[i] <= today && today <= release.summary.df$END_DATE[i])
    CURRENT_SPRINT_INDEX <<- release.summary.df$SPRINT_INDEX[i]
}
release.summary.df <- subset(release.summary.df, SPRINT_INDEX < CURRENT_SPRINT_INDEX)

iteration.ids <- paste(c(current.release$id,as.character(release.summary.df$SPRINT_ITERATION_ID)), collapse=",")
work.item.ids <- GetReleaseWorkItemIds(iteration.ids)
#work.item.ids$workItems <- head(work.item.ids$workItems, 199)


work.item.df <- GetReleaseWorkItems(work.item.ids$workItems$id)

work.item.df$System.CreatedDate <- format(as.Date(work.item.df$System.CreatedDate), "%d/%m/%Y")
work.item.df$Microsoft.VSTS.Common.ClosedDate <- format(as.Date(work.item.df$Microsoft.VSTS.Common.ClosedDate), "%d/%m/%Y")

# Add Velocity
release.summary.df <- inner_join(release.summary.df, rename(work.item.df %>% group_by(System.IterationId) %>% summarise(VELOCITY=sum(Microsoft.VSTS.Scheduling.Effort, na.rm=TRUE)), SPRINT_ITERATION_ID=System.IterationId), by="SPRINT_ITERATION_ID")

###Total work done measured at the END of each sprint
release.summary.df$COMPLETED_RELEASE_POINTS <- cumsum(release.summary.df$VELOCITY)

###Moving average calculations
if(nrow(release.summary.df)>4){
  release.summary.df$VELOCITY_SMA_5 <- SMA(release.summary.df$VELOCITY, 5)
  release.summary.df$VELOCITY_WMA_5 <- WMA(release.summary.df$VELOCITY, 5)
} else {
  release.summary.df$VELOCITY_SMA_5 <- SMA(release.summary.df$VELOCITY, nrow(release.summary.df)-1)
  release.summary.df$VELOCITY_WMA_5 <- WMA(release.summary.df$VELOCITY, nrow(release.summary.df)-1)
}

###Defects summary
release.summary.df <- left_join(release.summary.df, 
                                 rename(
                                   subset(work.item.df, System.WorkItemType=="Bug") %>%
                                   group_by(System.IterationId) %>%
                                   summarise(DEFECTS_COMPLETED_COUNT=n(),
                                             DEFECTS_COMPLETED_POINTS=sum(Microsoft.VSTS.Scheduling.Effort, na.rm=TRUE))
                                   , SPRINT_ITERATION_ID=System.IterationId), 
                                 by="SPRINT_ITERATION_ID")

###PBI summary
release.summary.df <- left_join(release.summary.df, 
                                 rename(
                                   subset(work.item.df, System.WorkItemType=="Product Backlog Item") 
                                   %>% group_by(System.IterationId) 
                                   %>% summarise(PBIS_COMPLETED_COUNT=n(),
                                                 PBIS_COMPLETED_POINTS=sum(Microsoft.VSTS.Scheduling.Effort, na.rm=TRUE))
                                   , SPRINT_ITERATION_ID=System.IterationId), 
                                 by="SPRINT_ITERATION_ID")


INPUT_DATA$TOTAL_RELEASE_POINTS = rowSums(cbind(INPUT_DATA$TOTAL_RELEASE_PBI_POINTS, INPUT_DATA$TOTAL_RELEASE_DEFECT_POINTS, INPUT_DATA$TOTAL_RELEASE_WORKORDER_POINTS), na.rm=TRUE)
release.summary.df <- inner_join(release.summary.df, INPUT_DATA, by="SPRINT_INDEX")