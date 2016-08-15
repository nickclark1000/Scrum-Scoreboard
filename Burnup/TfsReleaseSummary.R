GetCurrentRelease <- function() {
  # Computes the current release based on today's date.
  #
  # Args:
  #   None
  #
  # Returns:
  #   current.release: The current release as a list.
  if(tfs.collection == "" || tfs.project == "")
    stop("TFS Collection or Project is not defined", call. = FALSE)
  url <- paste("/tfs/",tfs.collection,"/",tfs.project,"/_apis/wit/classificationNodes/iterations?$depth=3",sep="")
  releases <- TfsApiGet(url)
  today <- format(Sys.Date())
  
  for (i in 1:nrow(releases$children)){
    child <- releases$children[i, ]
    if (!is.na(child$attributes$startDate) || !is.na(child$attributes$finishDate)) {
      cat("Release:",child$name,", Start:",child$attributes$startDate,", Finish:",child$attributes$finishDate,"\n")
      if (today > child$attributes$startDate && today < child$attributes$finishDate) {
        current.major.release <<- child
        target.release.date <<- child$attributes$finishDate
      }
    } else {
      cat("Release:", child$name, "is missing start or finish dates.\n")
    }
  }
  if(!exists("current.major.release"))
    stop("Today's date does not fall between any release start and end dates")
  
  cat("Current Major Release:",current.major.release$name,"\n")
  
  if(is.element('TRUE',current.major.release$children[[1]]$hasChildren)) {
    for(i in 1:nrow(current.major.release$children[[1]])) {
      grandchild <- current.major.release$children[[1]][i,]
      if(grandchild$hasChildren) {
        if (!is.na(grandchild$attributes$startDate) || !is.na(grandchild$attributes$finishDate)) {
          cat("Release:",grandchild$name,", Start:",grandchild$attributes$startDate,", Finish:",grandchild$attributes$finishDate,"\n")
          if (today > grandchild$attributes$startDate && today < grandchild$attributes$finishDate) {
            current.minor.release <<- grandchild
            target.release.date <<- grandchild$attributes$finishDate
            cat("Current Minor Release:",current.minor.release$name,"\n")
          }
        } else {
          cat("Release:", grandchild$name, "is missing start or finish dates.\n")
        }
      }
    }
  } else {
    cat("No minor releases to define \n")
    current.minor.release <- NULL
  }
  return(list(current.minor.release=current.minor.release,current.major.release=current.major.release))
}


GetReleaseSprints <- function(release) {
  # Returns a list of sprints in the release.
  #
  # Args:
  #   release: Release list.
  #
  # Returns:
  #   sprints$children: List of child sprints in the release.
  if(tfs.collection == "" || tfs.project == "")
    stop("TFS Collection or Project is not defined", call. = FALSE)
  url <- paste("/tfs/",tfs.collection,"/",tfs.project,"/_apis/wit/classificationNodes/iterations/",URLencode(release$name),"?$depth=1", sep="")
  sprints <- TfsApiGet(url)
  return(sprints$children)
}


GetReleaseWorkItemIds <- function(iteration.ids, date = format(Sys.Date())) {
  #Returns list of work item IDs.
  query <- paste("Select [System.Id] 
                 From WorkItems 
                 Where [System.WorkItemType] in ('Product Backlog Item', 'Bug', 'Work Order') 
                 AND [System.IterationId] in (", iteration.ids,")
                 AND [System.AreaPath] under '", team.default.area.path,"'
                 AND [System.State] <> 'Removed'
                 ASOF '", date, "'", sep="")
  cat("Request Query:", query, "\n")
  url <- paste("/tfs/",tfs.collection,"/",tfs.project,"/_apis/wit/wiql?api-version=1.0",sep="")
  work.items<-TfsApiPost(url,query)
  return(work.items)
}

GetReleaseWorkItems <- function(work.item.id.list, date = format(Sys.Date())) {
  remainder <- work.item.id.list
  
  while(length(remainder)>0) {
    w <- head(remainder, 200)
    remainder <- remainder[-c(1:200)]

    list <- paste(as.character(w), collapse=",")
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
    url <- paste("/tfs/",tfs.collection,"/_apis/wit/workitems?ids=",list,"&asOf=",date,"&fields=",gsub("[\n ]","",return.fields),"&api-version=1.0",sep="")
    if(!exists("work.items")) {
      work.items <-  TfsApiGet(url)$value$fields
    } else {
      work.items <- full_join(work.items, TfsApiGet(url)$value$fields)
    }
    
  }
  return(work.items)
}

GetBacklogSizeOverTime <- function(iteration.ids, dates) {
  cat("Dates:", dates)
  backlog.size.over.time <- data.frame(TOTAL_RELEASE_POINTS = double(), AS_OF = character())
  for(i in 1:length(dates)) {
    work.item.ids <- GetReleaseWorkItemIds(iteration.ids, dates[i])
    work.item.df <- GetReleaseWorkItems(work.item.ids$workItems$id, dates[i])
    backlog.size.as.of <- data.frame(TOTAL_RELEASE_POINTS = sum(work.item.df$Microsoft.VSTS.Scheduling.Effort, na.rm=TRUE), AS_OF = dates[i])
    backlog.size.over.time <- bind_rows(backlog.size.over.time, backlog.size.as.of)
  }

  return(backlog.size.over.time)
  
}