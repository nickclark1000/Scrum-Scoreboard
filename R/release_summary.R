#for data wrangling
library(dplyr)
#for tfs data
library(rtfs)
#for handling dates
library(lubridate)
#for calculating moving average
source("library.R")


current_release <- rtfs::get_current_release()
current_major_release <- current_release$current_major_release
current_minor_release <- current_release$current_minor_release
if (is.null(current_minor_release)) {
    current_release <- current_release$current_major_release
} else {
    current_release <- current_release$current_minor_release
}

release_summary <- data.frame(TEAM = tfs_team, RELEASE_ITERATION_ID = current_release$id, 
    RELEASE_NAME = current_release$name, SPRINT_ITERATION_ID = current_release$children[[1]]$id, 
    SPRINT_NAME = current_release$children[[1]]$name,  
    START_DATE = current_release$children[[1]]$attributes$startDate, END_DATE = current_release$children[[1]]$attributes$finishDate)

release_summary <- release_summary[order(as.Date(release_summary$START_DATE)),]

release_summary$SPRINT_INDEX <- c(1:nrow(current_release$children[[1]]))

today <- format(Sys.Date())
for (i in 1:nrow(release_summary)) {
    if (as.Date(release_summary$START_DATE[i]) <= today && 
        today <= as.Date(release_summary$END_DATE[i])) 
        CURRENT_SPRINT_INDEX <<- release_summary$SPRINT_INDEX[i]
}
release_summary <- subset(release_summary, SPRINT_INDEX < CURRENT_SPRINT_INDEX)

iteration_ids <- paste(c(current_release$id, as.character(release_summary$SPRINT_ITERATION_ID)), 
    collapse = ",")
work_item_ids <- rtfs::get_release_wi_ids(iteration_ids)$content


work_item_df <- rtfs::get_release_wis(work_item_ids$workItems$id)

# Add Actual Velocity
release_summary <- inner_join(release_summary, 
                                 rename(work_item_df %>% 
                                 group_by(System.IterationId) %>% 
                                 summarise(VELOCITY = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)), 
                                 SPRINT_ITERATION_ID = System.IterationId), 
                              by = "SPRINT_ITERATION_ID")

# Add Planned Velocity
planned_velocity <- data.frame(PLANNED_VELOCITY = double(),
                               SPRINT_ITERATION_ID = integer())
for(i in 1:length(release_summary$SPRINT_ITERATION_ID)){
  iteration_id <- release_summary$SPRINT_ITERATION_ID[i]
  date_time <- update(as_datetime(release_summary$START_DATE[i]), hour = 23, minute = 59)
  result <- rtfs::get_planned_velocity(iteration_id, date_time)
  planned_velocity <- bind_rows(planned_velocity, result)
}
release_summary <- inner_join(release_summary, planned_velocity, by = "SPRINT_ITERATION_ID")

### Total work done measured at the END of each sprint
release_summary$COMPLETED_RELEASE_POINTS <- cumsum(release_summary$VELOCITY)

### Moving average calculations
release_summary$VELOCITY_MA_5 <- velocity_moving_average(release_summary$VELOCITY)

### Defects summary
release_summary <- left_join(release_summary, 
                             rename(subset(work_item_df, System.WorkItemType == "Bug") %>% 
                                    group_by(System.IterationId) %>% 
                                    summarise(DEFECTS_COMPLETED_COUNT = n(), 
                                              DEFECTS_COMPLETED_POINTS = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)), 
                                    SPRINT_ITERATION_ID = System.IterationId), 
                             by = "SPRINT_ITERATION_ID")

defects <- subset(work_item_df, System.WorkItemType == "Bug")
pbis <- subset(work_item_df, System.WorkItemType == "Product Backlog Item")
b<-data.frame(DEFECTS_CREATED_COUNT=integer(0),DEFECTS_CREATED_POINTS=integer(0),PBIS_CREATED_COUNT=integer(0),PBIS_CREATED_POINTS=integer(0))
for (i in 1:nrow(release_summary)) {
  a<-subset(defects, defects$System.CreatedDate>= as.Date(release_summary$START_DATE[i]) & defects$System.CreatedDate<= as.Date(release_summary$END_DATE[i]))
  c<-subset(pbis, pbis$System.CreatedDate>= as.Date(release_summary$START_DATE[i]) & pbis$System.CreatedDate<= as.Date(release_summary$END_DATE[i]))
  
  b<-bind_rows(b,data.frame(DEFECTS_CREATED_COUNT=nrow(a),DEFECTS_CREATED_POINTS=sum(a$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE), PBIS_CREATED_COUNT=nrow(c), PBIS_CREATED_POINTS=sum(c$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)))
}

release_summary <- bind_cols(release_summary, b)
### PBI summary
release_summary <- left_join(release_summary, 
                             rename(subset(work_item_df, System.WorkItemType == "Product Backlog Item") %>% 
                                      group_by(System.IterationId) %>% 
                                      summarise(PBIS_COMPLETED_COUNT = n(), 
                                                PBIS_COMPLETED_POINTS = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)), 
                                    SPRINT_ITERATION_ID = System.IterationId), 
                             by = "SPRINT_ITERATION_ID")

backlog_history <- rtfs::get_backlog_history(iteration_ids, release_summary$END_DATE)

release_summary <- left_join(release_summary, backlog_history, by = c(END_DATE = "AS_OF"))

#test_case_history <- rtfs::get_tc_automation_history(release_summary$END_DATE)
#release_summary <- left_join(release_summary, test_case_history, by = c(END_DATE = "AS_OF"))