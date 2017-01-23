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
release_summary <- left_join(release_summary, 
                                 rename(work_item_df %>% 
                                    group_by(System.IterationId) %>% 
                                    summarise(VELOCITY = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)),
                                 SPRINT_ITERATION_ID = System.IterationId), 
                              by = "SPRINT_ITERATION_ID") %>%
                    mutate_each(funs(replace(., which(is.na(.)), 0)))

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



created <- data.frame(DEFECTS_CREATED_COUNT = integer(0),
                DEFECTS_CREATED_POINTS = integer(0),
                PBIS_CREATED_COUNT = integer(0),
                PBIS_CREATED_POINTS = integer(0))

for (i in 1:nrow(release_summary)){
  createdThisSprint <- subset(work_item_df, work_item_df$System.CreatedDate >= as.Date(release_summary$START_DATE[i]) & work_item_df$System.CreatedDate <= as.Date(release_summary$END_DATE[i]))
  bugsCreatedThisSprint <- subset(createdThisSprint, System.WorkItemType == "Bug")
  pbisCreatedThisSprint <- subset(createdThisSprint, System.WorkItemType == "Product Backlog Item")
  created <- bind_rows(created, data.frame(DEFECTS_CREATED_COUNT = nrow(bugsCreatedThisSprint),
                               DEFECTS_CREATED_POINTS = sum(bugsCreatedThisSprint$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE), 
                               PBIS_CREATED_COUNT = nrow(pbisCreatedThisSprint), 
                               PBIS_CREATED_POINTS = sum(pbisCreatedThisSprint$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)))
}

release_summary <- bind_cols(release_summary, created)

### Defects summary
release_summary <- left_join(release_summary, 
                             rename(subset(work_item_df, System.WorkItemType == "Bug") %>% 
                                      group_by(System.IterationId) %>% 
                                      summarise(DEFECTS_COMPLETED_COUNT = n(), 
                                                DEFECTS_COMPLETED_POINTS = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)), 
                                    SPRINT_ITERATION_ID = System.IterationId), 
                             by = "SPRINT_ITERATION_ID")

### PBI summary
release_summary <- left_join(release_summary, 
                             rename(subset(work_item_df, System.WorkItemType == "Product Backlog Item") %>% 
                                      group_by(System.IterationId) %>% 
                                      summarise(PBIS_COMPLETED_COUNT = n(), 
                                                PBIS_COMPLETED_POINTS = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)), 
                                    SPRINT_ITERATION_ID = System.IterationId), 
                             by = "SPRINT_ITERATION_ID")

### Work Order summary
release_summary <- left_join(release_summary, 
                             rename(subset(work_item_df, System.WorkItemType == "Work Order") %>% 
                                      group_by(System.IterationId) %>% 
                                      summarise(WOS_COMPLETED_COUNT = n(), 
                                                WOS_COMPLETED_POINTS = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)), 
                                    SPRINT_ITERATION_ID = System.IterationId), 
                             by = "SPRINT_ITERATION_ID")

backlog_history <- rtfs::get_backlog_history(iteration_ids, release_summary$END_DATE)

release_summary <- left_join(release_summary, backlog_history, by = c(END_DATE = "AS_OF"))

release_summary$WORK_ITEMS_COMPLETED_COUNT <- rowSums(release_summary[, c("DEFECTS_COMPLETED_COUNT", "PBIS_COMPLETED_COUNT", "WOS_COMPLETED_COUNT")], na.rm = TRUE)
release_summary$COMPLETED_RELEASE_COUNT <- cumsum(release_summary$WORK_ITEMS_COMPLETED_COUNT)

#test_case_history <- rtfs::get_tc_automation_history(release_summary$END_DATE)
#release_summary <- left_join(release_summary, test_case_history, by = c(END_DATE = "AS_OF"))

sprint_history <- rtfs::get_sprint_history(tail(release_summary$SPRINT_ITERATION_ID, 1), tail(release_summary$START_DATE, 1), tail(release_summary$END_DATE, 1))
