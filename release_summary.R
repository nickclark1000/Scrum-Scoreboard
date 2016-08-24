#for data wrangling
library(dplyr)
#for calculating moving averages
library(TTR)
#for tfs data
library(rtfs)


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
    SPRINT_NAME = current_release$children[[1]]$name, SPRINT_INDEX = c(1:nrow(current_release$children[[1]])), 
    START_DATE = current_release$children[[1]]$attributes$startDate, END_DATE = current_release$children[[1]]$attributes$finishDate)

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

# Add Velocity
release_summary <- inner_join(release_summary, 
                                 rename(work_item_df %>% 
                                 group_by(System.IterationId) %>% 
                                 summarise(VELOCITY = sum(Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)), 
                                 SPRINT_ITERATION_ID = System.IterationId), 
                              by = "SPRINT_ITERATION_ID")

### Total work done measured at the END of each sprint
release_summary$COMPLETED_RELEASE_POINTS <- cumsum(release_summary$VELOCITY)

### Moving average calculations
if (nrow(release_summary) > 4) {
    release_summary$VELOCITY_SMA_5 <- SMA(release_summary$VELOCITY, 5)
    release_summary$VELOCITY_WMA_5 <- WMA(release_summary$VELOCITY, 5)
} else {
    release_summary$VELOCITY_SMA_5 <- SMA(release_summary$VELOCITY, nrow(release_summary) - 1)
    release_summary$VELOCITY_WMA_5 <- WMA(release_summary$VELOCITY, nrow(release_summary) - 1)
}

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

backlog_history <- rtfs::get_backlog_history(iteration_ids, release_summary$END_DATE)

release_summary <- left_join(release_summary, backlog_history, by = c(END_DATE = "AS_OF"))