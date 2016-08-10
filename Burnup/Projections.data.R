source("library.R")

w <- release.summary.df

cat(file=stderr(), "current sprint index:", CURRENT_SPRINT_INDEX, "\n")
NO_CHANGE_LINE <- data.frame(x = c(CURRENT_SPRINT_INDEX - 1, 1000), y = c(tail(w$TOTAL_RELEASE_POINTS, 1), tail(w$TOTAL_RELEASE_POINTS, 1)))
NO_CHANGE_LM <- lm(NO_CHANGE_LINE$y ~ NO_CHANGE_LINE$x)


###Initialize data frame for 5-sprint avg velocity forecast
FORECAST_AVG_5 <- data.frame(SPRINT_INDEX = c(tail(w$SPRINT_INDEX,1),tail(w$SPRINT_INDEX,1)+1), AVG_5_PROJECTED_POINTS = c(tail(w$COMPLETED_RELEASE_POINTS,1),tail(w$COMPLETED_RELEASE_POINTS,1) + tail(w$VELOCITY_SMA_5,1)))
AVG_5_LM <- lm(FORECAST_AVG_5$AVG_5_PROJECTED_POINTS ~ FORECAST_AVG_5$SPRINT_INDEX)


###Initialize data frame for 5-sprint avg backlog forecast
FORECAST_BACKLOG_AVG_5 <- data.frame(SPRINT_INDEX = c(tail(w$SPRINT_INDEX, 1), tail(w$SPRINT_INDEX, 1) + 1), AVG_5_PROJECTED_POINTS = c(tail(w$TOTAL_RELEASE_POINTS,1),tail(w$TOTAL_RELEASE_POINTS,1) + mean(diff(tail(w$TOTAL_RELEASE_POINTS,6)))))
BACKLOG_AVG_5_LM <- lm(FORECAST_BACKLOG_AVG_5$AVG_5_PROJECTED_POINTS ~ FORECAST_BACKLOG_AVG_5$SPRINT_INDEX)



###Determine 'No Change' and 'Last-5-sprints' intersection point
NC_LAST5AVG_INTERSECTION <- lmIntx(NO_CHANGE_LM,AVG_5_LM)

BACKLOG5_LAST5AVG_INTERSECTION <- lmIntx(BACKLOG_AVG_5_LM,AVG_5_LM)