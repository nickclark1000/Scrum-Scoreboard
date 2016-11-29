source("library.R")

cat(file=stderr(), "current sprint index:", CURRENT_SPRINT_INDEX, "\n")
NO_CHANGE_LINE <- data.frame(x = c(as.numeric(as.POSIXct((tail(release_summary$END_DATE, 1))))*1000, as.numeric(as.POSIXct((tail(release_summary$END_DATE, 1))))*1000 + 1000000), y = c(tail(release_summary$TOTAL_RELEASE_POINTS, 1), tail(release_summary$TOTAL_RELEASE_POINTS, 1)))
NO_CHANGE_LM <- lm(NO_CHANGE_LINE$y ~ NO_CHANGE_LINE$x)


###Initialize data frame for 5-sprint avg velocity forecast
FORECAST_AVG_5 <- data.frame(DATE = c(as.numeric(as.POSIXct((tail(release_summary$END_DATE, 1)))), as.numeric(as.POSIXct((tail(release_summary$END_DATE, 1)))) + 1210000), AVG_5_PROJECTED_POINTS = c(tail(release_summary$COMPLETED_RELEASE_POINTS,1),tail(release_summary$COMPLETED_RELEASE_POINTS,1) + mean(diff(tail(release_summary$COMPLETED_RELEASE_POINTS, 6)))))
AVG_5_LM <- lm(FORECAST_AVG_5$AVG_5_PROJECTED_POINTS ~ FORECAST_AVG_5$DATE)


###Initialize data frame for 5-sprint avg backlog forecast
FORECAST_BACKLOG_AVG_5 <- data.frame(DATE = c(as.numeric(as.POSIXct((tail(release_summary$END_DATE, 1)))), as.numeric(as.POSIXct((tail(release_summary$END_DATE, 1)))) + 1210000), AVG_5_PROJECTED_POINTS = c(tail(release_summary$TOTAL_RELEASE_POINTS,1),tail(release_summary$TOTAL_RELEASE_POINTS,1) + mean(diff(tail(release_summary$TOTAL_RELEASE_POINTS, 6)))))
BACKLOG_AVG_5_LM <- lm(FORECAST_BACKLOG_AVG_5$AVG_5_PROJECTED_POINTS ~ FORECAST_BACKLOG_AVG_5$DATE)



###Determine 'No Change' and 'Last-5-sprints' intersection point
NC_LAST5AVG_INTERSECTION <- lmIntx(NO_CHANGE_LM,AVG_5_LM)

BACKLOG5_LAST5AVG_INTERSECTION <- lmIntx(BACKLOG_AVG_5_LM,AVG_5_LM)

plotBurnupChart <- function(df, targetDate, chartTitle, yAxisTitle){
  long_df <- df[,c("Date", "Completed", "Total")] %>% tidyr::gather(Type, Points, c(Completed, Total))
  
  totalConstantDate <- as.Date(as.POSIXct(NC_LAST5AVG_INTERSECTION$x, origin = "1970-01-01"))
  totalIncreaseDate <- as.Date(as.POSIXct(BACKLOG5_LAST5AVG_INTERSECTION$x, origin = "1970-01-01"))
  lastDate <- as.Date(tail(df$Date, 1))
  lastDateUnix <- as.numeric(as.POSIXct(tail(df$Date, 1)))
  totalIncreasePoints <- BACKLOG5_LAST5AVG_INTERSECTION$x * BACKLOG_AVG_5_LM$coef[2] + BACKLOG_AVG_5_LM$coef[1]
  
  p<-plot_ly(data = long_df, x = as.Date(Date), y=Points, color = Type, fill = 'tonexty') %>%
    ##Target Release
    add_trace(x = c(as.Date(targetDate), as.Date(targetDate)), y = c(0, max(Points)), mode = "lines", name = "Target Release", line = list(dash = "dash", color = "black")) %>%
    ##No Change Target
    add_trace(x = c(totalConstantDate, totalConstantDate), y = c(0, NC_LAST5AVG_INTERSECTION$y), mode = "lines", name = "No Change Target", line = list(dash = "dash", color = "purple"), showlegend = FALSE) %>%
    ##No Change horizontal
    add_trace(x = c(lastDate, totalConstantDate), y = c(NC_LAST5AVG_INTERSECTION$y, NC_LAST5AVG_INTERSECTION$y), mode = "lines", name = "No Change to Backlog Size", line = list(dash = "dash", color = "purple")) %>%
    ##Projected Backlog Size
    add_trace(x = c(lastDate, totalIncreaseDate), y = c(lastDateUnix * BACKLOG_AVG_5_LM$coef[2] + BACKLOG_AVG_5_LM$coef[1], totalIncreasePoints), name = "Projected Backlog Size", line = list(dash = "dash", color = "orange"), showlegend = TRUE) %>% 
    ##Backlog Change Target
    add_trace(x = c(totalIncreaseDate, totalIncreaseDate), y = c(0, totalIncreasePoints), mode="lines", line = list(dash = "dash", color = "orange"), showlegend = FALSE) %>%
    ##Velocity trend
    add_trace(x = c(lastDate, max(totalIncreaseDate, totalConstantDate)), y = c(lastDateUnix * AVG_5_LM$coef[2] + AVG_5_LM$coef[1], max(totalIncreasePoints, NC_LAST5AVG_INTERSECTION$y)), name = "Projected Velocity", line = list(dash ="dash", color = "green"), showlegend = TRUE) %>%
    
    layout(title = chartTitle, yaxis = list(title = yAxisTitle, rangemode = "tozero"), xaxis = list(title = "Date", type = "date", tick0 = lastDateUnix * 1000, dtick = 1210000000))
  
}