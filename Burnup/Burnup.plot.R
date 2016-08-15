plotBurnupChart <- function(release.summary, noChangeVector, target.release.date, completedForecast){
  release.summary.df <- rename(release.summary.df, Completed = COMPLETED_RELEASE_POINTS)
  release.summary.df <- rename(release.summary.df, Total = TOTAL_RELEASE_POINTS)
  long_df <- release.summary.df[,c("SPRINT_INDEX", "END_DATE", "Completed", "Total")] %>% tidyr::gather(Type, Points, c(Completed, Total))
  
  NC_VEL_XDATE <- as.Date(as.POSIXct(NC_LAST5AVG_INTERSECTION$x, origin="1970-01-01"))
  BACKLOG5_VEL_XDATE <- as.Date(as.POSIXct(BACKLOG5_LAST5AVG_INTERSECTION$x, origin="1970-01-01"))
  LAST_SPRINT_END_DATE <- as.Date(tail(release.summary.df$END_DATE,1))
  LAST_SPRINT_END_DATE_UNIX <- as.numeric(as.POSIXct(tail(release.summary.df$END_DATE, 1)))
  BACKLOG5_VEL_XPOINTS <- BACKLOG5_LAST5AVG_INTERSECTION$x*BACKLOG_AVG_5_LM$coef[2]+BACKLOG_AVG_5_LM$coef[1]

  p<-plot_ly(data = long_df, x = as.Date(END_DATE), y=Points, color = Type) %>%
     ##Target Release
     add_trace(x = c(as.Date(target.release.date), as.Date(target.release.date)), y = c(0, max(Points)), mode = "lines", name = "Target Release", line = list(dash = "dash", color = "black")) %>%
     ##No Change Target
     add_trace(x = c(NC_VEL_XDATE, NC_VEL_XDATE), y = c(0, NC_LAST5AVG_INTERSECTION$y), mode = "lines", name = "No Change Target", line = list(dash = "dash", color = "purple"), showlegend = FALSE) %>%
     ##No Change horizontal
     add_trace(x = c(LAST_SPRINT_END_DATE, NC_VEL_XDATE), y = c(NC_LAST5AVG_INTERSECTION$y, NC_LAST5AVG_INTERSECTION$y), mode = "lines", name = "No Change to Backlog Size", line = list(dash = "dash", color = "purple")) %>%
     ##Projected Backlog Size
     add_trace(x=c(LAST_SPRINT_END_DATE, BACKLOG5_VEL_XDATE),y=c(LAST_SPRINT_END_DATE_UNIX * BACKLOG_AVG_5_LM$coef[2] + BACKLOG_AVG_5_LM$coef[1], BACKLOG5_VEL_XPOINTS), name="Projected Backlog Size", line = list(dash = "dash", color = "orange"), showlegend = TRUE) %>% 
     ##Backlog Change Target
     add_trace(x = c(BACKLOG5_VEL_XDATE, BACKLOG5_VEL_XDATE), y = c(0, BACKLOG5_VEL_XPOINTS), mode="lines", line = list(dash = "dash", color = "orange"), showlegend = FALSE) %>%
     ##Velocity trend
     add_trace(x=c(LAST_SPRINT_END_DATE, max(BACKLOG5_VEL_XDATE, NC_VEL_XDATE)),y=c(LAST_SPRINT_END_DATE_UNIX * AVG_5_LM$coef[2] + AVG_5_LM$coef[1], max(BACKLOG5_VEL_XPOINTS,NC_LAST5AVG_INTERSECTION$y)), name = "Projected Velocity", line = list(dash ="dash", color = "green"), showlegend = TRUE) %>%
     
   layout(title="Release Burn-up", yaxis = list(rangemode = "tozero"), xaxis = list(title = "Date", type = "date", tick0 = LAST_SPRINT_END_DATE_UNIX * 1000, dtick = 1210000000))
  
}