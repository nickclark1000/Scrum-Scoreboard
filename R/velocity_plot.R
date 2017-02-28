plotVelocityTimeSeries <- function(sprint, actualVelocity, plannedVelocity, movingAverage){
  df <- data.frame(Sprint = as.factor(sprint), Actual = actualVelocity, Planned = plannedVelocity, Average = movingAverage)
  long_df <- df %>% tidyr::gather(Type, Velocity, c(Actual, Planned, Average))
  plot_ly(long_df, 
          x = ~Sprint, 
          y = ~Velocity, 
          color = ~Type, 
          type = 'scatter', 
          mode = 'lines+markers+text') %>%
      layout(title = "Velocity", 
             yaxis = list(rangemode = "tozero", title = "Story Points"), 
             xaxis = list(autotick = FALSE, dtick = 1), 
             legend = list(x = 0.5, y = 0))
}