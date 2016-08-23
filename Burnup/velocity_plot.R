plotVelocityTimeSeries <- function(sprint, actualVelocity, movingAverage){

  df<-data.frame(Sprint=as.factor(sprint), Actual = actualVelocity, Average = movingAverage)
  long_df <- df %>% tidyr::gather(Type, Velocity, c(Actual, Average))
  p<-plot_ly(data = long_df, x = Sprint, y = Velocity, color = Type) %>%
  layout(title = "Velocity", yaxis = list(rangemode = "tozero"), xaxis = list(autotick = FALSE, dtick = 1),legend = list(x = 0.5, y = 0))
  
  # Sys.setenv("plotly_username" = "nickclark1000")
  # Sys.setenv("plotly_api_key" = "hwwklug3c1")
  # plotly_IMAGE(p, format = "png", out_file = "output.png")
}