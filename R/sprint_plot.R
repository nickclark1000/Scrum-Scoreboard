plotSprintChart <- function(df, chartTitle, yAxisTitle){
  long_df <- df %>% tidyr::gather(Type, Points, c(Completed, Total))
  lastDateUnix <- as.numeric(as.POSIXct(tail(df$Date, 1)))
  
  plot_ly(data = long_df, x = as.Date(Date), y = Points, color = Type, fill = 'tonexty') %>%
  layout(title = chartTitle, 
         yaxis = list(title = yAxisTitle, rangemode = "tozero"), 
         xaxis = list(title = "Date", type = "date"))
}