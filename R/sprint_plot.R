plotSprintChart <- function(df, chartTitle, yAxisTitle){
  df$SPRINT_DAY <- c(1:nrow(df))
  long_df <- df %>% tidyr::gather(Type, Points, c(Completed, Total))
  plot_ly(long_df, 
          x = ~SPRINT_DAY, 
          y = ~Points, 
          color = ~Type, 
          type = 'scatter', 
          mode = 'lines+markers', 
          fill = 'tonexty') %>%
      layout(title = ~chartTitle, 
             yaxis = list(title = ~yAxisTitle, rangemode = "tozero"), 
             xaxis = list(title = "Day of Sprint", autotick = FALSE))
}