plotSprintChart <- function(df, chartTitle, yAxisTitle){
  df$SPRINT_DAY <- c(1:nrow(df))
#  df$X_LABEL <- df %>% tidyr::unite(label, SPRINT_DAY, Date, sep = ' - ')
  long_df <- df %>% tidyr::gather(Type, Points, c(Completed, Total))
  plot_ly(data = long_df, x = SPRINT_DAY, y = Points, color = Type, fill = 'tonexty') %>%
  layout(title = chartTitle, 
         yaxis = list(title = yAxisTitle, rangemode = "tozero"), 
         xaxis = list(title = "Day of Sprint", autotick = FALSE))
}