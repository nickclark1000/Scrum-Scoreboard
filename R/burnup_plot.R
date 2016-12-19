source("library.R")

plotBurnupChart <- function(df, targetDate, chartTitle, yAxisTitle){
  long_df <- df %>% tidyr::gather(Type, Points, c(Completed, Total))
  lastDateUnix <- as.numeric(as.POSIXct(tail(df$Date, 1)))
  lastDate <- as.Date(tail(df$Date, 1))
  lastTotal <- tail(df$Total, 1)
  lastCompleted <- tail(df$Completed, 1)
  
  totalConstantLM <- getLinearModel(x1 = lastDateUnix*1000,
                                    x2 = lastDateUnix*1000 + 1000000,
                                    y1 = lastTotal,
                                    y2 = lastTotal)
  
  completedLast5LM <- getLinearModel(x1 = lastDateUnix, 
                                     x2 = lastDateUnix + 1210000,
                                     y1 = lastCompleted, 
                                     y2 = lastCompleted + mean(diff(tail(df$Completed, 6))))
  
  totalLast5LM <- getLinearModel(x1 = lastDateUnix, 
                                 x2 = lastDateUnix + 1210000,
                                 y1 = lastTotal, 
                                 y2 = lastTotal + mean(diff(tail(df$Total, 6))))
  
  constantIntersection <- lmIntx(totalConstantLM, completedLast5LM)
  
  last5Intersection <- lmIntx(totalLast5LM, completedLast5LM)
  
  totalIncreasePoints <- last5Intersection$x * totalLast5LM$coef[2] + totalLast5LM$coef[1]
  constantXDate <- as.Date(as.POSIXct(constantIntersection$x, origin = "1970-01-01"))
  last5XDate <- as.Date(as.POSIXct(last5Intersection$x, origin = "1970-01-01"))
  
  plot_ly(data = long_df, x = as.Date(Date), y = Points, color = Type, fill = 'tonexty') %>%
  layout(title = chartTitle, 
         yaxis = list(title = yAxisTitle, rangemode = "tozero"), 
         xaxis = list(title = "Date", type = "date", tick0 = lastDateUnix * 1000, dtick = 1210000000)) %>%
  
  ##Target Release  
    add_trace(x = c(as.Date(targetDate), as.Date(targetDate)), 
              y = c(0, max(Points)), 
              mode = "lines", 
              name = "Target Release", 
              line = list(dash = "dash", color = "black")) %>%
  ##No Change Target
  add_trace(x = c(constantXDate, constantXDate), 
            y = c(0, constantIntersection$y), 
            mode = "lines", 
            name = "No Change Target", 
            line = list(dash = "dash", color = "purple"), 
            showlegend = FALSE) %>%
  ##No Change horizontal
  add_trace(x = c(lastDate, constantXDate), 
            y = c(constantIntersection$y, constantIntersection$y), 
            mode = "lines", 
            name = "No Change to Backlog Size", 
            line = list(dash = "dash", color = "purple")) %>%
  ##Projected Backlog Size
  add_trace(x = c(lastDate, last5XDate), 
            y = c(lastDateUnix * totalLast5LM$coef[2] + totalLast5LM$coef[1], totalIncreasePoints), 
            name = "Projected Backlog Size", 
            line = list(dash = "dash", color = "orange"), 
            showlegend = TRUE) %>% 
  ##Backlog Change Target
  add_trace(x = c(last5XDate, last5XDate), 
            y = c(0, totalIncreasePoints), 
            mode="lines", 
            line = list(dash = "dash", color = "orange"), 
            showlegend = FALSE) %>%
  ##Velocity trend
  add_trace(x = c(lastDate, max(last5XDate, constantXDate)), 
            y = c(lastDateUnix * completedLast5LM$coef[2] + completedLast5LM$coef[1], max(totalIncreasePoints, constantIntersection$y)), 
            name = "Projected Velocity", 
            line = list(dash ="dash", color = "green"), 
            showlegend = TRUE)
        

}