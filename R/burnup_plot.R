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
  
  plot_ly(long_df, 
          x = ~as.Date(Date), 
          y = ~Points, 
          color = ~Type, 
          type = 'scatter', 
          mode = 'lines+markers',
          fill = 'tonexty',
          text = ~Points) %>%
    layout(title = ~chartTitle,
           yaxis = list(title = ~yAxisTitle, rangemode = "tozero"),
           xaxis = list(title = "Date", type = "date", tick0 = ~lastDateUnix * 1000, dtick = 1210000000)) %>%
    add_text(textfont = list(family = "sans serif", size = 14, color = toRGB("grey50")), 
             textposition = "top right", 
             showlegend = FALSE,
             fill = 'none') %>%
    ##Target Release
    add_trace(x = c(~as.Date(targetDate), ~as.Date(targetDate)),
              y = c(0, ~max(Points)),
              mode = "lines",
              color = 'Target Release',
              line = list(dash = "dash", color = "black"),
              fill = 'none') %>%
    ##No Change Target
    add_trace(x = c(~constantXDate, ~constantXDate),
              y = c(0, ~constantIntersection$y),
              mode = "lines",
              legendgroup = 'No Change',
              color = "No Change Target",
              line = list(dash = "dash", color = "purple"),
              showlegend = FALSE,
              fill = 'none') %>%
    ##No Change horizontal
    add_trace(x = c(~lastDate, ~constantXDate),
              y = c(~constantIntersection$y, ~constantIntersection$y),
              mode = "lines",
              legendgroup = 'No Change',
              color = "No Change to Backlog Size",
              line = list(dash = "dash", color = "purple"),
              fill = 'none') %>%
    ##Projected Backlog Size
    add_trace(x = c(~lastDate, ~last5XDate),
              y = c(~lastDateUnix * totalLast5LM$coef[2] + totalLast5LM$coef[1], ~totalIncreasePoints),
              legendgroup = 'Projected Change',
              color = "Projected Backlog Size",
              line = list(dash = "dash", color = "orange"),
              showlegend = TRUE,
              fill = 'none') %>%
    ##Backlog Change Target
    add_trace(x = c(~last5XDate, ~last5XDate),
              y = c(0, ~totalIncreasePoints),
              mode="lines",
              legendgroup = 'Projected Change',
              line = list(dash = "dash", color = "orange"),
              showlegend = FALSE,
              fill = 'none') %>%
    ##Velocity trend
    add_trace(x = c(~lastDate, ~max(last5XDate, constantXDate)),
              y = c(~lastDateUnix * completedLast5LM$coef[2] + completedLast5LM$coef[1], ~max(totalIncreasePoints, constantIntersection$y)),
              color = "Projected Velocity",
              line = list(dash ="dash", color = "green"),
              showlegend = TRUE,
              fill = 'none')
}