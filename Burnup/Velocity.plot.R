plotVelocityTimeSeries <- function(sprint, actualVelocity, plannedVelocity, movingAverage){
  # par(oma = c(3, 1, 1, 1))
  # plot(sprint, actualVelocity, type="o", main="Velocity", xlab="Sprint", ylab="Points", xaxt='n', ylim=c(0,max(plannedVelocity, actualVelocity)))
  # axis(1,labels=sprint, at=sprint, las=2)
  # ##Add moving average to plot
  # lines(y=movingAverage,x=sprint, col="blue", lwd=2, lty=3)
  # 
  # ##Add planned velocity to plot
  # lines(y=plannedVelocity,x=sprint, col="red", lwd=2, lty=2)
  # 
  # # Add legend to bottom, outside plot region
  # par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  # plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  # legend("bottom", 
  #        legend=c("Planned Velocity", "Actual Velocity", "Simple Moving Average (n=5)"), 
  #        lwd=c(2,2,2),
  #        lty=c(2,1,3),
  #        col=c("red","black","blue"), 
  #        cex=0.75, 
  #        ncol=3, 
  #        bty="n", 
  #        xpd = TRUE
  # )
  df<-data.frame(Sprint=as.factor(sprint), Actual=actualVelocity, Planned=plannedVelocity,Average=movingAverage)
  long_df <- df %>% tidyr::gather(Type, Velocity, c(Actual,Planned,Average))
  
 #  p<-ggplot(data=long_df, aes(x=Sprint,y=Velocity, colour=Type, group=1)) +
 #    geom_line() +
 #    geom_point() +
 #    expand_limits(y=0) +
 #   theme(legend.position="bottom") +
 #   # layout(legend = list(x = 0.5, y = 0, xanchor = "left", yanchor = "bottom", orientation = 'h')) +
 #    ggtitle(paste(TEAM_NAME,"Velocity"))
 # # ggplotly(p)
  p<-plot_ly(data=long_df, x=Sprint, y=Velocity,color = Type) %>%
  layout(title="Velocity", yaxis = list(rangemode = "tozero"),xaxis = list(autotick = FALSE, dtick=1),legend = list(x = 0.5, y = 0))
  
  # Sys.setenv("plotly_username" = "nickclark1000")
  # Sys.setenv("plotly_api_key" = "hwwklug3c1")
  # plotly_IMAGE(p, format = "png", out_file = "output.png")
}