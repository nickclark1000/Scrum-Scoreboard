plotBurnupChart <- function(sprint, completedReleasePoints, totalReleasePoints, yMax, noChangeVector, targetReleaseSprint, completedForecast){
  df<-data.frame(Sprint=as.factor(sprint), COMPLETED=completedReleasePoints, TOTAL=totalReleasePoints)
  long_df <- df %>% tidyr::gather(Type, Points, c(COMPLETED,TOTAL))
  
  p<-plot_ly(data=long_df, x=Sprint, y=Points,color = Type) %>%
    add_trace(x = c(targetReleaseSprint, targetReleaseSprint), y = c(0,max(Points)), mode="lines",name="Target Release",line=list(dash="dash",color="black")) %>%
    add_trace(x = c(BACKLOG5_LAST5AVG_INTERSECTION$x, BACKLOG5_LAST5AVG_INTERSECTION$x), y = c(0,BACKLOG5_LAST5AVG_INTERSECTION$x*BACKLOG_AVG_5_LM$coef[2]+BACKLOG_AVG_5_LM$coef[1]), mode="lines",name="Projected Increase",line=list(dash="dash",color="orange"),showlegend = FALSE) %>%
    add_trace(x = c(NC_LAST5AVG_INTERSECTION$x, NC_LAST5AVG_INTERSECTION$x), y = c(0,max(totalReleasePoints)), mode="lines",name="No Change Target",line=list(dash="dash",color="purple"),showlegend = FALSE) %>%
    add_trace(x = c(tail(sprint,1), NC_LAST5AVG_INTERSECTION$x), y = c(max(totalReleasePoints),max(totalReleasePoints)), mode="lines",name="No Change",line=list(dash="dash",color="purple")) %>%
    add_trace(x=c(tail(sprint,1),BACKLOG5_LAST5AVG_INTERSECTION$x),y=c(tail(sprint,1)*AVG_5_LM$coef[2]+AVG_5_LM$coef[1],BACKLOG5_LAST5AVG_INTERSECTION$x*AVG_5_LM$coef[2]+AVG_5_LM$coef[1]),line=list(dash="dash",color="red"),showlegend = FALSE) %>%
    add_trace(x=c(tail(sprint,1),BACKLOG5_LAST5AVG_INTERSECTION$x),y=c(tail(sprint,1)*BACKLOG_AVG_5_LM$coef[2]+BACKLOG_AVG_5_LM$coef[1],BACKLOG5_LAST5AVG_INTERSECTION$x*BACKLOG_AVG_5_LM$coef[2]+BACKLOG_AVG_5_LM$coef[1]),line=list(dash="dash",color="orange"),showlegend = FALSE) %>%
  layout(title="Release Burn-up", yaxis = list(rangemode = "tozero"),xaxis = list(autotick = FALSE, dtick=1),legend = list(x = 0.5, y = 0))
  
}