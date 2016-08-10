plotBurnupChart <- function(sprint, completedReleasePoints, totalReleasePoints, yMax, noChangeVector, targetReleaseSprint, completedForecast){
  # par(oma = c(3, 1, 1, 1))
  # plot(sprint, completedReleasePoints, main=paste(releaseName,"Release Burn-up"), xlab="Sprint", ylab="Points", xaxt='n', ylim=c(0, yMax+50), xlim = c(head(sprint,1),round(NC_LAST5AVG_INTERSECTION$x+1,0)))
  # 
  # ###Add the 'No Change' horizontal line of total release points
  # lines(x=noChangeVector$x, y=noChangeVector$y,col="blue",lwd=2, lty=2)
  # 
  # 
  # ###Add 5-sprint avg velocity line
  # abline(completedForecast,col="orange",lwd=2, lty=2)
  # 
  # abline(BACKLOG_AVG_5_LM,col="red",lwd=2, lty=2)
  # 
  # 
  # ###add intersection of 'No Change' and 'Avg last 5'
  # abline(v=NC_LAST5AVG_INTERSECTION$x,col="orange",lwd=2, lty=3)
  # 
  # abline(v=BACKLOG5_LAST5AVG_INTERSECTION$x,col="red",lwd=2, lty=3)
  # 
  # 
  # ###add target release date/sprint
  # abline(v=targetReleaseSprint,col="black",lwd=2, lty=3)
  # 
  # ##Add Backlog size to plot
  # lines(y=totalReleasePoints,x=sprint, col="blue", lwd=2)
  # 
  # ###Add axis
  # axis(1,labels=c(head(sprint,1):round(NC_LAST5AVG_INTERSECTION$x+1,0)), at=c(head(sprint,1):round(NC_LAST5AVG_INTERSECTION$x+1,0)), las=2)
  # 
  # # Add legend to bottom, outside plot region
  # par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  # plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  # legend("bottom", 
  #        legend=c("Backlog Size","No Scope Change","Projected Backlog Increase"#,"All-sprints linear trend"
  #                 ,"Last-5-sprint average", "Target Release"), 
  #        lwd=c(2,2,2,2,2),
  #        lty=c(1,2,2,2,3),
  #        col=c("blue", "blue","red"#"green"
  #              ,"orange","black"), 
  #        cex=0.75, 
  #        ncol=2, 
  #        bty="n", 
  #        xpd = TRUE
  # )
 # m<-lm(tail(totalReleasePoints,5)~tail(sprint,5))
  #n<-lm(tail(completedReleasePoints,5)~tail(sprint,5))
  #BACKLOG5_LAST5AVG_INTERSECTION<-lmIntx(m,n)
 # m<-loess(totalReleasePoints~sprint)
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