###########Velocity plot

###Load summary data
source('C:/Users/u6033371/Documents/Agile Resources/AgileData/Summary.R')

###velocity plot of the last 20 sprints
t <- tail(v, 20)

##plot
plot_path <- paste('C:/Users/u6033371/Documents/Agile Resources/AgileData/Velocity_',TEAM_NAME,'.png')
png(plot_path, width=6, height=4, units="in", res=900)
par(oma = c(3, 1, 1, 1))
plot(t$SPRINT, t$VELOCITY, type="o", main="Velocity", xlab="Sprint", ylab="Points", xaxt='n')
axis(1,labels=t$SPRINT, at=t$SPRINT, las=2)

##Add SMA_5 to plot
lines(y=t$SMA_5,x=t$SPRINT, col="green", lwd=2)

# Add legend to bottom, outside plot region
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", 
       legend=c("Simple Moving Average (n=5)"), 
       lwd=c(2),
       lty=c(1),
       col=c("green"), 
       cex=0.75, 
       ncol=1, 
       bty="n", 
       xpd = TRUE
)
dev.off()