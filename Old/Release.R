###########Burn-up Chart

#####User Input Variables
FIRST_SPRINT <- 101  ###first sprint in release
CURRENT_SPRINT <- 109
RELEASE_NAME <- '3.0.0 GA Solution Manager' ###to be used in the plot title
CURRENT_RELEASE_IDENTIFIER <- "3.0 GA" ###Unique string in Iteration Path to identify release work items
RELEASE_IDENTIFIERS <- c("3.0 Alpha", "3.0 TPR", "3.0 Beta", "3.0 GA")
TARGET_RELEASE_SPRINT <- 115 ###targeted release (at the beginning of this sprint. i.e., end of previous sprint)
BACKLOG_CSV <- "C:/users/u6033371/Desktop/EDG Kore Team.csv" ###location of csv file on disk

###add dplyr library
library(dplyr)

###Read backlog state CSV, skip the first row that TFS adds to excel sheet
d <- read.csv(BACKLOG_CSV, skip=1)

###Remove all non-zero work items from backlog
e <- dplyr::filter(d, d$Effort>0)

###Group by iteration path and set the column names
v<- setNames(aggregate(e$Effort, by=list(ITERATION_PATH=e$Iteration.Path), FUN=sum), c("ITERATION_PATH","VELOCITY"))

###Remove everything to the left of Sprint
a <- sapply(strsplit(as.character(v$ITERATION_PATH), split='Sprint ', fixed=TRUE), function(x) (x[2]))

###Remove everything to the right of '(' and convert to numeric
v$SPRINT <- as.numeric(sapply(strsplit(a, split=' (', fixed=TRUE), function(x) (x[1])))

###Subset of current release sprints
w <- subset(v, SPRINT>=FIRST_SPRINT & SPRINT<CURRENT_SPRINT)

###sort
w<- w[order(w$SPRINT),]

###Initialize 'TOTAL' variable
TOTAL = 0

###Calculate total work done over time by looping through each sprint
for (i in 1:length(w[,1])) {
  TOTAL = TOTAL + w[i,"VELOCITY"]
  w[i,"COMPLETED_RELEASE_POINTS"] <- TOTAL
}

###Get all items that contain CURRENT_RELEASE_IDENTIFIER in the iteration path
q<-v[grep(CURRENT_RELEASE_IDENTIFIER, v$ITERATION_PATH),]

###'No Change' in release points
NO_CHANGE <- sum(q$VELOCITY)

###add 5-sprint average velocity line
avg <- mean(tail(w$VELOCITY,5))

###Define forecast data frame 'f' and reorder the columns
f <- subset(w, select=-c(ITERATION_PATH))[,c(2,1,3)]

###Initialize data frame for 5-sprint avg velocity forecast
FORECAST_AVG_5 <- data.frame(SPRINT = max(w$SPRINT), FORECASTED_RELEASE_POINTS = TOTAL)
AVG_5_LINE <- data.frame(x=c(FORECAST_AVG_5$SPRINT[1],FORECAST_AVG_5$SPRINT[1]+1),y=c(FORECAST_AVG_5$FORECASTED_RELEASE_POINTS[1],FORECAST_AVG_5$FORECASTED_RELEASE_POINTS[1]+avg))
AVG_5_LM <- lm(AVG_5_LINE$y~AVG_5_LINE$x)

###Initialize data frame for Last-5-sprint linear trend forecast
FORECAST_LM_5 <- data.frame(SPRINT = max(w$SPRINT), FORECASTED_RELEASE_POINTS = TOTAL)

###Initialize data frame for All-sprint linear trend forecast
FORECAST_LM_ALL <- data.frame(SPRINT = max(w$SPRINT), FORECASTED_RELEASE_POINTS = TOTAL)

NO_CHANGE_LINE <- data.frame(x=c(0,1000),y=c(NO_CHANGE,NO_CHANGE))
NO_CHANGE_LM <- lm(NO_CHANGE_LINE$y~NO_CHANGE_LINE$x)

ALL_SPRINT_LM <- lm(w$COMPLETED_RELEASE_POINTS~w$SPRINT)

LAST_5_SPRINT_LM <- lm(tail(w$COMPLETED_RELEASE_POINTS,5)~tail(w$SPRINT,5))



###Project data points until completion
projectedDataPoints <- function(initialSprint, initialTotal, predictionModel, dataFrame) {
  ###Initialize variables
  LAST_SPRINT <- initialSprint
  FORECAST <- initialTotal
  
  ###Determine each subsequent forecasted point until you reach the NO_CHANGE line
  while(FORECAST < NO_CHANGE){
    LAST_SPRINT = LAST_SPRINT + 1
    FORECAST = predictionModel$coefficient[2]*LAST_SPRINT + predictionModel$coefficient[1]
    dataFrame <- bind_rows(dataFrame,data.frame(SPRINT = LAST_SPRINT,FORECASTED_RELEASE_POINTS = FORECAST))
  }
  return(dataFrame)
}

FORECAST_LM_5 <- projectedDataPoints(FORECAST_LM_5$SPRINT[1],FORECAST_LM_5$FORECASTED_RELEASE_POINTS[1], LAST_5_SPRINT_LM, FORECAST_LM_5)
names(FORECAST_LM_5)[2] <- "LM5_PROJECTED_POINTS"
f <- full_join(f,FORECAST_LM_5,by="SPRINT")

FORECAST_LM_ALL <- projectedDataPoints(FORECAST_LM_ALL$SPRINT[1],FORECAST_LM_ALL$FORECASTED_RELEASE_POINTS[1], ALL_SPRINT_LM, FORECAST_LM_ALL)
names(FORECAST_LM_ALL)[2] <- "LM_ALL_PROJECTED_POINTS"
f <- full_join(f,FORECAST_LM_ALL,by="SPRINT")

FORECAST_AVG_5 <- projectedDataPoints(FORECAST_AVG_5$SPRINT[1],FORECAST_AVG_5$FORECASTED_RELEASE_POINTS[1], AVG_5_LM, FORECAST_AVG_5)
names(FORECAST_AVG_5)[2] <- "AVG_5_PROJECTED_POINTS"
f <- full_join(f,FORECAST_AVG_5,by="SPRINT")

z <- max(f$SPRINT)
while(z<TARGET_RELEASE_SPRINT){
  z = z + 1
  f <- bind_rows(f,data.frame(SPRINT = z))
}

##########Determine intersection points
# Linear model Intercept function
lmIntx <- function(fit1, fit2, rnd=2) {
  b1<- fit1$coefficient[1]  #y-int for fit1
  m1<- fit1$coefficient[2]  #slope for fit1
  b2<- fit2$coefficient[1]  #y-int for fit2
  m2<- fit2$coefficient[2]  #slope for fit2
  if(m1==m2 & b1==b2) {print("Lines are identical")
  } else if(m1==m2 & b1 != b2) {print("Lines are parallel")
  } else {
    x <- (b2-b1)/(m1-m2)      #solved general equation for x
    y <- m1*x + b1            #plug in the result
    data.frame(x=round(x, rnd), y=round(y, rnd))
  }
}

###Determine 'No Change' and 'All-sprints' intersection point
NC_ALL_INTERSECTION <- lmIntx(NO_CHANGE_LM,ALL_SPRINT_LM)

###Determine 'No Change' and 'Last-5-sprints' intersection point
NC_LAST5_INTERSECTION <- lmIntx(NO_CHANGE_LM,LAST_5_SPRINT_LM)

###Determine 'No Change' and 'Last-5-sprints' intersection point
NC_LAST5AVG_INTERSECTION <- lmIntx(NO_CHANGE_LM,AVG_5_LM)


#################plot
par(oma = c(3, 1, 1, 1))
plot(f$SPRINT, f$COMPLETED_RELEASE_POINTS, main=paste(RELEASE_NAME,"Release Burn-up"), xlab="Sprint", ylab="Points", xaxt='n', ylim=c(0, sum(q$VELOCITY)+50))

###Add the 'No Change' horizontal line of total release points
abline(h=sum(q$VELOCITY),col="blue",lwd=2)

###add Last-5-sprint linear trendline
#abline(LAST_5_SPRINT_LM,col="red",lwd=2)

###add intersection of 'No Change' and 'Last-5-Sprints'
#abline(v=NC_LAST5_INTERSECTION$x,col="red",lwd=2, lty=2)

###add linear trendline for all sprints
#abline(ALL_SPRINT_LM,col="green",lwd=2)

###add intersection of 'No Change' and 'All Sprints'
#abline(v=NC_ALL_INTERSECTION$x,col="green",lwd=2, lty=2)

###Add 5-sprint avg velocity line
abline(AVG_5_LM,col="orange",lwd=2)

###add intersection of 'No Change' and 'Avg last 5'
abline(v=NC_LAST5AVG_INTERSECTION$x,col="orange",lwd=2, lty=2)


###add target release date/sprint
abline(v=TARGET_RELEASE_SPRINT,col="black",lwd=2, lty=2)

###Add axis
axis(1,labels=f$SPRINT, at=f$SPRINT, las=2)

# Add legend to bottom, outside plot region
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", 
       legend=c("No Scope Change"#,"Last-5-sprint linear trend","All-sprints linear trend"
                ,"Last-5-sprint average", "Target Release"), 
       lwd=c(2,2,2,2,2),
       lty=c(1,1,1,1,2),
       col=c("blue"#,"red","green"
             ,"orange","black"), 
       cex=0.75, 
       ncol=2, 
       bty="n", 
       xpd = TRUE
       )

