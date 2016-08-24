###Project data points until completion
projectedDataPoints <- function(initialSprint, initialTotal, predictionModel, dataFrame, totalReleasePoints) {
  ###Initialize variables
  LAST_SPRINT <- initialSprint
  FORECAST <- initialTotal
  
  ###Determine each subsequent forecasted point until you reach the NO_CHANGE line
  while(FORECAST < totalReleasePoints){
    LAST_SPRINT = LAST_SPRINT + 1
    FORECAST = predictionModel$coefficient[2]*LAST_SPRINT + predictionModel$coefficient[1]
    dataFrame <- bind_rows(dataFrame,data.frame(SPRINT = LAST_SPRINT,FORECASTED_RELEASE_POINTS = FORECAST))
  }
  return(dataFrame)
}


##########Determine intersection points
# Linear model Intercept function
lmIntx <- function(fit1, fit2, rnd=2) {
  b1<- fit1$coefficient[1]  #y-int for fit1
  m1<- fit1$coefficient[2]  #slope for fit1
  b2<- fit2$coefficient[1]  #y-int for fit2
  m2<- fit2$coefficient[2]  #slope for fit2
  cat(file=stderr(),"b1:",b1,"\n","m1:",m1,"\n","b2",b2,"\n","m2",m2,"\n")
  if(m1==m2 & b1==b2) {print("Lines are identical")
  } else if(m1==m2 & b1 != b2) {print("Lines are parallel")
  } else {
    x <- (b2-b1)/(m1-m2)      #solved general equation for x
    y <- m1*x + b1            #plug in the result
    data.frame(x=round(x, rnd), y=round(y, rnd))
  }
}

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}