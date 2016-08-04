#####Backlog Summary Data

#####User Input Variables

###Kore
FIRST_DAY_OF_FIRST_SPRINT <- "2015-01-08"
TEAM_NAME <- "EDG Kore"
BACKLOG_CSV <- "C:/users/u6033371/Desktop/EDG Kore Team.csv" ###location of csv file on disk
FIRST_SPRINT <- 72
CURRENT_SPRINT <- 110
CURRENT_RELEASE_IDENTIFIER <- "3.0 GA" ###Unique string in Iteration Path to identify release work items
RELEASE_IDENTIFIERS <- c("3.0 Alpha", "3.0 TPR", "3.0 Beta", "3.0 GA")

###Crusaders
#FIRST_DAY_OF_FIRST_SPRINT <- "2015-08-06"
#TEAM_NAME <- "EDG Digital Crusaders"
#BACKLOG_CSV <- "C:/users/u6033371/Desktop/EDG Crusaders Team.csv" ###location of csv file on disk
#FIRST_SPRINT <- 87
#CURRENT_SPRINT <- 110
#CURRENT_RELEASE_IDENTIFIER <- "3.0 GA" ###Unique string in Iteration Path to identify release work items
#RELEASE_IDENTIFIERS <- c("3.0 Alpha", "3.0 TPR", "3.0 Beta", "3.0 GA")

###EFR
#FIRST_DAY_OF_FIRST_SPRINT <- "2015-02-12"
#TEAM_NAME <- "3EFR SOLIDarity"
#BACKLOG_CSV <- "C:/users/u6033371/Desktop/3EFR Team.csv" ###location of csv file on disk
#FIRST_SPRINT <- 72

###load dplyr for data wrangling
library(dplyr)
###load TTR for calculating moving averages
library(TTR)

###Read backlog state CSV, skip the first row that TFS adds to excel sheet
d <- read.csv(BACKLOG_CSV, skip=1)

d$TEAM_NAME <- TEAM_NAME

###Remove everything to the left of Sprint
a <- sapply(strsplit(as.character(d$Iteration.Path), split='Sprint ', fixed=TRUE), function(x) (x[2]))

###Remove everything to the right of '(' and convert to numeric
d$SPRINT <- as.numeric(sapply(strsplit(a, split=' (', fixed=TRUE), function(x) (x[1])))

###Convert to Date format
d$CREATED_DATE<-as.Date(d$Created.Date,"%d/%m/%Y")

###Get all rows that contain each RELEASE_IDENTIFIER in the iteration path and add data to new RELEASE_NAME column
for (i in 1:length(RELEASE_IDENTIFIERS)){
  d[grep(RELEASE_IDENTIFIERS[i], d$Iteration.Path),"RELEASE_NAME"] <- RELEASE_IDENTIFIERS[i]
}

TOTAL_RELEASE_POINTS <- sum(subset(d,RELEASE_NAME==CURRENT_RELEASE_IDENTIFIER)$Effort, na.rm=TRUE)

d <- subset(d, SPRINT >= FIRST_SPRINT | is.na(SPRINT))

###Group by iteration path and set the column names
v<- setNames(aggregate(d$Effort, by=list(SPRINT=d$SPRINT), FUN=sum, na.rm=TRUE), c("SPRINT","VELOCITY"))

###Add release name
s<-unique(d[c("SPRINT","RELEASE_NAME")])
v<-left_join(v,s,by="SPRINT")


###Define sprint indexes (re-index to 0 at the beginning of each sprint)
v$SPRINT_INDEX=0
for (i in 2:nrow(v)){
  if(v$RELEASE_NAME[i]==v$RELEASE_NAME[i-1])
    v$SPRINT_INDEX[i] = v$SPRINT_INDEX[i-1]+1
  else
    v$SPRINT_INDEX[i]=0
}

###Calculate total work done over time by looping through each sprint.
###Measured at the END of each sprint

v$COMPLETED_RELEASE_POINTS[1]=v$VELOCITY[1]

for (i in 2:(nrow(v))) {
  if(v$RELEASE_NAME[i-1]==v$RELEASE_NAME[i])
    v$COMPLETED_RELEASE_POINTS[i] = v$COMPLETED_RELEASE_POINTS[i-1] + v$VELOCITY[i]
  else
    v$COMPLETED_RELEASE_POINTS[i] = v$VELOCITY[i]
}

###Compute and append Simple Moving Average - 5
v<-mutate(v,SMA_5=SMA(v$VELOCITY, 5))

###Compute and append Weighted Moving Average - 5
v<-mutate(v,WMA_5=WMA(v$VELOCITY, 5))


###Hacky date dataframe since the date type won't persist if you write directly to v
START_DATE <- as.Date(FIRST_DAY_OF_FIRST_SPRINT)
END_DATE <- START_DATE+13
dates<-data.frame(START_DATE=START_DATE,END_DATE=END_DATE)

###Add the start and end sprint dates
for (i in 1:(nrow(v)-1)) {
  START_DATE = START_DATE+14
  END_DATE = END_DATE+14
  dates<-bind_rows(dates,data.frame(START_DATE=START_DATE,END_DATE=END_DATE))
}

v <- bind_cols(v,dates)

v$TEAM_NAME <- TEAM_NAME


defects <- subset(d,Work.Item.Type=="Bug")
defects <- mutate(defects, COUNT=1)
defects_d <- setNames(aggregate(defects$Effort, by=list(SPRINT=defects$SPRINT), FUN=sum, na.rm=TRUE), c("SPRINT","DEFECTS_COMPLETED_POINTS"))
defects_c <- setNames(aggregate(defects$COUNT, by=list(SPRINT=defects$SPRINT), FUN=sum, na.rm=TRUE), c("SPRINT","DEFECTS_COMPLETED_COUNT"))
v<-full_join(v,defects_d,by="SPRINT")
v<-full_join(v,defects_c,by="SPRINT")

pbis <- subset(d,Work.Item.Type=="Product Backlog Item")
pbis <- mutate(pbis, COUNT=1)
pbis_d <- setNames(aggregate(pbis$Effort, by=list(SPRINT=pbis$SPRINT), FUN=sum, na.rm=TRUE), c("SPRINT","PBIS_COMPLETED_POINTS"))
pbis_c <- setNames(aggregate(pbis$COUNT, by=list(SPRINT=pbis$SPRINT), FUN=sum, na.rm=TRUE), c("SPRINT","PBIS_COMPLETED_COUNT"))
v<-full_join(v,pbis_d,by="SPRINT")
v<-full_join(v,pbis_c,by="SPRINT")

b<-data.frame(DEFECTS_CREATED_COUNT=integer(0),DEFECTS_CREATED_POINTS=integer(0),PBIS_CREATED_COUNT=integer(0),PBIS_CREATED_POINTS=integer(0))
for (i in 1:dim(v)[1]) {
  a<-subset(defects, CREATED_DATE>= v$START_DATE[i] & CREATED_DATE<= v$END_DATE[i])
  c<-subset(pbis, CREATED_DATE>= v$START_DATE[i] & CREATED_DATE<= v$END_DATE[i])
  
  b<-bind_rows(b,data.frame(DEFECTS_CREATED_COUNT=dim(a)[1],DEFECTS_CREATED_POINTS=sum(a$Effort, na.rm = TRUE), PBIS_CREATED_COUNT=dim(c)[1], PBIS_CREATED_POINTS=sum(c$Effort, na.rm = TRUE)))
 }

v <- bind_cols(v,b)

v<-subset(v, SPRINT<CURRENT_SPRINT)

###Reorder columns
v<-v[,c("TEAM_NAME","RELEASE_NAME","SPRINT","SPRINT_INDEX","START_DATE","END_DATE","VELOCITY","SMA_5","WMA_5","COMPLETED_RELEASE_POINTS", "DEFECTS_COMPLETED_COUNT","DEFECTS_COMPLETED_POINTS","DEFECTS_CREATED_COUNT","DEFECTS_CREATED_POINTS","PBIS_COMPLETED_COUNT","PBIS_COMPLETED_POINTS","PBIS_CREATED_COUNT","PBIS_CREATED_POINTS")]