###load dplyr for data wrangling
library(dplyr)
###load TTR for calculating moving averages
library(TTR)

RELEASE_IDENTIFIERS <- c(CURRENT_RELEASE_IDENTIFIER)

d$TEAM_NAME <- TEAM_NAME


###Remove everything to the left of Sprint, to the right of '(' and convert to numeric
a <- sapply(strsplit(as.character(d$Iteration.Path), split='Sprint ', fixed=TRUE), function(x) (x[2]))
d$SPRINT <- as.numeric(sapply(strsplit(a, split=' (', fixed=TRUE), function(x) (x[1])))


d$CREATED_DATE<-as.Date(d$Created.Date,"%d/%m/%Y")


for (i in 1:length(RELEASE_IDENTIFIERS)){
  d[grep(RELEASE_IDENTIFIERS[i], d$Iteration.Path),"RELEASE_NAME"] <- RELEASE_IDENTIFIERS[i]
}


d <- subset(d, SPRINT >= FIRST_SPRINT | is.na(SPRINT))


v<- setNames(aggregate(d$Effort, by=list(SPRINT=d$SPRINT), FUN=sum, na.rm=TRUE), c("SPRINT","VELOCITY"))


s<-unique(d[c("SPRINT","RELEASE_NAME")])
v<-left_join(v,s,by="SPRINT")


v$SPRINT_INDEX=0
for (i in 2:nrow(v)){
  if(v$RELEASE_NAME[i]==v$RELEASE_NAME[i-1] & !is.na(v$RELEASE_NAME[i]) & !is.na(v$RELEASE_NAME[i-1]))
    v$SPRINT_INDEX[i] = v$SPRINT_INDEX[i-1]+1
  else
    v$SPRINT_INDEX[i]=0
}

###Total work done measured at the END of each sprint
v$COMPLETED_RELEASE_POINTS[1]=v$VELOCITY[1]

for (i in 2:(nrow(v))) {
  if(v$RELEASE_NAME[i-1]==v$RELEASE_NAME[i] & !is.na(v$RELEASE_NAME[i]) & !is.na(v$RELEASE_NAME[i-1]))
    v$COMPLETED_RELEASE_POINTS[i] = v$COMPLETED_RELEASE_POINTS[i-1] + v$VELOCITY[i]
  else
    v$COMPLETED_RELEASE_POINTS[i] = v$VELOCITY[i]
}
cat(file=stderr(),"nrow:",nrow(v),"\n")
if(nrow(v)>4){
  v<-mutate(v,VELOCITY_SMA_5=SMA(v$VELOCITY, 5))
  
  
  v<-mutate(v,VELOCITY_WMA_5=WMA(v$VELOCITY, 5))
} else {
  v<-mutate(v,VELOCITY_SMA_5=SMA(v$VELOCITY, nrow(v)-1))
  
  
  v<-mutate(v,VELOCITY_WMA_5=WMA(v$VELOCITY, nrow(v)-1))
}



###Hacky date dataframe since the date type won't persist if you write directly to v
START_DATE <- as.Date(FIRST_DAY_OF_FIRST_SPRINT)
END_DATE <- START_DATE+13
dates<-data.frame(START_DATE=START_DATE,END_DATE=END_DATE)


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

INPUT_DATA$TOTAL_RELEASE_POINTS = rowSums(cbind(INPUT_DATA$TOTAL_RELEASE_PBI_POINTS, INPUT_DATA$TOTAL_RELEASE_DEFECT_POINTS, INPUT_DATA$TOTAL_RELEASE_WORKORDER_POINTS), na.rm=TRUE)
v<-full_join(v,INPUT_DATA,by="SPRINT")

v<-subset(v, SPRINT<CURRENT_SPRINT)


#v<-v[,c("TEAM_NAME","RELEASE_NAME","SPRINT","SPRINT_INDEX","START_DATE","END_DATE","VELOCITY","VELOCITY_SMA_5","VELOCITY_WMA_5","COMPLETED_RELEASE_POINTS", "DEFECTS_COMPLETED_COUNT","DEFECTS_COMPLETED_POINTS","DEFECTS_CREATED_COUNT","DEFECTS_CREATED_POINTS","PBIS_COMPLETED_COUNT","PBIS_COMPLETED_POINTS","PBIS_CREATED_COUNT","PBIS_CREATED_POINTS")]