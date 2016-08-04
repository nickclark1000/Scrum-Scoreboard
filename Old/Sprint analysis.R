######## Sprint Analysis

##add dplyr library
library(dplyr)

###Initialize list
g <- list()

###Define csv files
csvFiles <- c("C:/temp/burndown/Sprint 101_EffortDayDetail_050_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_051_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_052_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_053_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_054_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_055_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_056_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_057_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_058_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_059_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_060_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_061_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_062_0510113614.csv",
              "C:/temp/burndown/Sprint 101_EffortDayDetail_063_0510113614.csv")

dates <- c('2016-02-19',
           '2016-02-20',
           '2016-02-21',
           '2016-02-22',
           '2016-02-23',
           '2016-02-24',
           '2016-02-25',
           '2016-02-26',
           '2016-02-27',
           '2016-02-28',
           '2016-02-29',
           '2016-03-01',
           '2016-03-02',
           '2016-03-03')

data <- cbind(csvFiles, dates)

####Loop through each day
for (i in 1:length(data[,1])) {

  ###Read CSV
  d <- read.csv(data[i,"csvFiles"])
  
  ###Define date of sprint based on column name
  sprintDate <- as.Date(data[i,"dates"])
  
  ###Add Date column
  d <- mutate(d, Date=sprintDate)
  
  ###Rename columns
  colnames(d)[3] <- "Effort"
  colnames(d)[4] <- "Iteration"
  colnames(d)[5] <- "State"
  
  ###Append d to g as new rows
  g <- bind_rows(g, d)
}
