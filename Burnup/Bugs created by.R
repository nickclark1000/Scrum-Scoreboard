###########Bugs created by 

##add dplyr library
library(dplyr)

###Read CSV
d <- read.csv("C:/users/u6033371/Desktop/EDG Kore Team.csv")

###Convert to R date object
d$Created.Date <- as.Date(d$Created.Date, format="%d/%m/%Y")

###Subset of bugs from the past year
b <- subset(d, d$Created.Date > '2015-04-27' & Work.Item.Type=='Bug')

###Add dummy column
b <- mutate(b, count=1)

###Sum the number of bugs created by person
b1 <- aggregate(count ~ Created.By, data=b, FUN=length)

View(b1)