###########Open 3.0 defects by activity

##add dplyr library
library(dplyr)

###Read CSV
d <- read.csv("C:/users/u6033371/Desktop/EDG Kore Team.csv")

###grab open defects only
b <- subset(d, Work.Item.Type=='Bug' & State!='Done')

###Add dummy column
b <- mutate(b, bug_count=1)

###Sum the number of bugs by area path
b1 <- aggregate(b$bug_count, by=list(Category=b$Area.Path), FUN=sum)

View(b1)