library(foreign)
library(nlme)
library(plm)
library(tseries)
library(simcf)

data <- read.dta("C:/Users/arsell/Desktop/CharterInfoSetAnalysis.dta")

# SURVEYYEAR = year
# SCHOOLIDNCESASSIGNED = school id
# TOTALSTUDENTS = total students
# charteronehalfoverlap = charter school with 1.5 miles with overlapping grades
# chartertwohalfoverlap = charter school with 2.5 miles with overlapping grades
# charterfiveoverlap = charter school with 5 miles with overlapping grades
# chartertenoverlap = charter school with 10 miles with overlapping grades
# numchartersonehalfoverlap = total number of charters within 1.5 miles with overlapping grades
# numcharterstwohalfoverlap = total number of charters within 2.5 miles with overlapping grades
# numchartersfiveoverlap = total number of charters within 5 miles with overlapping grades
# numcharterstenoverlap = total number of charters within 10 miles with overlapping grades
# FRLpercent = percent of enrollment that is FRL eligible
# BLACKpercent = percent of enrollment that is black
# HISPANICpercent = percent of enrollment that is hispanic
# WHITEpercent = percent of enrollment that is white
# AGENCYNAMEDISTRICT = school district name
# SCHOOLNAME = school name

## rename variables
data$id <- data$SCHOOLIDNCESASSIGNED
data$year <- data$SURVEYYEAR


## create lags
DVs <- list(data$FRLpercent, data$BLACKpercent, data$HISPANICpercent, data$WHITEpercent, data$TOTALSTUDENTS)

data$FRLlag1 <- lagpanel(data$FRLpercent, data$id, data$year,1)
data$BLACKlag1 <- lagpanel(data$BLACKpercent, data$id, data$year,1)
data$HISPANIClag1 <- lagpanel(data$HISPANICpercent, data$id, data$year,1)
data$WHITElag1 <- lagpanel(data$WHITEpercent, data$id, data$year,1)
data$TOTALSTUDENTSlag1 <- lagpanel(data$TOTALSTUDENTS, data$id, data$year,1)

## model for total students

selectdata <- na.omit(cbind(data$id,
                            data$year,
                            data$TOTALSTUDENTS,
                            data$TOTALSTUDENTSlag1,
                            data$charteronehalfoverlap,
                            data$chartertwohalfoverlap,
                            data$charterfiveoverlap,
                            data$chartertenoverlap,
                            data$numchartersonehalfoverlap,
                            data$numcharterstwohalfoverlap,
                            data$numchartersfiveoverlap,
                            data$numcharterstenoverlap
                            ))

selectdata <- as.data.frame(selectdata)
names(selectdata) <- c("id",
                       "year",
                       "totalstudents",
                       "totalstudentslag1",
                       "charteronehalfoverlap",
                       "chartertwohalfoverlap",
                       "charterfiveoverlap",
                       "chartertenoverlap",
                       "numchartersonehalfoverlap",
                       "numcharterstwohalfoverlap",
                       "numchartersfiveoverlap",
                       "numcharterstenoverlap")

attach(selectdata)

schoolidlist <- unique(id)
fe <- makeFEdummies(id)
n <- length(schoolidlist)

for (i in 1:n){
  currschool <- schoolidlist[i]
  
}