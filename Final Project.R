library(foreign)
library(nlme)
library(plm)
library(tseries)
library(simcf)

data <- read.dta("Data/CharterInfoSetAnalysis.dta")

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
  plot(totalstudents[id==currschool], type="l", ylab="Total Students", xlab = "Year",
       main = paste("School", currschool))
}

for (i in 1:n){
  currschool <- schoolidlist[i]
  acf(totalstudents[id==currschool])
}

for (i in 1:n){
  currschool <- schoolidlist[i]
  pacf(totalstudents[id==currschool])
}

PPtest.pvalues <- rep(0,n)
adftest.pvalues <- rep(0,n)

for (i in 1:n){
  currschool <- schoolidlist[i]
  curPP <- try(PP.test(totalstudents[id==currschool])$p.value)
  if (any(class(curPP)=="try-error")) curPP <- NA
  PPtest.pvalues[i] <- curPP
  
  curadf <- try(adf.test(totalstudents[id==currschool])$p.value)
  if (any(class(curadf) == "try-error")) curadf <- NA
  adftest.pvalues[i] <- curadf
}

pdf("PPtest.pdf",width=6,height=3.25)
hist(PPtest.pvalues)          # Plot a histogram of the p-values
dev.off()

pdf("adftest.pdf",width=6,height=3.25)
hist(adftest.pvalues)         # Plot a histogram of the p-values
dev.off()

## PLM data frame

pdata <- pdata.frame(selectdata, index=c("id", "year"))

#random effects model - 5 miles
fit.r1 <- lme(fixed = totalstudents ~ charterfiveoverlap + factor(year),
            random = ~ 1 | id,
            correlation = corARMA(form = ~ year | id,
                                  p = 1,
                                  q = 0))
summary(fit.r1)

## fixed effects model - 5 miles
pvar(pdata)

fit.f1 <- plm(totalstudents ~ totalstudentslag1 + charterfiveoverlap + factor(year),
              data=pdata, model="within")
summary(fit.f1)

pbgtest(fit.f1)

robust <- "None"   # Choose var-cov estimator here
if (robust=="None") vc <- vcov(plm.res2)
if (robust=="Arellano") vc <- vcovHC(plm.res2)   # Arellano (1987) heteroskedastic and serial correlation robust VC
if (robust=="BeckKatz") vc <- vcovBK(plm.res2)   # Beck and Katz (1995) panel corrected VC
if (robust=="DriscollKraay") vc <- vcovSCC(plm.res2)   # Driscoll and Kraay panel corrected VC

pe.fit.f1 <- coef(fit.f1)
vc.fit.f1 <- vc
se.fit.f1 <- sqrt(diag(vc.fit.f1))
tstat.fit.f1 <- abs(pe.fit.f1/se.fit.f1)
df.fit.f1 <- rep(fit.f1$df.residual, length(tstat.fit.f1))
pval.fit.f1 <- 2*pt(tstat.fit.f1, df.fit.f1, lower.tail = FALSE)
