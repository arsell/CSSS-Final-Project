library(foreign)
library(nlme)      # Estimation of mixed effects models
library(lme4)      # Alternative package for mixed effects models
library(plm)       # Econometrics package for linear panel models
library(arm)       # Gelman & Hill code for mixed effects simulation
library(pcse)      # Calculate PCSEs for LS models (Beck & Katz)
library(tseries)   # For ADF unit root test
library(simcf)     # For panel functions and simulators
library(MASS)

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

# data <- subset(data, data$AGENCYNAMEDISTRICT=="OKLAHOMA CITY" | data$AGENCYNAMEDISTRICT=="TULSA")


## create lags
DVs <- list(data$FRLpercent, data$BLACKpercent, data$HISPANICpercent, data$WHITEpercent, data$TOTALSTUDENTS)

data$FRLlag1 <- lagpanel(data$FRLpercent, data$id, data$year,1)
data$BLACKlag1 <- lagpanel(data$BLACKpercent, data$id, data$year,1)
data$HISPANIClag1 <- lagpanel(data$HISPANICpercent, data$id, data$year,1)
data$HISPANIClag2 <- lagpanel(data$HISPANICpercent, data$id, data$year,2)
data$HISPANIClag3 <- lagpanel(data$HISPANICpercent, data$id, data$year,3)
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
                            data$numcharterstenoverlap,
                            data$HISPANICpercent,
                            data$HISPANIClag1,
                            data$FRLpercent,
                            data$FRLlag1,
                            data$WHITEpercent,
                            data$WHITElag1,
                            data$BLACKpercent,
                            data$BLACKlag1))

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
                       "numcharterstenoverlap",
                       "HISPANICpercent",
                       "HISPANIClag1",
                       "FRLpercent",
                       "FRLlag1",
                       "WHITEpercent",
                       "WHITElag1",
                       "BLACKpercent",
                       "BLACKlag1")

attach(selectdata)

schoolidlist <- unique(id)
fe <- makeFEdummies(id)
n <- length(schoolidlist)

for (i in 1:n){
  currschool <- schoolidlist[i]
  plot(HISPANICpercent[id==currschool], type="l", ylab="Total Students", xlab = "Year",
       main = paste("School", currschool))
}

for (i in 1:n){
  currschool <- schoolidlist[i]
  acf(HISPANICpercent[id==currschool])
}

for (i in 1:n){
  currschool <- schoolidlist[i]
  pacf(HISPANICpercent[id==currschool])
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

fit.f1 <- plm(FRLpercent ~ FRLlag1 + chartertenoverlap,
              data=pdata, model="within")
summary(fit.f1)

fit.f2 <- plm(HISPANICpercent ~ HISPANIClag1 + chartertenoverlap,
              data=pdata, model="within")

fit.f3 <- plm(BLACKpercent ~ BLACKlag1 + chartertenoverlap,
              data=pdata, model="within")

fit.f4 <- plm(WHITEpercent ~ WHITElag1 + chartertenoverlap,
              data=pdata, model="within")

pbgtest(fit.f1)

robust <- "Arellano"   # Choose var-cov estimator here
if (robust=="None") vc <- vcov(fit.f1)
if (robust=="Arellano") vc <- vcovHC(fit.f1)   # Arellano (1987) heteroskedastic and serial correlation robust VC
if (robust=="BeckKatz") vc <- vcovBK(fit.f1)   # Beck and Katz (1995) panel corrected VC
if (robust=="DriscollKraay") vc <- vcovSCC(fit.f1)   # Driscoll and Kraay panel corrected VC

pe.fit.f1 <- coef(fit.f1)
vc.fit.f1 <- vc
se.fit.f1 <- sqrt(diag(vc.fit.f1))
tstat.fit.f1 <- abs(pe.fit.f1/se.fit.f1)
df.fit.f1 <- rep(fit.f1$df.residual, length(tstat.fit.f1))
pval.fit.f1 <- 2*pt(tstat.fit.f1, df.fit.f1, lower.tail = FALSE)
fe.fit.f1 <- fixef(fit.f1)
resid.fit.f1 <- resid(fit.f1)

pe.fit.f2 <- coef(fit.f2)
vc.fit.f2 <- vc
se.fit.f2 <- sqrt(diag(vc.fit.f2))
tstat.fit.f2 <- abs(pe.fit.f2/se.fit.f2)
df.fit.f2 <- rep(fit.f2$df.residual, length(tstat.fit.f2))
pval.fit.f2 <- 2*pt(tstat.fit.f2, df.fit.f2, lower.tail = FALSE)
fe.fit.f2 <- fixef(fit.f2)
resid.fit.f2 <- resid(fit.f2)

pe.fit.f3 <- coef(fit.f3)
vc.fit.f3 <- vc
se.fit.f3 <- sqrt(diag(vc.fit.f3))
tstat.fit.f3 <- abs(pe.fit.f2/se.fit.f3)
df.fit.f3 <- rep(fit.f3$df.residual, length(tstat.fit.f3))
pval.fit.f3 <- 2*pt(tstat.fit.f3, df.fit.f3, lower.tail = FALSE)
fe.fit.f3 <- fixef(fit.f3)
resid.fit.f3 <- resid(fit.f3)

pe.fit.f4 <- coef(fit.f4)
vc.fit.f4 <- vc
se.fit.f4 <- sqrt(diag(vc.fit.f4))
tstat.fit.f4 <- abs(pe.fit.f4/se.fit.f4)
df.fit.f4 <- rep(fit.f4$df.residual, length(tstat.fit.f4))
pval.fit.f4 <- 2*pt(tstat.fit.f4, df.fit.f4, lower.tail = FALSE)
fe.fit.f4 <- fixef(fit.f4)
resid.fit.f4 <- resid(fit.f4)

## Interpret through simulation

sims <- 1000
simparam <- mvrnorm(sims, pe.fit.f1, vc.fit.f1)
simphi <- simparam[,1]
simbetas <- cbind(rep(mean(fe.fit.f1),sims),simparam[,2:ncol(simparam)])

formula <- "FRLpercent ~ chartertenoverlap"
formula <- as.formula(formula)

simparam2 <- mvrnorm(sims, pe.fit.f2, vc.fit.f2)
simphi2 <- simparam2[,1]
simbetas2 <- cbind(rep(mean(fe.fit.f2),sims),simparam2[,2:ncol(simparam2)])

formula2 <- "HISPANICpercent ~ chartertenoverlap"
formula2 <- as.formula(formula2)

simparam3 <- mvrnorm(sims, pe.fit.f3, vc.fit.f3)
simphi3 <- simparam3[,1]
simbetas3 <- cbind(rep(mean(fe.fit.f3),sims),simparam3[,2:ncol(simparam3)])

formula3 <- "BLACKpercent ~ chartertenoverlap"
formula3 <- as.formula(formula3)

simparam4 <- mvrnorm(sims, pe.fit.f4, vc.fit.f4)
simphi4 <- simparam4[,1]
simbetas4 <- cbind(rep(mean(fe.fit.f4),sims),simparam4[,2:ncol(simparam4)])

formula4 <- "WHITEpercent ~ chartertenoverlap"
formula4 <- as.formula(formula4)


periods.out <- 5
xhyp <- cfMake(formula,pdata,periods.out)
for (i in 1:periods.out){
  xhyp <- cfChange(xhyp, "chartertenoverlap", x=1, scen=i, xpre=0)
}

xhyp2 <- cfMake(formula2,pdata,periods.out)
for (i in 1:periods.out){
  xhyp2 <- cfChange(xhyp2, "chartertenoverlap", x=1, scen=i, xpre=0)
}

xhyp3 <- cfMake(formula3,pdata,periods.out)
for (i in 1:periods.out){
  xhyp3 <- cfChange(xhyp3, "chartertenoverlap", x=1, scen=i, xpre=0)
}

xhyp4 <- cfMake(formula4,pdata,periods.out)
for (i in 1:periods.out){
  xhyp4 <- cfChange(xhyp4, "chartertenoverlap", x=1, scen=i, xpre=0)
}

phi <- mean(simphi)
lagY <- mean(FRLlag1)
initialY <- mean(FRLpercent)

phi2 <- mean(simphi2)
lagY2 <- mean(HISPANIClag1)
initialY2 <- mean(HISPANICpercent)

phi3 <- mean(simphi3)
lagY3 <- mean(BLACKlag1)
initialY3 <- mean(BLACKpercent)

phi4 <- mean(simphi4)
lagY4 <- mean(WHITElag1)
initialY4 <- mean(WHITEpercent)

sim.ev2 <- ldvsimev(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=NA,        # NA indicates no constant!
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY,          # lags of y, most recent last
                    initialY=initialY   # for differenced models, the lag of the level of y
)

sim.ev2.2 <- ldvsimev(xhyp2,               # The matrix of hypothetical x's
                    simbetas2,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=NA,        # NA indicates no constant!
                    phi=phi2,            # estimated AR parameters; length must match lagY 
                    lagY=lagY2,          # lags of y, most recent last
                    initialY=initialY2   # for differenced models, the lag of the level of y
)

sim.ev2.3 <- ldvsimev(xhyp3,               # The matrix of hypothetical x's
                      simbetas3,           # The matrix of simulated betas
                      ci=0.95,            # Desired confidence interval
                      constant=NA,        # NA indicates no constant!
                      phi=phi3,            # estimated AR parameters; length must match lagY 
                      lagY=lagY3,          # lags of y, most recent last
                      initialY=initialY3   # for differenced models, the lag of the level of y
)

sim.ev2.4 <- ldvsimev(xhyp4,               # The matrix of hypothetical x's
                      simbetas4,           # The matrix of simulated betas
                      ci=0.95,            # Desired confidence interval
                      constant=NA,        # NA indicates no constant!
                      phi=phi4,            # estimated AR parameters; length must match lagY 
                      lagY=lagY4,          # lags of y, most recent last
                      initialY=initialY4   # for differenced models, the lag of the level of y
)

sim.fd2 <- ldvsimfd(xhyp,               # The matrix of hypothetical x's
                    simbetas,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=1,         # Column containing the constant
                    # set to NA for no constant
                    phi=phi,            # estimated AR parameters; length must match lagY 
                    lagY=lagY         # lags of y, most recent last
)

sim.fd2.2 <- ldvsimfd(xhyp2,               # The matrix of hypothetical x's
                    simbetas2,           # The matrix of simulated betas
                    ci=0.95,            # Desired confidence interval
                    constant=1,         # Column containing the constant
                    # set to NA for no constant
                    phi=phi2,            # estimated AR parameters; length must match lagY 
                    lagY=lagY2         # lags of y, most recent last
)

sim.fd2.3 <- ldvsimfd(xhyp3,               # The matrix of hypothetical x's
                      simbetas3,           # The matrix of simulated betas
                      ci=0.95,            # Desired confidence interval
                      constant=1,         # Column containing the constant
                      # set to NA for no constant
                      phi=phi3,            # estimated AR parameters; length must match lagY 
                      lagY=lagY3         # lags of y, most recent last
)

sim.fd2.4 <- ldvsimfd(xhyp4,               # The matrix of hypothetical x's
                      simbetas4,           # The matrix of simulated betas
                      ci=0.95,            # Desired confidence interval
                      constant=1,         # Column containing the constant
                      # set to NA for no constant
                      phi=phi4,            # estimated AR parameters; length must match lagY 
                      lagY=lagY4         # lags of y, most recent last
)



# Fixed effects model of change in GDP given increase in EDT

pdf("simevFEupALL.pdf",width=5,height=4.5)
plot.new()
par(usr=c(1,7,0,70))
axis(1,at=seq(1,5,1))
axis(2,at=seq(0,70,10))
title(xlab="Time",ylab="Expected % enrollment",
      main="Post-Estimation Simulation of \nCharter School Locating Nearby") 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.ev2$lower,
           rev(sim.ev2$upper),
           sim.ev2$lower[1])

ypoly2 <- c(sim.ev2.2$lower,
           rev(sim.ev2.2$upper),
           sim.ev2.2$lower[1])

ypoly3 <- c(sim.ev2.3$lower,
            rev(sim.ev2.3$upper),
            sim.ev2.3$lower[1])

ypoly4 <- c(sim.ev2.4$lower,
            rev(sim.ev2.4$upper),
            sim.ev2.4$lower[1])


# Choose the color of the polygon 
col <- "lightgreen"
col2 <- "lightblue"
col3 <- "lightpink"
col4 <- "lightgoldenrod"


# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly,
        col=col,
        border=FALSE
)

polygon(x=xpoly,
        y=ypoly2,
        col=col2,
        border=FALSE
)

polygon(x=xpoly,
        y=ypoly3,
        col=col3,
        border=FALSE
)

polygon(x=xpoly,
        y=ypoly4,
        col=col4,
        border=FALSE
)

# Plot the fitted line
lines(x=1:periods.out,y=sim.ev2$pe,col="darkgreen")
lines(x=c(0,5),y=c(0,0),lty="solid")

lines(x=1:periods.out,y=sim.ev2.2$pe,col="darkblue")
lines(x=c(0,5),y=c(0,0),lty="solid")

lines(x=1:periods.out,y=sim.ev2.3$pe,col="darkred")
lines(x=c(0,5),y=c(0,0),lty="solid")

lines(x=1:periods.out,y=sim.ev2.4$pe,col="darkgoldenrod")
lines(x=c(0,5),y=c(0,0),lty="solid")

legend("right", legend = c("FRL", "White", "Black", "Hispanic"),
       col=c("darkgreen", "darkgoldenrod", "darkred", "darkblue"),
       lty=1,
       box.lty=0)

dev.off()

## 2x2 FRL

pdf("simfdFEupFRL.pdf", width = 5, height = 4.5)
plot.new()
par(usr=c(1,5,-10, 10))
axis(1,at=seq(1,5,1))
axis(2,at=seq(-10,10,2))
title(xlab="Time",ylab="Expected change in % enrollment") 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.fd2$lower,
           rev(sim.fd2$upper),
           sim.fd2$lower[1])

ypoly2 <- c(sim.fd2.2$lower,
            rev(sim.fd2.2$upper),
            sim.fd2.2$lower[1])

ypoly3 <- c(sim.fd2.3$lower,
            rev(sim.fd2.3$upper),
            sim.fd2.3$lower[1])

ypoly4 <- c(sim.fd2.4$lower,
            rev(sim.fd2.4$upper),
            sim.fd2.4$lower[1])

# Choose the color of the polygon 
col <- "lightgreen"
col2 <- "lightblue"
col3 <- "lightpink"
col4 <- "lightgoldenrod"


# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly,
        col=col,
        border=FALSE
)

# Plot the fitted line
lines(x=1:periods.out,y=sim.fd2$pe,col="darkgreen")
lines(x=c(0,5),y=c(0,0),lty="solid")

dev.off()

## 2x2 Hispanic

pdf("simfdFEupHISPANIC.pdf", width = 5, height = 4.5)
plot.new()
par(usr=c(1,5,-10, 10))
axis(1,at=seq(1,5,1))
title(xlab="Time",ylab=NULL) 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.fd2$lower,
           rev(sim.fd2$upper),
           sim.fd2$lower[1])

ypoly2 <- c(sim.fd2.2$lower,
            rev(sim.fd2.2$upper),
            sim.fd2.2$lower[1])

ypoly3 <- c(sim.fd2.3$lower,
            rev(sim.fd2.3$upper),
            sim.fd2.3$lower[1])

ypoly4 <- c(sim.fd2.4$lower,
            rev(sim.fd2.4$upper),
            sim.fd2.4$lower[1])

# Choose the color of the polygon 
col <- "lightgreen"
col2 <- "lightblue"
col3 <- "lightpink"
col4 <- "lightgoldenrod"


# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly2,
        col=col2,
        border=FALSE
)

# Plot the fitted line
lines(x=1:periods.out,y=sim.fd2.2$pe,col="darkblue")
lines(x=c(0,5),y=c(0,0),lty="solid")

dev.off()

## 2x2 Black

pdf("simfdFEupBLACK.pdf", width = 5, height = 4.5)
plot.new()
par(usr=c(1,5,-10, 10))
axis(1,at=seq(1,5,1))
axis(2,at=seq(-10,10,2))
title(xlab="Time",ylab="Expected % enrollment") 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.fd2$lower,
           rev(sim.fd2$upper),
           sim.fd2$lower[1])

ypoly2 <- c(sim.fd2.2$lower,
            rev(sim.fd2.2$upper),
            sim.fd2.2$lower[1])

ypoly3 <- c(sim.fd2.3$lower,
            rev(sim.fd2.3$upper),
            sim.fd2.3$lower[1])

ypoly4 <- c(sim.fd2.4$lower,
            rev(sim.fd2.4$upper),
            sim.fd2.4$lower[1])

# Choose the color of the polygon 
col <- "lightgreen"
col2 <- "lightblue"
col3 <- "lightpink"
col4 <- "lightgoldenrod"


# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly3,
        col=col3,
        border=FALSE
)

# Plot the fitted line
lines(x=1:periods.out,y=sim.fd2.3$pe,col="darkred")
lines(x=c(0,5),y=c(0,0),lty="solid")

dev.off()

## 2x2 White

pdf("simfdFEupWhite.pdf", width = 5, height = 4.5)
plot.new()
par(usr=c(1,5,-10, 10))
axis(1,at=seq(1,5,1))
title(xlab="Time",ylab=NULL) 

# Make the x-coord of a confidence envelope polygon
xpoly <- c(1:periods.out,
           rev(1:periods.out),
           1)

# Make the y-coord of a confidence envelope polygon
ypoly <- c(sim.fd2$lower,
           rev(sim.fd2$upper),
           sim.fd2$lower[1])

ypoly2 <- c(sim.fd2.2$lower,
            rev(sim.fd2.2$upper),
            sim.fd2.2$lower[1])

ypoly3 <- c(sim.fd2.3$lower,
            rev(sim.fd2.3$upper),
            sim.fd2.3$lower[1])

ypoly4 <- c(sim.fd2.4$lower,
            rev(sim.fd2.4$upper),
            sim.fd2.4$lower[1])

# Choose the color of the polygon 
col <- "lightgreen"
col2 <- "lightblue"
col3 <- "lightpink"
col4 <- "lightgoldenrod"


# Plot the polygon first, before the points & lines
polygon(x=xpoly,
        y=ypoly4,
        col=col4,
        border=FALSE
)

# Plot the fitted line
lines(x=1:periods.out,y=sim.fd2.4$pe,col="darkgoldenrod")
lines(x=c(0,5),y=c(0,0),lty="solid")

dev.off()