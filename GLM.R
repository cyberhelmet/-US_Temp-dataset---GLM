
## Read the US min Jan temperature data and show first 5 rows
d <- read.table("ustemp.data",header=T)
head(d)


## Scatterplot matrix + smoothed line
pairs(~ Temp + Lat + Lon,data=d,panel=panel.smooth)


## Regress Temp against Lat and Lon
fit <- lm(Temp ~ Lat + Lon,data=d)
summary(fit)

## Diagnostic plots for the fit
opar <- par(mfrow=c(2,2))
plot(fit)
par(opar)

d[c('Seattle, WA', 'Portland, OR'),]
fit$fitted[c('Seattle, WA', 'Portland, OR')]

## Partial residual plots
opar <- par(mfrow=c(1,2))
termplot(fit,partial.resid=T,se=T)
par(opar)

fit <- lm(Temp ~ Lat + Lon + I(Lon^2) + I(Lon^3) + I(Lon^4) + I(Lon^5),data=d)

#we can use the anova command (recall how it progressively tests nested models)
anova(fit)

## Reduce to a third order model
fit <- lm(Temp ~ Lat + Lon + I(Lon^2) + I(Lon^3),data=d)
summary(fit)

## Diagnostic plots for the fit
opar <- par(mfrow=c(2,2))
plot(fit)
par(opar)

## Plot surface as an image
## Define lat/lon range
lats <- seq(25,49,1)
lons <- seq(-125,-70,2.5)
grid <- expand.grid(Lon=lons,Lat=lats)

## Predict temperatures, and structure as a matrix
tmps <- matrix(predict(fit, grid),
               length(lons),length(lats))
## Construct image and label cities
image(lons,lats,tmps,col=heat.colors(64))
with(d,text(Lon,Lat,rownames(d),cex=0.6))
## Add coastline
#install.packages("maps")
library(maps)
map("usa",xlim=c(-125,-70),ylim=c(25,49),add=T)

## Initial model - Temp against Lat and Lon
fit <- lm(Temp ~ Lat + Lon,data=d)

## Single term additions - all second order terms
add1(fit, . ~ . + I(Lat^2) + I(Lat*Lon) + I(Lon^2),test="F")


## New best candidate
fit <- lm(Temp ~ Lat + Lon + I(Lon^2),data=d)

## Single term deletions - can only delete Lat as
drop1(fit, . ~ Lat,test="F")

add1(fit, . ~ . + I(Lat^2) + I(Lat*Lon),test="F")

## Go back to our initial model - Temp against Lat and Lon
fit <- lm(Temp ~ Lat + Lon,data=d)


## Fully automated selection chooses poorly formed model
sfit <- step(fit,scope=list(
  lower= . ~ 1,
  upper= . ~ Lat+Lon+I(Lat^2)+I(Lon^2)+I(Lat*Lon)+
    I(Lat^3)+I(Lat*Lon^2)+I(Lat^2*Lon)+I(Lat^3)))
summary(sfit)


## Force correctly formed model
sfit <- step(fit,scope=list(
  lower= . ~ Lat+Lon,
  upper= . ~ Lat+Lon+I(Lat^2)+I(Lon^2)+I(Lat*Lon)+
    I(Lat^3)+I(Lat*Lon^2)+I(Lat^2*Lon)+I(Lat^3)))
summary(sfit)


