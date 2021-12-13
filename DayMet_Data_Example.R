
library(daymetr)
library(tidyverse)

## Test prediction

Fakedata <- data.frame(Latitude = 36.4,
                       Longitude = -85.6,
                       Year = 2019)

daymet <- download_daymet(site = "mysite",
                          lat = Fakedata$Latitude,
                          lon = Fakedata$Longitude,
                          start = 2018,
                          end = 2019,
                          internal = TRUE,
                          simplify = F) # returns tidy data!

Fakedata$JANMAXTEMP <- mean(daymet$data$tmax..deg.c.[366:396]) #2019
Fakedata$FEBMAXTEMP <- mean(daymet$data$tmax..deg.c.[397:424]) #2019
Fakedata$MARMAXTEMP <- mean(daymet$data$tmax..deg.c.[425:455]) #2019
Fakedata$Prcp <- sum(daymet$data$prcp..mm.day.[365:730]) #2019
Fakedata$AUGMAXTEMP <- mean(daymet$data$tmax..deg.c.[213:243]) #2018!!
Fakedata$JANMINTEMP <- mean(daymet$data$tmin..deg.c.[366:396]) #2019
Fakedata$FEBMINTEMP <- mean(daymet$data$tmin..deg.c.[397:424]) #2019
Fakedata$MARMINTEMP <- mean(daymet$data$tmin..deg.c.[425:455]) #2019

temp.out <- Fakedata[,c("Longitude", "Latitude")]

coordinates(temp.out) <- ~Longitude+Latitude

proj4string(temp.out) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

pts <- spTransform(temp.out,CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

Fakedata$FOREST <- extract(forests, y = pts)


pred.outbreak <- predict(m_gam3,Fakedata,type="response", link = logit)

plogis(pred.outbreak)


plot(test$Longitude, test$Latitude)

points(temp.out, col = "red")

# 2018
first.row$janMaxTemp <- mean(day.test$data$tmax..deg.c.[1:31]) #2018
first.row$janMinTemp <- mean(day.test$data$tmin..deg.c.[1:31]) #2018
first.row$janMeanVP <- mean(day.test$data$vp..Pa.[1:31]) #2018
first.row$julMaxTemp <- mean(day.test$data$tmax..deg.c.[182:212]) #2018
first.row$julMinTemp <- mean(day.test$data$tmin..deg.c.[182:212]) #2018
first.row$julMeanVP <- mean(day.test$data$vp..Pa.[182:212]) #2018
first.row$annPrecip <- sum(day.test$data$prcp..mm.day.[1:365]) #2018
# 2019
first.row$janMaxTemp2 <- mean(day.test$data$tmax..deg.c.[366:396]) #2019
first.row$janMinTemp2 <- mean(day.test$data$tmin..deg.c.[366:396]) #2019
first.row$janMeanVP2 <- mean(day.test$data$vp..Pa.[366:396]) #2019
first.row$julMaxTemp2 <- mean(day.test$data$tmax..deg.c.[547:577]) #2019
first.row$julMinTemp2 <- mean(day.test$data$tmin..deg.c.[547:577]) #2019
first.row$julMeanVP2 <- mean(day.test$data$vp..Pa.[547:577]) #2019
first.row$annPrecip2 <- sum(day.test$data$prcp..mm.day.[366:730]) #2019
