library(ggplot2)
southBrisTotal<- read.csv("southbrisbane-aq-2018.csv")
weatherTotal<- read.csv("weatherAUS.csv")
#class(southBrisTotal$Time)
southBris<-subset(southBrisTotal,!is.na(southBrisTotal$Date))
southBris$Date<-as.Date(southBris$Date, format="%d/%m/%Y")
weatherTotal$Date<-as.Date(weatherTotal$Date, format="%Y-%m-%d")

southBris9am<-subset(southBris,southBris$Time=="09:00")
summary(southBris9am)
southBris3pm<-subset(southBris,southBris$Time=="15:00")
#names(southBris9am)
predictablesAirQualityNames<-c("Relative.Humidity....", "Nitrogen.Oxide..ppm.", "Nitrogen.Dioxide..ppm.", "Nitrogen.Oxides..ppm.", "Carbon.Monoxide..ppm.", "PM10..ug.m.3.", "PM2.5..ug.m.3.")
weatherTotal<-subset(weatherTotal, weatherTotal$Location=="Brisbane")
weather9am<-data.frame("Date"= weatherTotal$Date, weatherTotal$MinTemp, weatherTotal$MaxTemp, weatherTotal$Rainfall, weatherTotal$Evaporation,
                       weatherTotal$Sunshine, weatherTotal$WindGustDir, weatherTotal$WindGustSpeed, weatherTotal$RainToday,
                       weatherTotal$RISK_MM, weatherTotal$RainTomorrow,
                       weatherTotal$WindDir9am, weatherTotal$Humidity9am, weatherTotal$Pressure9am,
                       weatherTotal$Cloud9am, weatherTotal$Temp9am)
weather3pm<-data.frame("Date"= weatherTotal$Date, weatherTotal$MinTemp, weatherTotal$MaxTemp, weatherTotal$Rainfall, weatherTotal$Evaporation,
                       weatherTotal$Sunshine, weatherTotal$WindGustDir, weatherTotal$WindGustSpeed, weatherTotal$RainToday,
                       weatherTotal$RISK_MM, weatherTotal$RainTomorrow,
                       weatherTotal$WindDir3pm, weatherTotal$Humidity3pm, weatherTotal$Pressure3pm,
                       weatherTotal$Cloud3pm, weatherTotal$Temp3pm)
#names(weather9am)
#names(southBris9am)

total9am<- merge(weather9am,southBris9am,by="Date")
total3pm<- merge(weather3pm,southBris3pm,by="Date")
total9am$time<-"9am"
total3pm$time<-"3pm"
names(total3pm)<-c("Date","MinTemp","MaxTemp","Rainfall","Evaporation","Sunshine","WindGustDir","WindGustSpeed","RainToday","RISK_MM","RainTomorrow","WindDir","Humidity","Pressure","Cloud","Temp","Time","Wind.Direction..degTN.","Wind.Speed..m.s.","Wind.Sigma.Theta..deg.","Wind.Speed.Std.Dev..m.s.","Air.Temperature..degC.","Relative.Humidity....","Nitrogen.Oxide..ppm.","Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.","Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
names(total9am)<-c("Date","MinTemp","MaxTemp","Rainfall","Evaporation","Sunshine","WindGustDir","WindGustSpeed","RainToday","RISK_MM","RainTomorrow","WindDir","Humidity","Pressure","Cloud","Temp","Time","Wind.Direction..degTN.","Wind.Speed..m.s.","Wind.Sigma.Theta..deg.","Wind.Speed.Std.Dev..m.s.","Air.Temperature..degC.","Relative.Humidity....","Nitrogen.Oxide..ppm.","Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.","Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
totalData<-rbind(total9am,total3pm)
summary(totalData)

i<-"Sunshine"
ggplot(data = total9am,aes(x = total9am$Date, y = total9am[[i]]))+
  geom_point()+
  geom_smooth()
max<-0

  for(j in 3:(nrow(total9am)-3)){
    #print(southBris9am[[predictablesAirQualityNames]][1])
      k<-mean(c(total9am[[i]][j-1], total9am[[i]][j+1],total9am[[i]][j-2], total9am[[i]][j+2]))
      if(max<(total9am[[i]][j]-k)){
        max<-total9am[[i]][j]-k
      }
  }
print(i)
print(max)

