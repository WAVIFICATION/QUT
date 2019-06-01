library(ggplot2)
southBrisTotal<- read.csv("/Users/nikhilnair/GitHub/QUT/project_IFN509/southbrisbane-aq-2018.csv")
weatherTotal<- read.csv("/Users/nikhilnair/GitHub/QUT/project_IFN509/weatherAUS.csv")
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



#i<-"Sunshine"
#ggplot(data = total9am,aes(x = total9am$Date, y = total9am[[i]]))+
#  geom_point()+
#  geom_smooth()
#max<-0

#
#
# Start_Data_Cleaning_Nikhil_Tissa

#Hard_Reject_Unpredictable_Column_values
#Tissa
#Weather9am

unpredictableweatherqualities<-c("weatherTotal.Cloud9am","weatherTotal.Pressure9am","weatherTotal.Humidity9am","weatherTotal.WindDir9am","weatherTotal.RainTomorrow","weatherTotal.RISK_MM","weatherTotal.RainToday","weatherTotal.WindGustSpeed","weatherTotal.Sunshine","weatherTotal.Evaporation","weatherTotal.Rainfall")
for (i in (unpredictableweatherqualities))
{
  
  weather9am <- weather9am[complete.cases(weather9am[[i]]),]
}

#
#Weather3pm
#Tissa
unpredictableweatherqualities<-c("weatherTotal.Cloud3pm","weatherTotal.Pressure3pm","weatherTotal.Humidity3pm","weatherTotal.WindDir3pm","weatherTotal.RainTomorrow","weatherTotal.RISK_MM","weatherTotal.RainToday","weatherTotal.WindGustSpeed","weatherTotal.Sunshine","weatherTotal.Evaporation","weatherTotal.Rainfall")
for (i in (unpredictableweatherqualities))
{
  
  weather3pm <- weather3pm[complete.cases(weather3pm[[i]]),]
}

#
#southbris9am
#Tissa
unpredictableairqualities<-c("Wind.Direction..degTN.","Wind.Speed..m.s.","Wind.Sigma.Theta..deg.","Wind.Speed.Std.Dev..m.s.","Relative.Humidity....")
for (i in (unpredictableairqualities))
{
  
  print(complete.cases(southBris9am[[i]]))
  southBris9am <- southBris9am[complete.cases(southBris9am[[i]]),]
}
#
#southbris3pm
#Tissa
unpredictableairqualities<-c("Wind.Direction..degTN.","Wind.Speed..m.s.","Wind.Sigma.Theta..deg.","Wind.Speed.Std.Dev..m.s.","Relative.Humidity....")
for (i in (unpredictableairqualities))
{
  
  print(complete.cases(southbris3pm[[i]]))
  southbris3pm <- southbris3pm[complete.cases(southbris3pm[[i]]),]
}
#

#Soft Reject_Predicatble_Column_Values
#Nikhil
#Changing_outlier_values_from_Predicatble_Columns
#to replace NA values in weather 3pm by average of n-1,n-2,n+1,n+2 values
#southbris9am
predictableairqualities<-c( "Nitrogen.Oxide..ppm.", "Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.", "Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
for (i in (predictableairqualities))
{
  outlier_value<-boxplot.stats(southBris9am[[i]])$out
  for(j in 3:(nrow(southBris9am)-3))
  {
    if(southBris9am[[i]][j] %in% outlier_value)
    {
      k<-0
      temp<-vector()
      ani<-j-2
      while(k!=4)
      {
        if(!(southBris9am[[i]][ani] %in% outlier_value))
        {
          #move the value to temp
          temp<-c(temp,southBris9am[[i]][ani])
          #increment k
          k<-k+1
        }
        #increment ani
        ani<-ani+1
      }
      southBris9am[[i]][j]<-mean(temp)
      print(southBris9am[[i]][j])
    }
  }
}
#
#southbris3pm
#Nikhil
#to replace NA values in weather 3pm by average of n-1,n-2,n+1,n+2 values
predictableairqualities<-c( "Nitrogen.Oxide..ppm.", "Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.", "Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
for (i in (predictableairqualities))
{
  outlier_value<-boxplot.stats(southBris3pm[[i]])$out
  for(j in 3:(nrow(southBris3pm)-3))
  {
    if(southBris3pm[[i]][j] %in% outlier_value)
    {
      k<-0
      temp<-vector()
      ani<-j-2
      while(k!=4)
      {
        if(!(southBris3pm[[i]][ani] %in% outlier_value))
        {
          #move the value to temp
          temp<-c(temp,southBris3pm[[i]][ani])
          #increment k
          k<-k+1
        }
        #increment ani
        ani<-ani+1
      }
      southBris3pm[[i]][j]<-mean(temp)
      print(southBris3pm[[i]][j])
    }
  }
}
#
#Soft_Rejecting_NA_values_with_mean

#SouthBris3pm
#Nikhil
#to replace NA values in weather 3pm by average of n-1,n-2,n+1,n+2 values
predictableairqualities<-c("Air.Temperature..degC.", "Nitrogen.Oxide..ppm.", "Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.", "Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
for (a in (predictableairqualities))
{
  i<-a
  for(j in 3:(nrow(southBris3pm)-3))
  {
    if(is.na(southBris3pm[[i]][j]))
    {
      k<-0
      temp<-vector()
      ani<-j-2
      while(k!=4)
      {
        if(!is.na(southBris3pm[[i]][ani]))
        {
          #move the value to temp
          temp<-c(temp,southBris3pm[[i]][ani])
          #increment ani
          ani<-ani+1
        }
        #increment k
        k<-k+1
      }
      southBris3pm[[i]][j]<-mean(temp)
      print(southBris3pm[[i]][j])
    }
  }
}
#
#SouthBris9pm
#Nikhil
#to replace NA values in weather 3pm by average of n-1,n-2,n+1,n+2 values
predictableairqualities<-c("Air.Temperature..degC.", "Nitrogen.Oxide..ppm.", "Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.", "Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
for (a in (predictableairqualities))
{
  i<-a
  for(j in 3:(nrow(southBris9am)-3))
  {
    if(is.na(southBris9am[[i]][j]))
    {
      k<-0
      temp<-vector()
      ani<-j-2
      while(k!=4)
      {
        if(!is.na(southBris9am[[i]][ani]))
        {
          #move the value to temp
          temp<-c(temp,southBris9am[[i]][ani])
          #increment ani
          ani<-ani+1
        }
        #increment k
        k<-k+1
      }
      southBris9am[[i]][j]<-mean(temp)
      print(southBris9am[[i]][j])
    }
  }
}
summary(southBris9am)
#
#weather9am
#Tissa
#to replace NA values in weather 3pm by average of n-1,n-2,n+1,n+2 values
predictableweatherqualities<-c("weatherTotal.MinTemp", "weatherTotal.MaxTemp", "weatherTotal.Temp9am"  )

for (a in (predictableweatherqualities))
{
  i<-a
  for(j in 3:(nrow(weather9am)-3))
  {
    if(is.na(weather9am[[i]][j]))
    {
      k<-0
      temp<-vector()
      ani<-j-2
      while(k!=4)
      {
        if(!is.na(weather9am[[i]][ani]))
        {
          #move the value to temp
          temp<-c(temp,weather9am[[i]][ani])
          #increment ani
          ani<-ani+1
        }
        #increment k
        k<-k+1
      }
      weather9am[[i]][j]<-mean(temp)
      print(weather9am[[i]][j])
    }
  }
}

#
#weather3pm
#Nikhil
#to replace NA values in weather 3pm by average of n-1,n-2,n+1,n+2 values
predictableweatherqualities<-c("weatherTotal.MinTemp", "weatherTotal.MaxTemp", "weatherTotal.Temp3pm")

for (a in (predictableweatherqualities))
{
  i<-a
  for(j in 3:(nrow(weather3pm)-3))
  {
    if(is.na(weather3pm[[i]][j]))
    {
      k<-0
      temp<-vector()
      ani<-j-2
      while(k!=4)
      {
        if(!is.na(weather3pm[[i]][ani]))
        {
          #move the value to temp
          temp<-c(temp,weather3pm[[i]][ani])
          #increment ani
          ani<-ani+1
        }
        #increment k
        k<-k+1
      }
      weather3pm[[i]][j]<-mean(temp)
      print(weather3pm[[i]][j])
    }
  }
}
#

# End_Data_Cleaning_Nikhil_Tissa

#
#
#start_Integrate_Jerin
total9am<- merge(weather9am,southBris9am,by="Date")
total3pm<- merge(weather3pm,southBris3pm,by="Date")
names(total3pm)<-c("Date","MinTemp","MaxTemp","Rainfall","Evaporation","Sunshine","WindGustDir","WindGustSpeed","RainToday","RISK_MM","RainTomorrow","WindDir","Humidity","Pressure","Cloud","Temp","Time","Wind.Direction..degTN.","Wind.Speed..m.s.","Wind.Sigma.Theta..deg.","Wind.Speed.Std.Dev..m.s.","Air.Temperature..degC.","Relative.Humidity....","Nitrogen.Oxide..ppm.","Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.","Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
names(total9am)<-c("Date","MinTemp","MaxTemp","Rainfall","Evaporation","Sunshine","WindGustDir","WindGustSpeed","RainToday","RISK_MM","RainTomorrow","WindDir","Humidity","Pressure","Cloud","Temp","Time","Wind.Direction..degTN.","Wind.Speed..m.s.","Wind.Sigma.Theta..deg.","Wind.Speed.Std.Dev..m.s.","Air.Temperature..degC.","Relative.Humidity....","Nitrogen.Oxide..ppm.","Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.","Carbon.Monoxide..ppm.","PM10..ug.m.3.","PM2.5..ug.m.3.")
total9am$Time<-"9am"
total3pm$Time<-"3pm"
totalData<-rbind(total9am,total3pm)

#end_Integrate_Jerin
#
#
class(totalData)

#Start_Correlation_Nikhil_Tissa

cor_data<-totalData[, !(names(totalData) %in% c("Date" ,"WindGustDir","RainToday","RainTomorrow","WindDir","Time"))]
corrplot(cor(as.matrix(cor_data)),method = "circle")

#End_Correlation_Tissa_Nikhil
#
#
#start_DTree_Jerin

#end_DTree_Jerin
#
#



#
#
#start_Clustering_Jerin
#row.names(mydata)<-paste(mydata$Date,mydata$Time)
total9am<-total9am[, !(names(total9am) %in% c("Time","WindGustDir"))]
total3pm<-total3pm[, !(names(total3pm) %in% c("Time","WindGustDir"))]
#row.names(total9am)<-total9am$Date
#row.names(total3pm)<-total3pm$Date
mydata<-total9am[total9am$Date %in% total3pm$Date,]
for(i in 1:nrow(mydata))#take avarage of 9am and 3pm data for each date
{
  for (j in 1:nrows(total3pm)) {
    if(total3pm$Date[j]==mydata$Date[i]){
      for(k in names(mydata)){
        mydata[[k]][i]<-(mydata[[k]][i]+total3pm[[k]][j])
      }
    }
  }
}
row.names(mydata)<-mydata$Date#naming rows with dates
mydata<-mydata[, !(names(mydata) %in% c("Date"))]#removing dates from attrbute list


for( i in names(mydata)){
  mydata[[i]]<-as.numeric(mydata[[i]])
}

mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
#fviz_nbclust(mydata, kmeans, method = "wss") +
#  geom_vline(xintercept = 3, linetype = 2)+
#  labs(subtitle = "Elbow method")
set.seed(123)
#fviz_nbclust(mydata, kmeans, nstart = 25,  method = "gap_stat", nboot = 100)+
# labs(subtitle = "Gap statistic method")
cluster<-kmeans(mydata, centers = 9, nstart = 25)
fviz_cluster(cluster, data = mydata)
#end_Clustering_Jerin
#
#

