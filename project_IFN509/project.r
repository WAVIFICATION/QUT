library(ggplot2)
library(corrplot)

southBrisTotal<- read.csv("southbrisbane-aq-2018.csv")
weatherTotal<- read.csv("weatherAUS.csv")
#class(southBrisTotal$Time)
southBris<-subset(southBrisTotal,!is.na(southBrisTotal$Date))
southBris$Date<-as.Date(southBris$Date, format="%d/%m/%Y")
weatherTotal$Date<-as.Date(weatherTotal$Date, format="%Y-%m-%d")

southBris9am<-subset(southBris,southBris$Time=="09:00")
#summary(southBris9am)
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
  
  #print(complete.cases(southBris9am[[i]]))
  southBris9am <- southBris9am[complete.cases(southBris9am[[i]]),]
}
#
#southbris3pm
#Tissa
unpredictableairqualities<-c("Wind.Direction..degTN.","Wind.Speed..m.s.","Wind.Sigma.Theta..deg.","Wind.Speed.Std.Dev..m.s.","Relative.Humidity....")
for (i in (unpredictableairqualities))
{
  
  #print(complete.cases(southbris3pm[[i]]))
  southBris3pm <- southBris3pm[complete.cases(southBris3pm[[i]]),]
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
      #print(southBris9am[[i]][j])
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
      #print(southBris3pm[[i]][j])
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
      #print(southBris3pm[[i]][j])
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
      #print(southBris9am[[i]][j])
    }
  }
}
#summary(southBris9am)
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
      #print(weather9am[[i]][j])
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
      #print(weather3pm[[i]][j])
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
totalData<-rbind(total9am,total3pm)#totaldata named as 9am and 3pm time

#row.names(mydata)<-paste(mydata$Date,mydata$Time)
t1<-total9am[, !(names(total9am) %in% c("Time","WindGustDir"))]
t2<-total3pm[, !(names(total3pm) %in% c("Time","WindGustDir"))]

for(xc in c("RainTomorrow","RainToday","WindDir")){#changing character value into enumerated ones
  levels(t1[[xc]])<- 1:nlevels(t1[[xc]])
  levels(t2[[xc]])<- 1:nlevels(t2[[xc]])
  # print(xc)
  # print(nlevels(t2[[xc]]))
}
for(xc in names(t1)){
  t1[[xc]]<-as.numeric(t1[[xc]])
  t2[[xc]]<-as.numeric(t2[[xc]])
}

# str(t1)
AverageTimeTotalData<-t1[t1$Date %in% t2$Date,]
for(i in 1:nrow(AverageTimeTotalData))#take avarage of 9am and 3pm data for each date: AverageTimeTotalData
{
  for (j in 1:nrow(t2)) {
    if(t2$Date[j]==AverageTimeTotalData$Date[i]){
      for(k in names(AverageTimeTotalData)){
        AverageTimeTotalData[[k]][i]<-((AverageTimeTotalData[[k]][i]+t2[[k]][j])/2)
      }
      break()
    }
  }
}

#end_Integrate_Jerin
#
#

#Start_Correlation_Nikhil_Tissa

cor_data<-totalData[, !(names(totalData) %in% c("Date" ,"WindGustDir","RainToday","RainTomorrow","WindDir","Time"))]
corrplot(cor(as.matrix(cor_data)),method = "circle")

#End_Correlation_Tissa_Nikhil


#
#
#
#
#start_DTree_Vineela


treedata <- filter(AverageTimeTotalData, Nitrogen.Oxides..ppm.>= 0.0000 & Nitrogen.Oxides..ppm.<= 0.0130) %>%
  mutate(Oxide_Range= "verylow")

treedata <- rbind(treedata, filter(AverageTimeTotalData, Nitrogen.Oxides..ppm.> 0.0130 & Nitrogen.Oxides..ppm.<= 0.0230) %>%
  mutate(Oxide_Range= "low"))

treedata <- rbind(treedata, filter(AverageTimeTotalData, Nitrogen.Oxides..ppm.> 0.0230 & Nitrogen.Oxides..ppm.<= 0.0302) %>%
  mutate(Oxide_Range= "medium"))

treedata <- rbind(treedata, filter(AverageTimeTotalData, Nitrogen.Oxides..ppm.> 0.0302 & Nitrogen.Oxides..ppm.<= 0.0390) %>%
  mutate(Oxide_Range= "high"))

treedata <- rbind(treedata, filter(AverageTimeTotalData, Nitrogen.Oxides..ppm.> 0.0390 & Nitrogen.Oxides..ppm.<= 0.2430) %>%
  mutate(Oxide_Range= "veryhigh"))



fit <- rpart(Oxide_Range ~ Air.Temperature..degC. + Wind.Speed..m.s. +
               Wind.Direction..degTN. + Relative.Humidity....,
             method="class", data=treedata)

# printcp(fit) # display the results
# plotcp(fit) # visualize cross-validation results
# summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Air Quality")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postscript plot of tree


# prune the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Air Quality")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#test and training data
# set.seed(1234)
# ind <- sample(2, nrow(i), replace=TRUE, prob=c(0.7, 0.3))
# ind
# train_data1 <- i[ind==1,]
# train_data1
# test_data1 <- i[ind==2,]
# test_data1

#ctree

# train_data1$Wind.Direction..degTN.<-as.numeric(train_data1$Wind.Direction..degTN.)
# i$Oxide_Range<-as.factor(i$Oxide_Range)
# formula1 <-Oxide_Range ~ Air.Temperature..degC. + Wind.Speed..m.s. +
#   Wind.Direction..degTN. + Relative.Humidity.... 
# ctree1 <- ctree(formula1, data = train_data1)
# ctree1
# 
# plot(ctree1)
# predictions <- predict(ctree1, newdata = test_data1)
# 
# table(predictions, test_data1$Oxide_Range)
# 
# 
# library(caret)
# library(e1071)
# confusionMatrix(predict(ctree1, newdata = test_data1), test_data1$Oxide_Range)

#end_DTree_Vineela
#
#



#
#
#start_Clustering_Jerin
mydata<-AverageTimeTotalData
mydata$Date<-as.Date(mydata$Date)
row.names(mydata)<-mydata$Date#naming rows with dates
mydata<-mydata[, !(names(mydata) %in% c("Date","Temp","WindDir","Wind.Sigma.Theta..deg.",
                                        "Wind.Speed.Std.Dev..m.s.","Nitrogen.Oxide..ppm.",
                                        "Nitrogen.Dioxide..ppm.","Cloud","RainTomorrow",
                                        "WindGustSpeed"))]#removing dates from attrbute list




 mydata <- scale(mydata) # standardize variables
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
set.seed(123)
fviz_nbclust(mydata, kmeans, nstart = 25,  method = "gap_stat", nboot = 100)+
 labs(subtitle = "Gap statistic method")#Using gap method to identify best number of clusters


cluster_i<-kmeans(mydata, centers = 3, nstart = 25)
fviz_cluster(cluster_i, data = mydata)+#clustering
  labs(subtitle = "Clustering of total relevent data")
#end_Clustering_Jerin
#
#

#
#
#start_cluster_analysis_Jerin
# library(cluster) 
# clusplot(mydata, cluster_i$cluster, color=TRUE, shade=TRUE, 
#          labels=2, lines=0)
# library(fpc)
# plotcluster(mydata, cluster_i$cluster)
# 
# d <- dist(mydata, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward") 
# plot(fit) # display dendogram
# groups <- cutree(fit, k=3) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit, k=3, border="red")


# library(pvclust)
# fit <- pvclust(mydata, method.hclust="ward",
#                method.dist="euclidean")
# plot(fit) # dendogram with p values
# # add rectangles around groups highly supported by the data
# pvrect(fit, alpha=.95)




#end_cluster_analysis_Jerin
#
#