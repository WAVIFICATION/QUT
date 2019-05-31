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