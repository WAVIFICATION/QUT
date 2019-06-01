library(tidyverse)
library(party)
southb <- read.csv("southbrisbane-aq-2018.csv")
str(southb)

summary(southb)


a <- filter(southb, Nitrogen.Oxides..ppm.>= 0.0000 & Nitrogen.Oxides..ppm.<= 0.0130) %>%
  mutate(Oxide_Range= "verylow")
   a
   
   b <- filter(southb, Nitrogen.Oxides..ppm.> 0.0130 & Nitrogen.Oxides..ppm.<= 0.0230) %>%
     mutate(Oxide_Range= "low")
   b
   
   
   c <- filter(southb, Nitrogen.Oxides..ppm.> 0.0230 & Nitrogen.Oxides..ppm.<= 0.0302) %>%
     mutate(Oxide_Range= "medium")
   c
   
   d <- filter(southb, Nitrogen.Oxides..ppm.> 0.0302 & Nitrogen.Oxides..ppm.<= 0.0390) %>%
     mutate(Oxide_Range= "high")
   d
   
   e <- filter(southb, Nitrogen.Oxides..ppm.> 0.0390 & Nitrogen.Oxides..ppm.<= 0.2430) %>%
     mutate(Oxide_Range= "veryhigh")
   e
   
   
   z <- filter(southb, is.na(Nitrogen.Oxides..ppm.)) %>%
     mutate(Oxide_Range = NA)
   
  f <- rbind(a,b)
g<- rbind(c,d)     
h <- rbind(f,g) 
i <- rbind(h,e)
j <- rbind(i,z)

southb_plot1 <- ggplot(i, aes(fill = Oxide_Range, color = Oxide_Range))
#southb_plot

i$Oxide_Range <- as.factor(i$Oxide_Range)
str(i)

set.seed(1234)
ind <- sample(2, nrow(i), replace=TRUE, prob=c(0.7, 0.3))
ind
train_data1 <- i[ind==1,]
#train_data
test_data1 <- i[ind==2,]
#test_data


formula1 <- Oxide_Range ~ Air.Temperature..degC. 

southb_ctree1 <- ctree(formula1, data = train_data1)
#southb_ctree1

plot(southb_ctree1)

predictions <- predict(southb_ctree1, newdata = test_data1)


table(predictions, test_data1$Oxide_Range)


library(caret)
library(e1071)
confusionMatrix(predict(southb_ctree1, newdata = test_data1), test_data1$Oxide_Range)
