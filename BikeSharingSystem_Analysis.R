#readind data into r
bikerental<-read.csv("D:/Backup_Nikhil/MS_IS/Sem1/Flex2/BANA7038/Final/DataSet/day.csv",header = TRUE)
bikerental

#exploring data
#no of observations:
nrow(bikerental)
#no of variables:
ncol(bikerental)
#checking for null values:
nacheck<-is.na(bikerental)
sum(nacheck) #no null values

#changing normalized values of temp to actual values:
bikerental$actualtemp <- bikerental$temp*41
#changing feeled temp to actual values:
bikerental$feeltemp <- bikerental$atemp*50
bikerental

library(plotly)
packageVersion('plotly')

#plotting scatterplot for dataset
plot(bikerental, col= blues9)

#p <- plot_ly(bikerental, type = "box")
#temp and atemp seems to be correlated

#correlation check:
#1) actual temp and count
cor(bikerental$actualtemp,bikerental$cnt)
cor.test(bikerental$actualtemp,bikerental$cnt)
#2) feeled temp and count
cor(bikerental$feeltemp,bikerental$cnt)
cor.test(bikerental$feeltemp,bikerental$cnt)

#humidity --  can be taken out
bikerental$actualhum <- bikerental$hum*100
cor.test(bikerental$actualhum,bikerental$cnt)
#windspeed --  can be taken out
bikerental$windspeed
bikerental$actualwind <- bikerental$windspeed*67
cor.test(bikerental$actualwind,bikerental$cnt)

# correlation between temp and atemp
cor.test(bikerental$actualtemp,bikerental$feeltemp)
#highly correlated - can result into multicollineairty


#checking for relation between temp and registered and casual users
cor.test(bikerental$actualtemp,bikerental$casual)
cor.test(bikerental$actualtemp,bikerental$registered)
plot(bikerental$actualtemp,bikerental$casual,col="red")
points(bikerental$actualtemp,bikerental$registered)
abline(lm(bikerental$registered~bikerental$actualtemp))
abline(lm(bikerental$casual~bikerental$actualtemp))
#modeling with temp and cnt
model1<-lm(cnt~actualtemp+feeltemp,data = bikerental)
summary(cnt)
model1
summary(model1)
model1i<-lm(cnt~actualtemp+feeltemp+actualtemp:feeltemp)
summary(model1i) #better fit

anova(model1,model1i)

influence.measures(model1)
#no influencial point and no leverage point based on h-hat and df-fits
library(psych)
weatherp<-dummy.code(weathersit)
is.matrix(weatherp)
bikerental1<-cbind(bikerental,weatherp)
names(bikerental1)
names(bikerental1)[20] <- "clear"
names(bikerental1)[21] <- "mist"
names(bikerental1)[22] <- "snow"
attach(bikerental1)
model2<-lm(cnt~actualtemp+feeltemp+clear+mist+snow,data = bikerental1)
summary(model2)
model2i<-lm(cnt~actualtemp+feeltemp+clear+mist+snow+actualtemp:feeltemp,data = bikerental1)
summary(model2i)
model3<-lm(cnt~actualtemp+clear+mist+snow,data = bikerental1)
summary(model3)
bikerental1
plot(bikerental$actualwind,bikerental$cnt)
library(ggplot2)
qplot(bikerental$actualhum,bikerental$cnt, geom = c("point","smooth"))

par(mfrow=c(1,1))
head(bikerental)
#temperature ranges for each season:
springer1 <- subset(bikerental,season == 1)$actualtemp
smean <- mean(springer1)
sstd <- sd(springer1)
smean; sstd
hist(springer1)

summer <- subset(bikerental,season == 2)$actualtemp
smean <- mean(summer)
sstd <- sd(summer)
smean; sstd
hist(summer)

fall <- subset(bikerental,season == 3)$actualtemp
smean <- mean(fall)
sstd <- sd(fall)
smean; sstd
hist(fall)

winter <- subset(bikerental,season == 4)$actualtemp
smean <- mean(winter)
sstd <- sd(winter)
smean; sstd
hist(winter)
attach(bikerental)
#checking if holiday is a good predictor
model4<-lm(cnt~actualtemp+holiday)
summary(model4)
anova(model4)
model4i<-lm(cnt~actualtemp)
summary(model4i)
anova(model4,model4i)
### conclusion holiday is not a good predictor -- can be taken out

###checking for weather -- 
bikerental1<-bikerental
bikerental1$weatherfac<-as.factor(bikerental$weathersit)
as.factor(bikerental$weathersit)
model5<-lm(cnt~actualtemp+weatherfac,data = bikerental1)
summary(model5)
anova(model5)
##its a predictor
###checking for interation
model6<-lm(cnt~actualtemp+weatherfac+actualtemp:weatherfac,data = bikerental1)
summary(model6)
anova(model5,model6)
#### no interaction


###checking for working day
bikerental1$workingfac <-as.factor(bikerental1$workingday)
model7<-lm(cnt~actualtemp+weatherfac+workingday, data = bikerental1)
summary(model7)
anova(model5,model7)
#working day can be taken out

#checking for humidity
model8<-lm(cnt~actualtemp+weatherfac+hum,data = bikerental1)
summary(model8)
anova(model8)
anova(model5,model8)

#humiday can be taken in

#checking for windspeed
model9<-lm(cnt~actualtemp+weatherfac+hum+windspeed, data=bikerental1)
summary(model9)
anova(model8,model9)

#checking for season
bikerental1$seasonfac <- as.factor(season)
model10<-lm(cnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerental1)
summary(model10)
plot(bikerental1$seasonfac,bikerental1$cnt)

###bike count is in order fall,summer,winter,spring
library(MASS)
boxcox(model10)

model10i<-lm(cnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerental1)
summary(model10i)
model10i$fitted.values

bikerental1$transcnt <- bikerental1$cnt^.6
model11<-lm(transcnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerental1)
summary(model11)

plot(model11)


bikerental1$transcnt <- bikerental1$cnt^.5
model11<-lm(transcnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerental1)
summary(model11)

plot(model11)

bikerental1$transcnt <- bikerental1$cnt^.4
model11<-lm(transcnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerental1)
summary(model11)

plot(model11)

bikerental1$transcnt <- bikerental1$cnt^.3 #ideal
model11<-lm(transcnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerental1)
summary(model11)

plot(model11)
####################################################################################

b<-aov(cnt~seasonfac,data = bikerental1)
summary(b)
TukeyHSD(b)

library(MASS)
boxcox(model9)
bikerental1$transcnt <- bikerental1$cnt^.3
model9i<-lm(transcnt~actualtemp+weatherfac+hum+windspeed, data=bikerental1)
summary(model9i)

###try for individual seasons

bikerental1spring <- subset(bikerental1,bikerental1$season == 1)
bikerental1spring
nrow(bikerental1spring)
model9s<-lm(transcnt~actualtemp+weatherfac+hum+windspeed, data=bikerental1spring)
summary(model9s)

bikerental1summer <- subset(bikerental1,bikerental1$season == 2)
bikerental1summer
nrow(bikerental1summer)
model9su<-lm(transcnt~actualtemp+weatherfac+hum+windspeed, data=bikerental1summer)
summary(model9su)




#takind data yearwise
bikerentalyear1 <- subset(bikerental1,bikerental1$yr == 0)
nrow(bikerentalyear1)
bikerentalyear1$transcnt <- bikerentalyear1$cnt^.3 #ideal
model11y1<-lm(transcnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerentalyear1)
summary(model11y1)
model11y1$fitted.values
bikeyear2<-predict(model11y1,interval = "predict")
bikeyear2

plot(bikeyear2[,1],type = "p",col="green")
points(bikeyear2[,2],type = "p",col="red")
points(bikeyear2[,3],type = "p",col="red")
bikerentalyear2 <- subset(bikerental1,bikerental1$yr == 1)
bikerentalyear2$transcnt <- bikerentalyear2$cnt^.3 #ideal
model11y2<-lm(transcnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerentalyear2)
points(bikerentalyear2$cnt, type="p", pch = 20 , col = "blue")
points(bikerentalyear2$transcnt, type="p", pch = 20 , col = "blue")

#predicting as per model10
model10y1<-lm(cnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerentalyear1)
summary(model10y1)

plot(model10y1)
plot(model10y1$fitted.values,model10y1$residuals)
abline(h=0)

#applying transformation
boxcox(model10y1)
#taking labda as .9
bikerentalyear1$transcnt <- bikerentalyear1$cnt*.95
model10y1a<-lm(transcnt~actualtemp+weatherfac+hum+windspeed+seasonfac, data=bikerentalyear1)
summary(model10y1a)

plot(model10y1a)
plot(model10y1a$fitted.values,model10y1a$residuals)
abline(h=0)


bikeyear2<-predict(model10y1a, interval =  "predict")
plot(bikeyear2[,1],type = "p",col="green")
points(bikeyear2[,2],type = "p",col="red")
points(bikeyear2[,3],type = "p",col="red")
bikerentalyear2$transcnt <- bikerentalyear2$cnt^.95
points(bikerentalyear2$transcnt, type="p", pch = 20 , col = "blue")
