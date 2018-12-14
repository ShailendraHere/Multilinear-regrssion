#Analysis and model building for the Auto dataset using multilinear regression


library(caret)
library(ISLR)
library(usdm)

data(Auto)
View(Auto)
dim(Auto)


# the variable name is removed as it is a qualitaive variable
Auto1<-Auto[,-9]
 
#plotting variables against each other we realise there is a strong relationship amongst some variables
plot(Auto1)

#To conform the insight from the plot we create a correlation matrix 
cor(Auto1)
#correlation  matrix tells us that there is a lot of correlation amongst variable
#as we are using this data for prediction and not to understand the indiviual effect of the predictor on response multicolinearity is not much of an issue
#response being the mpg
plot(Auto1$mpg)

vif(Auto1)
#displacement and weight have very high VIF so we wont be using them for prediction


#lets begin with our model we will follow the backward selection to identify the predictors
model<-lm(mpg~.,data = Auto1)
summary(model)
#from summary we can see that displacement weight year and orgin have very low p values and adj-R squared=81.82

model1<-lm(mpg~weight+year+displacement+origin,data = Auto1)
summary(model1)
#there is negligible diffrence between the adj R-squared values of model and model1



#now we start modelling by dividing our dataset into training and testing data into 70-30 respectively

splitData <-createDataPartition(Auto1$mpg, p=0.7,list = F)
train<-Auto1[splitData,]
test<-Auto1[-splitData,]


mode2<-lm(mpg~weight+year+displacement+origin,data = train)
summary(model2)
#model 2 is same as model 1 just the diffrence is the dataset.

par(mfrow=c(2,2))
plot(model2)
# In the first plot we can see the the presence of hetroskedacity(non constant variance)
# also there is a pattern in the first plot indicates there is a issue with the linear assumption


model3<-lm(log(mpg)~log(weight)+year+horsepower*acceleration,data = train)
summary(model3)
#adj R-squared value is significantly increased compared with previous models

par(mfrow=c(2,2))
plot(model3)
#hetroskedacity and non linearity is removed now this model can be used for prediction

predict(model3,test)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(model3$residuals)
#RMSE is quite low for model3 and thus model3 is a good model

par(mfrow=c(2,2))
plot(model3)

train<-train[-c(167,125,387),]# removing the outliers

model4<-lm(log(mpg)~log(weight)+year+weight*displacement+displacement*horsepower,data = train)
summary(model4)
#model 4 performs better than model 3 due to inclusion of interaction terms(2 most colinear pairs from cor matrix)
#model 4 also uses the data for training for which the outliers are removed

RMSE(model4$residuals)
par(mfrow=c(2,2))
plot(model4)

