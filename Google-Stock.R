# Google stocks analysis

rm(list=ls())
data<-read.csv("GOOG.csv",header=TRUE,sep=",")
head(data)


#Formatting issues if any
Date<-formatted_date <- as.Date(data$Date, format = "%d-%m-%Y")

#taking log of closing prices as they tend to be tight skewd
lnclose<-(data$Close)

#plotting log close pricing over time period t
png("Googles_Closing_prices.png")
plot(lnclose ~ Date, main="Plotting of Googles (log)closing prices against time t",type='l',col='maroon')
dev.off()


#creating time t trend variable
nrows<-nrow(data)
Trend<-c(1:nrows)


# we will be creating a y(t)=beta0+beta1*t + beta2*y(t-1)+r

#adding NA for first one and others as lag of 1 to rest of the values
lnclose_lag1<-c(NA,lnclose[-nrows])
plot(lnclose~lnclose_lag1,type='b', main="Scatterplot of close vs lag of 1 of close prcies",col='red')


#build model of t +AR(1)
model<-lm(lnclose~Trend+lnclose_lag1)
model
summar(model)

#this model above also considerd NA value in the beginning we dont want that

#we want to define our regression range as it is not nrows
#we want to pick some for model and rest for validation so we weill
#create a range
lag<-1
range_model <-c(lag+1:nrows)

#now let's create our variables for above range
y<-lnclose[range_model]
t<-Trend[range_model]
y_lag_1<-lnclose_lag1[range_model]

model<-lm(y~t+y_lag_1)
model
summary(model)


#==========================
#Forecasting
#==========================

# In sample

modelfit<-fitted(model) #find predicted values using the model
fitted_all <-c(NA,modelfit)
fitted_all[nrows]


#plot actual vs predicted
plot(lnclose~Date,type="b",col="yellow")
lines(fitted_all~Date,col='red')


#y_hat of last value using 2 methods

fitted_all[nrows]
y_nrow<-predict(model,data.frame(t=nrows,y_lag_1=lnclose_lag1[nrows]))

#error 
error_nrow<-lnclose[nrows]-y_nrow
error_nrow

#predict yhat for nrows+1

y_hat_nrows_1<-predict(model,data.frame(t=nrows+1,y_lag_1=lnclose[nrows]))
y_hat_nrows_1

#predict yhat for nrows+2
y_hat_nrows_2<-predict(model,data.frame(t=nrows+2,y_lag_1=y_hat_nrows_1))
y_hat_nrows_2
