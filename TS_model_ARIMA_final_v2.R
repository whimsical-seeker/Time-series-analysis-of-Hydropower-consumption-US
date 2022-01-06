####################################################################################################
#Project Title: Forecasting for hydroelectricity power consumption

#Author: Vidya shree Suresh

#Student ID - 20906282

#######################################################################################################


library(tidyverse)
library(tidymodels)
library(data.table)
library(tidyposterior)
library(tsibble)
library(fable) 
library(forecast) 
library(tseries)
library(zoo)
library(lmtest)
library(Hmisc)
library(readxl)
library(TSA)




df_hyd=read_xlsx("raw_data.xlsx")

#Exploratory Data analysis
#-------------------------

summary(df_hyd)

plot(df_hyd$btu,type="l")

class(df_hyd$Mon)

class(df_hyd$Mon)


#Converting to a ts dataset

pf=periodogram(df_hyd$btu)

f1=pf$freq[which.max(pf$spec)]

1/pf$freq[which.max(pf$spec)]

df_ts= ts(df_hyd$btu[1:123],start=c(2011, 5), frequency=12)


par(mfcol=c(1,2))

plot(df_ts)
autoplot(df_ts) +
  ggtitle("Time Series plot of the hydropower consumption in US") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Month-year")+ ylab("Quadrillion btu")

cpt_check=cpt.var(df_ts,method="PELT",class=FALSE)
cpt_check_mean=cpt.mean(df_ts,method="PELT",class=FALSE)


#Distribution

hist(df_ts)
ggseasonplot(df_ts_a)


##Extracting data till 2021 Sep and conducting the study

df_ts_a=df_ts
  
#overlaying the normal distribution

h <- hist(df_ts_a, density = 10,
          col = "blue", xlab = "Quadrillion btu ", main = "Histogram and density plot of the hydropower consumption in US",ylim=c(0,30)) 
xfit <- seq(min(df_ts_a), max(df_ts_a), length = 40) 
yfit <- dnorm(xfit, mean = mean(df_ts_a), sd = sd(df_ts_a)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df_ts_a) 

lines(xfit, yfit, col = "black", lwd = 2)

abline(v = mean(df_ts),                       # Add line for mean
       col = "red",
       lwd = 2,
       )

abline(v = median(df_ts),                       # Add line for mean
       col = "green",
       lwd = 2)

text(1,3, "abline( 1, 2 )", col = 2)

#Time series  characteristics
#-------------------------

plot(decompose(df_ts_a), title= "Decomposition of  the hydropower consumption in US")



#Time series statistical properties
#-------------------------



boxplot(df_ts_a) #There is one outlier 

#The spread of the data looks normal



mean(df_ts_a)

var(df_ts_a)  

cv= sd(df_ts_a)/mean(df_ts_a)  ## low coefficient of variation


# Testing for unit root
adf.test(df_ts_a) #stationary 




#The Shapiro-Wilk Test uses the test statistic as follows:
# 𝐻0: the errors follow a normal distribution.
# 𝐻1: the errors do not follow a normal distribution

print("Normality")
shapiro.test(df_ts_a) ## It i#Reject the null hypothesis not normal

mean(model2$residuals)

plot(model2$residuals)



qqnorm(df_ts_a)
qqline(df_ts_a)

qqnorm(diff(log(df_ts_a)))
qqline(diff(log(df_ts_a)))

par(mfcol=c(1,2))

ggplot(mapping = aes(sample =as.numeric((((df_ts_a)^lambda-1)/lambda)))) +
 stat_qq_point(size = 2,color = "red") +
stat_qq_line(color="green") +
 xlab("x-axis") + ylab("y-axis")

#The box-cox transformed model produces a Q-Q plot with a much straighter line. No data transformation is required
bc <- boxcox(df_ts_a~time(df_ts_a))
(lambda <- bc$x[which.max(bc$y)])

p <- probplot(x, line=FALSE)
lines(p, col="green", lty=2, lwd=2)

lambda=0
hist((((df_ts_a)^lambda-1)/lambda))

qqnorm(df_ts_a[2:118])
qqline(df_ts_a[2:118])


qqnorm(log(df_ts_a[2:118]))
qqline(log(df_ts_a[2:118]))

qqnorm((((df_ts_a)^lambda-1)/lambda))
qqline((((df_ts_a)^lambda-1)/lambda))


new_model <- lm((((df_ts_a)^lambda-1)/lambda) ~ time(df_ts_a))

new_model <- lm(log(df_ts_a) ~ time(df_ts_a))

summary(new_model)



Box.test(df_ts_a, type="Ljung-Box")

# Possible estimates of AR and MA terms
#----------------------------------
###Splitting data into train and test

df_ts_a=ts(df_hyd$btu[1:118],start=c(2011, 5), frequency=12)

df_ts_v=ts(df_hyd$btu[118:length(df_ts)],start=c(2021, 3), frequency=12)

acf(df_ts_a,50, main="ACF plot for ts_hyd ")  # aperiodic oscillation

#Sample ACF displays an attenuating 
#sine wave. Perhaps need AR terms



#Tapering or sinusoidal pattern that converges to 0, possibly alternating negative and positive signs


pacf(df_ts_a,50, main="PACF plot for ts_hyd ")

#Sample PACF displays spike at 1,2,3,10,12. Have to consider a constrained AR model 

#seasonal differencing is required which makes it a MA model and 

#Fitting the first model with the AR constrained to arima(x = df_ts_a, order = c(0,0,1 ), seasonal = list(order = c(0,0,0), period = 12))
model0=arima(df_ts_a, order=c(3,0,0)) #if the lag after two are ignored

summary(model0)

acf(model0$residuals,50)

pacf(model0$residuals,50)




model1=arima(df_ts_a, order=c(3,0,0), seasonal=list(order=c(1,0,0), period=12)) #if the lag after two are ignored 

summary(model1)

sqrt(diag(vcov(model1)))

acf(model1$residuals, main= "ACF - ARIMA model for (3,0,0) (1,0,0) [12]")

pacf(model1$residuals, main= "PACF - ARIMA model for (3,0,0) (1,0,0) [12]")

## There is still dependency spikes are prominent at lag 12 aCF plot add MA term

#The Shapiro-Wilk Test uses the test statistic as follows:
# 𝐻0: the errors follow a normal distribution.
# 𝐻1: the errors do not follow a normal distribution

print("Normality")
shapiro.test(model1$residuals) ## It i#Reject the null hypothesis not normal

qqnorm(model1$residuals)
qqline(model1$residuals)



##Adding the difference term to check if there is a difference



model2=arima(df_ts_a, order=c(3,0,0), seasonal=list(order=c(1,0,1), period=12)) #if the lag after two are ignored 

summary(model2)


acf(model2$residuals,main="ACF - ARIMA model for (3,0,0)(1,0,1)[12]")

pacf(model2$residuals,main="PACF - ARIMA model for (3,0,0)(1,0,1)[12]")

print("Normality")
shapiro.test(model2$residuals) ## Do not reject the null hypothesis not mormal


Box.test(model2$residuals, type="Ljung-Box")  # cannot reject the null hypothesis, it is white since p>0.05



qqnorm(model2$residuals)
qqline(model2$residuals) # It is normally distributed apart from the slight deviation at the tail

model2.stdres = rstandard(model1)


plot(model2.stdres,ylab='Standardized residuals of ts_hyd',type='l')
  abline(h=0)

mean((model2$residuals)^2) #MSE 0.0002497842 #RMSE: 0.01580456

sqrt(sum((model2$residuals)^2)/length(df_ts_a))

model_a=model2

#Even though the normality test says the sample is normally distributed the dispersion is abit higher on the other end of the tail



##Forecasting with the model
#---------------------------

fcast = predict(model2, n.ahead = 5)

fcast$pred

accuracy(fcast$pred,df_ts_v) # RMSE = 0.04291639  , MAPE=16.55429 %


mean(abs(df_ts_v-fcast$pred)/df_ts_v*100)


## Since the Normal distribution is not completely followed we go with testing the space model

model_s=StructTS(df_ts_a)


summary(model_s)



qqnorm(model_s$residuals)
qqline(model_s$residuals)

acf(model_s$residuals)

plot(model_s$residuals,type="p")


fcast_s = predict(model_s, n.ahead = 5)

fcast_s$pred

accuracy(fcast_s$pred,df_ts_v) # RMSE =  0.02813369  , MAPE=9.128351  %

############ HoltWinters model##############################

model_h=hw(df_ts_a)

summary(model_h)

acf(model_h$residuals)

model_h$SSE

accuracy(model_h$fitted,df_ts_a)  #0.0186378

fcast_s = predict(model_h,n.ahead = 5)

fcast_s

accuracy(fcast_s,df_ts_v) # RMSE =  0.02813369  , MAPE=9.128351  %


k <- 60 # minimum data length for fitting a model
n <- length(df_ts_a)

###Rolling 12 point forecast

mae1 <- mae2 <- mae3 <- matrix(NA,n-k,12)
st <- tsp(df_ts_a)[1]+(k-2)/12

hor=12

for(i in 1:(n-k))
{
 print(i)
  xshort <- window(df_ts_a, start=st+(i-k+1)/12, end=st+i/12)
  xnext <- window(df_ts_a, start=st + (i+1)/12, end=st + (i+12)/12)
  fit1 <- StructTS(xshort)
  fcast1 <- predict(fit1, n.ahead =12)

  fit2 <- ets(xshort)
  fcast2 <- forecast(fit2, h=hor)
  fit3 <-hw(xshort,bootstrap=TRUE)
  fcast3 <- forecast(fit3, h=hor)
  
  mae1[i,1:length(xnext)] <-   abs(fcast1$pred-xnext)
  
  mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
  mae3[i,1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
}


plot(1:6, colMeans(mae1[,1:6],na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE", ylim = c( 4.219578e-05,0.11))

lines(1:6, colMeans(mae2[,1:6],na.rm=TRUE), type="l",col=3)
lines(1:6, colMeans(mae3[,1:6],na.rm=TRUE), type="l",col=4)

legend("topleft",legend=c("ARIMA","ets","HW"),col=2:4,lty=1)




plot(1:6, apply(mae1[,1:6],2,function(x) sum(x^2,na.rm = T)/length(x[!is.na(x)])), type="l", col=2, xlab="horizon", ylab="MAE", ylim = c(9.392031e-06,0.01))

lines(1:6, apply(mae2[,1:6],2,function(x) sum(x^2,na.rm = T)/length(x[!is.na(x)])), type="l",col=3)
lines(1:6, apply(mae3[,1:6],2,function(x) sum(x^2,na.rm = T)/length(x[!is.na(x)])), type="l",col=4)

legend("topleft",legend=c("ARIMA","ets","HW"),col=2:4,lty=1)



model_e= ets(df_ts_a[1:118],damped = T)

summary(model_e)

accuracy(model_e)


qqnorm(model_e$residuals)
qqline(model_e$residuals)

acf(model_e$residuals)

model_f.stdres = rstandard(model_e$residuals)


plot(model_e$residuals,ylab='Standardized residuals of Sunspot',type='l')
abline(h=0)

plot(model_s$residuals,type="p")

accuracy(forecast(model_e,h=5),df_ts_v)


ii=predict(model_a,n.ahead =5)

accuracy(ii$pred,df_ts_v)


df=data.frame()



for(i in 1:4)
{
  print(i)
  print(df_ts[1:(117+i)])
  fit1 <- arima(df_ts[1:(117+i)], order=c(3,0,0), seasonal=list(order=c(1,0,1), period=12))
  fit2=ets(df_ts[1:117+i])
  point=i
  ii=predict(fit1,n.ahead =2)
  p1_a= ii$pred[1]
  p2_a=ii$pred[2]
  et=forecast(fit2,h=2)
  p1_e= et[['mean']][1]
  p2_e=et[['mean']][2]
  
  df=rbind(df, c(point,p1_a,p2_a,p1_e,p2_e, df_ts[117+i+1],df_ts[117+i+2]))
  
  
}

colnames(df)=c("point","p1_a","p2_a","p1_e","p2_e","a1","a2")



##############Refinig the ARIMA model

model_f=arima(df_ts_a, order=c(2,0,1), seasonal=list(order=c(1,0,1), period=12)) #if the lag after two are ignored 

summary(model_f)


acf(model_f$residuals)

pacf(model_f$residuals)

print("Normality")
shapiro.test(model_f$residuals) ## Do not reject the null hypothesis not mormal


Box.test(model_f$residuals, type="Ljung-Box")  # cannot reject the null hypothesis, it is white since p>0.05



   # It is normally distributed apart from the slight deviation at the tail

model_f.stdres = rstandard(model1)


plot(model_f.stdres,ylab='Standardized residuals of Sunspot',type='l')
abline(h=0)

mean((model_f$residuals)) #MSE 0.0002497842 #RMSE: 0.01580456

sqrt(sum((model_f$residuals)^2)/length(df_ts_a))

par(mfcol=c(1,1))

plot.ts(df_ts_a, main="hydro data")  


final_pred= predict(model_f,n.ahead = 6)

accuracy(final_pred$pred,df_ts_v)

##############LSTM ##################

prediction=6

lag=prediction

df_ts_a1= as.data.frame(df_ts_a)


library(keras)
library(tensorflow)

lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}

lagged_series = lag_transform(df_ts, 2)

head(lagged_series)

N=nrow(lagged_series)

## 70% and  30% split

n=round(N*0.7,digits = 0)



train=lagged_series[1:n,]

test= lagged_series[(n+1):N,]


x_train=train$x

y_train=train$`x-2`

x_test=test$x

y_test=test$`x-2`



dim(x_train) <- c(length(x_train), 1, 1)

dim(x_test)<- c(length(x_test), 1, 1)

X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                # must be a common factor of both the train and test samples

units = 1                     # can adjust this, in model tuninig phase


model <- keras_model_sequential() 

model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)



model %>% compile(
  loss = 'mean_squared_error',
  optimizer = "adagrad" 
  #metrics = c('accuracy')
)


early_stopping <- callback_early_stopping(patience = 100)

model %>% fit(
  x = x_train, 
  y = y_train, 
  epochs = 500, 
  batch_size = batch_size,
  #shuffle ='FALSE', 
  validation_data = list(x_test, y_test)
  ##  callbacks = list(checkpoint, early_stopping)
)

loss <- evaluate(model, x = x_train, y = y_train)
loss <- evaluate(model, x = x_test , y = y_test)

Epochs = 50   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, validation_data = list(x_test, y_test), verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

L = length(x_test)
#scaler = Scaled$scaler
predictions = model %>% predict(x_test, batch_size=batch_size)

predictions= ts(predictions,start=c(2018,9),frequency = 12)

accuracy(predictions[30:35],df_ts_v)



