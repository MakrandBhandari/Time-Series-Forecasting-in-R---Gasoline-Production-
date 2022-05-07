############# Importing required libraries
library(forecast)
library(lubridate)
library(tseries)
library(datasets)
library(ggplot2)

############# Importing in-built dataset of monthly gasoline production 
gas_prod <- forecast::gas
df_gas <- as.data.frame(gas_prod)

############ Examine the time series 
plot(df_gas, xlab = 'Time (Years)', ylab = 'Gasoline production (millions of barrels)')
# we can observe that there isn't much growth in gasoline production till 1970. 
# From 1970 onwards, there is growing trend in gasoline production

### We will consider data from 1970 onwards in order to avoid old irrelevant data and get good forecasting model
class(df_gas)
class(gas_prod)
gas_aft1970 <- window(gas_prod, start = c(1970,1), frequency = 12)
df_gas_aft1970 <- as.data.frame(gas_aft1970)


### Descriptive Statistics of df_gas_aft1970 dataset
summary(df_gas_aft1970)

########################### Plots and graphs ###########################################################
####  Time series plot
plot(df_gas_aft1970, xlab = 'Time (Years)', ylab = 'Gasoline production (millions of barrels)', main = 'Australian Gas Production')
# from the above ts plot we can observe increasing trend, seasonality (period = 12) 
#and multiplicative ts model as the periodical fluctuations are growing with time

#### Histogram
df_gas__aft1970$x <- as.numeric(df_gas_aft1970$x)
hist(df_gas_aft1970$x, col = 'Green')
# from the histogram plot we can see that the distribution is not normal and is a bi modal distribution

#### Monthplot
monthplot(df_gas_aft1970$x)
# from the monthplot, we can observe that there is increasing production in gas month after month till July.
# after July it declines. However, there is an overall increase in production of gas year after year
# The inrease and then decrease in production within each year also indicates presence of seasonality 

#### Seasonplot
seasonplot(df_gas_aft1970$x, year.labels = T, col = 1:40)
# season plot also clearly shows increase in production till July and its declination from August onwards
# This pattern is repeated every year and shows seasonality


#### Polar Plot
ggseasonplot(df_gas_aft1970$x , polar = T)
# Polar plot indicates highest growth in gas prodn. in the month of July

#### Lag Plot : "gglagplot" will plot time series against lagged versions of themselves. 
#Helps visualising 'auto-dependence' even when auto-correlations vanish.
gglagplot(gas_aft1970)
# each color in the lag plot indicates month of the variable in the y-axis.Lines are connected in chronological order.
# We can observe strong positive relationship in lag 12 indicating strong seasonality in the data


##################### DECOMPOSITION OF TIME SERIES DATA INTO ITS COMPONENTS ############################
# From the time series PLOT, we can observe the time series is an additive model i.e, y = T + S + I
# decomposing the time series data into their respective components

gas.ts = ts(gas_aft1970,frequency = 4)
decompose.ts = decompose(gas.ts, type = 'additive')
plot(decompose.ts)

# From the decomposition plot, we can observe :
# 1. There is a positive trend over the years
# 2. There is a strong seasonality with constant variance over the years
# 3. There is constant variance in the random component till mid 1970s'



############################### Detecting outliers in time series data and replacing them with appropriate numeric values #########################
# tsoutliers() function uses Multiple Seasonal - Trend decomposition using Loess method (mstl) to identify outliers
# MSTL method estimates seasonal and trend components on high frequency data (like hourly, minutely, monthly data)
# other than annual data, and removes it from original data, On the remainder component it detects outliers using 
# IQR method and replaces those.
# for estimating trend component it uses Friedmen's super smoother method (supsmu)

# y(t) = T + S + R

# S_estimate => using MSTL method

# strength of seasonality :
# F(s) = 1 - Var(yt - T_est - S_est)/Var(yt - T_est)
# If F(s) > 0.6, yt* = yt - S_est
# If F(s) <= 0.6 , yt* = yt

# T_est  => using SUPSMU method

# R_est = yt* - T_est => remainder values in ts data

# Q1 => 25th percentile of remainder values ; Q3 => 75th percentile of remainder values
# IQR = Q3 - Q1
# If R_est < (Q1 - 3*IQR) or R_est > (Q3 +3*IQR), then that value is considered as outlier

# The outlier value identified is replaced using linear interpolation method, which uses neigbouring observations

# tsclean() is another function in R forecast package which uses outlier detection and replacement
# in similar manner but along with that it also replaces missing values using linear interpolation method

# first we will identify if there are any missing values and use the function accordingly

sum(is.na(df_gas_aft1970$x))
# there are no missing values, hence we will use tsoutliers() function

tsoutliers(gas_aft1970)


########### Periodicity
frequency(gas_aft1970)
# since, result is 12, it is a monthly time series dataset



############### Stationary #####################
# We want our time series to be stationary (i.e, const. mean, const. var.,and const. co-var over time)
# We will use Augmented Dickey - Fuller test in order to check stationarity.
# If the data is not stationary, we will use differencing method in order to do so


adf.test(gas_aft1970)
# ts data is stationary (p-value < significance value = 0.05, hence , reject null hyp. so, time series 
# is stationary)


################# Modelling - ARIMA #############################

##### 1. Examining the acf & pacf plot
acf(gas_aft1970, main = 'Autocorrelation plot - Gas Production')
pacf(gas_aft1970, main = 'Partial - Autocorrelation plot - Gas Production')
# By looking at ACF plot, due to its decay pattern in a sinusoidal way, it indicates presence of strong seasonality,
# we will use seasonal differencing in order to do so

##### Seasonal Differencing
diff1 <- diff(gas_aft1970,differences = 12)
acf(diff1)
pacf(diff1)

# Looking at PACF & ACF plots of diff1, it indicates p = 3 & q = 3 (max lag beyond which it is insignificant )
# So, we will try ARIMA(3,0,3)(0,1,0)[12] model

######### ARIMA(3,0,3)(0,1,0) Model
arima1 <- arima(x = gas_aft1970, order = c(3,0,3), seasonal = list(order = c(0,1,0),period = 12))
summary(arima1)


## In order to assess the current model, we will use train & validation data from given dataset
train.ts = window(gas_aft1970, start = 1970, end = c(1993,12))
validate.ts = window(gas_aft1970, start = 1994)

autoplot(train.ts , series = 'Train') +
  autolayer(validate.ts , series = 'Validation') +
  ggtitle("Australia gas training and validation data") +
  xlab("Year") + ylab("Production") +
  guides(colour = guide_legend(title = 'Forecast'))


#####  Model 1 : Training ARIMA(3,0,3)(0,1,0)[12] on training data set and validating it on validation dataset
arima2 <- arima(x = train.ts, order = c(3,0,3), seasonal = list(order = c(0,1,0), period = 12))
summary(arima2) # AIC = 4984.97

####  Actual vs Forecast on Validation dataset
forecast2 <- as.data.frame(forecast(arima2, h = 20))
VectorErr <- cbind(validate.ts , forecast2[,1])
ts.plot(VectorErr, col = c("blue", "red"), main = "Gas Production : Actual vs Forecast - ARIMA(3,0,3)(0,1,0)[12] model")
legend("bottomright", legend = c('Actual', 'Forecast'), col = c('blue', 'red'), cex = 0.5, lty = 1:1)
# By comparing plot, the results of forecast looks fine with less error

#### Accuracy of ARIMA(3,0,3)(0,1,0)[12] model
accuracy(forecast(arima2, h = 20), validate.ts)

# Forecast plot of next 12 months
plot(forecast(arima2, h = 12), ylab = "Gas production", xlab = "Year")



##### Validating the ARIMA model
# In this step, we will check whether the residuals :
# 1. are white noise or not. In other words, we will check whether they are having constant mean & variance over time using ACF & PACF plots
# 2. are normally distributed or not using histogram & q-q plot
# 3. will use Box-Ljung test to validate model
acf(arima2$residuals, main = 'ACF plot of residuals of arima2 model')
pacf(arima2$residuals, main = 'PACF plot of residuals of arima2 model')

plot(density(arima2$residuals))
qqnorm(arima2$residuals)
qqline(arima2$residuals)

Box.test(arima2$residuals, type = 'Ljung-Box')


# 1. By looking at ACF & PACF plot of residuals we will need to add seasonal components for q & p
# 2. By looking at q-q plot, we may need to do transformation on time series data . Commonly used transformation
# is log transformation
# 3. In Ljung-Box test, since p-value is > significance level, hence we will accept Null Hypothesis
# i.e Model is a good fit

# we will build two models 1st we will add seasonal p & q para. in arima2 model
# second model we will used log transformed data to build another arima model

####################### Validating second model ################

# Model - 2 : ARIMA(3,0,3)(1,1,1)[12]
arima3 <- arima(x = train.ts, order = c(3,0,3), seasonal = list(order = c(1,1,1), period = 12))
summary(arima3) # AIC = 4962.7



####  Actual vs Forecast on Validation dataset
forecast3 <- as.data.frame(forecast(arima3, h = 20))
VectorErr_1 <- cbind(validate.ts , forecast2[,1])
ts.plot(VectorErr_1, col = c("blue", "red"), main = "Gas Production : Actual vs Forecast - ARIMA(3,0,3)(1,1,1)[12] model")
legend("bottomright", legend = c('Actual', 'Forecast'), col = c('blue', 'red'), cex = 0.5, lty = 1:1)
# By comparing plot, the results of forecast looks fine with lesser error than arima2 model

#### Accuracy of ARIMA(3,0,3)(1,1,1)[12] model
accuracy(forecast(arima3, h = 20), validate.ts)

# Forecast plot of next 12 months
plot(forecast(arima3, h = 12), ylab = "Gas production", xlab = "Year")


#### Residuals tests
acf(arima3$residuals, main = 'ACF plot of residuals of arima3 model')
pacf(arima3$residuals, main = 'PACF plot of residuals of arima3 model')

plot(density(arima3$residuals))
qqnorm(arima3$residuals)
qqline(arima3$residuals)

Box.test(arima3$residuals, type = 'Ljung-Box') # p-value > significance level : Accept H0 => Model is good fit

# This model seems to perform better than previous arima2 model

####################### Log transformation
log_gas_aft1970 <- log(gas_aft1970)
acf(log_gas_aft1970, main = 'ACF plot of log transformed dataset')
pacf(log_gas_aft1970, main = 'PACF plot of log transformed dataset')
# By looking at PACF & ACF plots we need to do seasonal differencing , period = 12

diff2 <- diff(log_gas_aft1970, differences = 12)
acf(diff2, main = 'ACF plot of log transformed and seasonally differenced data')
pacf(diff2, main = 'PACF plot of log transformed and seasonally differenced data')
# By looking at acf & pacf plots , p = 3, q = 4

# Model 3 : ARIMA(3,0,4)(0,1,0)[12]
log_train <- window(log_gas_aft1970, start = 1970, end = c(1993,12))
log_validate <- window(log_gas_aft1970, start = 1994)

arima4 <- arima(log_train, order = c(3,0,4), seasonal = list(order = c(0,1,0), period = 12))
summary(arima4) # AIC = -753.64

####  Actual vs Forecast on Validation dataset
forecast4 <- as.data.frame(forecast(arima4, h = 20))
VectorErr_2 <- cbind(log_validate , forecast4[,1])
ts.plot(VectorErr_2, col = c("blue", "red"), main = "Gas Production : Actual vs Forecast - ARIMA(3,0,4)(0,1,0)[12] model")
legend("bottomright", legend = c('Actual', 'Forecast'), col = c('blue', 'red'),cex = 0.5, lty = 1:1)
# By comparing plot, the results of forecast looks fine with very very less error

#### Accuracy of ARIMA(3,0,4)(0,1,0)[12] model
accuracy(forecast(arima4, h = 20), log_validate)


# Forecast plot of next 12 months
plot(forecast(arima4, h = 12), ylab = "Gas production", xlab = "Year")

# Note : In model 3 : it is trained on log transformed data and forecasts log transformed data , hence we need exponential transformation to get 
# original forecasted values

forecast_12 <- as.data.frame(forecast(arima4, h = 12))
actual_forecast_12 <- exp(x = forecast_12[,1])

#Residuals tests
acf(arima4$residuals, main = 'ACF plot of residuals of arima4 model')
pacf(arima4$residuals, main = 'PACF plot of residuals of arima4 model')

plot(density(arima4$residuals))
qqnorm(arima4$residuals)
qqline(arima4$residuals) # Residuals follow normal distribution

Box.test(arima4$residuals, type = 'Ljung-Box') # p-value = 0.59101 > significance level 0.05 => Accept H0 => Model is a good fit


########## Model 4 : Auto ARIMA model
arima5 <- auto.arima(train.ts, seasonal = T, trace = F)
summary(arima5) # AIC = 4939.33

####  Actual vs Forecast on Validation dataset
forecast5 <- as.data.frame(forecast(arima5, h = 20))
VectorErr_3 <- cbind(validate.ts , forecast5[,1])
ts.plot(VectorErr_3, col = c("blue", "red"), main = "Gas Production : Actual vs Forecast - Auto Arima : ARIMA(2,1,1)(0,1,2)[12] model")
legend("bottomright", legend = c('Actual', 'Forecast'), col = c('blue', 'red'),cex = 0.5, lty = 1:1)
# By comparing plot, the results of forecast looks fine with very very less error

#### Accuracy of Auto ARIMA model
accuracy(forecast(arima5, h = 20), validate.ts)


# Forecast plot of next 12 months
plot(forecast(arima5, h = 12), ylab = "Gas production", xlab = "Year")

# Residuals tests
acf(arima5$residuals, main = 'ACF plot of residuals of auto-arima model')
pacf(arima5$residuals, main = 'PACF plot of residuals of auto-arima model')

plot(density(arima5$residuals))
qqnorm(arima5$residuals)
qqline(arima5$residuals)

Box.test(arima5$residuals, type = 'Ljung-Box') # p-value = 0.9207 > significance level = 0.05 : Accept H0 => Model is good fit


######## Comparing the models using AIC , BIC 

Comparision_Models <- data.frame(Model = c('ARIMA(3,0,3)(0,1,0)[12]', 'ARIMA(3,0,3)(1,1,1)[12]', 'log-tranformed : ARIMA(3,0,4)(0,1,0)[12]', 'Auto-Arima : ARIMA(2,1,1)(0,1,2)[12]'), 
              AIC = c(arima2$aic, arima3$aic, arima4$aic, arima5$aic), Log_liklihood = c(arima2$loglik, arima3$loglik, arima4$loglik, arima5$loglik))


# From comparison table, we can conclude that the model with minimum AIC and maximum log liklihood is ARIMA(3,0,4)(0,1,0)[12] model and 
# second best model is ARIMA(2,1,1)(0,1,2)[12] model