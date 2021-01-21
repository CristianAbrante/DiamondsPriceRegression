#----------------------------------------------------------------------
#      Homework 3: Total credit to the non-financial private sector
#----------------------------------------------------------------------
#                  Statistical data analisys

# Group: Ángel González, Álvaro Arranz, Cristian Abrante, Daniel Saiz

# - Prerequisites:
# Install necesary packages if not currently installed.

if (!require("xlsx"))
  install.packages("xlsx")
if (!require("tseries"))
  install.packages("tseries")
if (!require("zoo"))
  install.packages("zoo")
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("ggfortify"))
  install.packages("ggfortify")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("forecast"))
  install.packages("forecast")
if (!require("fpp"))
  install.packages("fpp")
if (!require("fpp2"))
  install.packages("fpp2")
if (!require("astsa"))
  install.packages("astsa")
if (!require("car"))
  install.packages("car")

# Load libraries
library(xlsx)
library(tseries)
library(zoo)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(forecast)
library(fpp)
library(fpp2)
library(gridExtra)
library("astsa")
library(car)

# Load data
# You have to set the current directory in order to load the data.

totalCredit <- read.xlsx("./data/data_g11.xlsx", sheetIndex = 1)


##########################################################################
# 1. Plot the series and briefly comment on the characteristics you 
#    observe (stationarity, trend, seasonality, ...).
##########################################################################

totalCredit.ts <- ts(totalCredit[,2], frequency=12, start=c(2000,1))

## Time Plot of Total credit to the non-financial private sector not Pretty
autoplot(
  totalCredit.ts, 
  xlab="Total credit to the non-financial private sector"
)

## Time Plot of Total credit to the non-financial private sector Pretty
ggplot(totalCredit, aes(x = Fecha, y = Tasa)) +
  geom_line(color = "steelblue") +
  xlab("Total credit to the non-financial private sector")+ 
  geom_point() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month")

# Exploring seasonality (declared frequency must be >1), seasonplot in package forecast:
ggmonthplot(totalCredit.ts)
ggseasonplot(totalCredit.ts)

##########################################################################
# 2. Question 2
##########################################################################

stl1 <- stl(totalCredit.ts, t.window=5, s.window = "periodic", robust = TRUE)
autoplot(stl1)

stl2 <- stl(totalCredit.ts, t.window=15, s.window = "periodic", robust = TRUE)
autoplot(stl2)

ggAcf(totalCredit.ts, lag.max = NULL)
ggAcf(totalCredit.ts, lag.max = NULL, type = "partial")
acf2(totalCredit.ts)

pred <- forecast(totalCredit.ts, h=12)
autoplot(pred)

stl2.remainder <- stl1$time.series[,3]
acf2(stl2.remainder)

##########################################################################
# 3. Fit an ARIMA model to your time series.
##########################################################################

# We are going to display the data and the log transformed data, in order
# to decide which with variable to use.

tsdisplay(totalCredit.ts, plot.type = "scatter")
tsdisplay(log(totalCredit.ts), plot.type = "scatter")

# Identify if there is a seasonal component in the time series
tsdisplay(totalCredit.ts, plot.type = "spectrum")
Acf(totalCredit.ts)
seasonplot(totalCredit.ts)

# computation of the period from the series to repeat.
spec.pgram(totalCredit.ts) 
which.max(spec.pgram(totalCredit.ts)$spec)
spec.pgram(totalCredit.ts)$freq[3]

# Decide the values of d and D components

# First we are going to check if the ts is
# statitionary using kpss and adf.
kpss.test(totalCredit.ts) # null is that the ts is stationary.
adf.test(totalCredit.ts)  # null is that the ts is not stationary.

# Then we are going to compute the number of differences.
# For regular part
ndiffs(totalCredit.ts, test = "kpss")
ndiffs(totalCredit.ts, test = "adf")

# For seasonal part.
nsdiffs(totalCredit.ts)
nsdiffs(totalCredit.ts)

# Decide the values of p, q, P, Q
# First, we have to plot the differences, with the seasonality obtained before
acf2(diff(diff(totalCredit.ts,12)))

# We are going to fit three different models
# p = 0, q = 0, P = 0, Q = 0
totalCredit.model1 <- Arima(totalCredit.ts, order=c(0,1,0),include.constant=1,seasonal=list(order=c(0,0,0), period=12))
Box.test(residuals(totalCredit.model1), lag=12)
t.test(residuals(totalCredit.model1))
checkresiduals(totalCredit.model1)
totalCredit.model1$aicc

# p = 1, q = 0, P = 0, Q = 0
totalCredit.model2 <- Arima(totalCredit.ts, order=c(1,1,0),include.constant=1,seasonal=list(order=c(0,0,0), period=12))
Box.test(residuals(totalCredit.model2), lag=12)
t.test(residuals(totalCredit.model2))
checkresiduals(totalCredit.model2)
totalCredit.model2$aicc

# p = 1, q = 1, P = 0, Q = 0
totalCredit.model3 <- Arima(totalCredit.ts, order=c(1,1,1),include.constant=1,seasonal=list(order=c(0,0,0), period=12))
Box.test(residuals(totalCredit.model3), lag=12)
t.test(residuals(totalCredit.model3))
checkresiduals(totalCredit.model3)
totalCredit.model3$aicc

# p = 1, q = 1, P = 1, Q = 0
totalCredit.model4 <- Arima(totalCredit.ts, order=c(1,1,1),include.constant=1,seasonal=list(order=c(1,0,0), period=12))
Box.test(residuals(totalCredit.model4), lag=12)
t.test(residuals(totalCredit.model4))
checkresiduals(totalCredit.model4)
totalCredit.model4$aicc

# p = 1, q = 1, P = 1, Q = 1
totalCredit.model5 <- Arima(totalCredit.ts, order=c(1,1,1),include.constant=1,seasonal=list(order=c(1,0,1), period=12))
Box.test(residuals(totalCredit.model5), lag=12)
t.test(residuals(totalCredit.model5))
checkresiduals(totalCredit.model5)
totalCredit.model5$aicc

# Based on AICc we chose model 4 AICc = 137.55
# We have to check assumptions on residuals.
jarque.bera.test(residuals(totalCredit.model4)) # low p-value so residuals are not normal.
qqPlot(residuals(totalCredit.model4))
order(residuals(totalCredit.model4))
jarque.bera.test(totalCredit.model4$residuals[-c(6,9,33,4)])
qqPlot(totalCredit.model4$residuals[-c(6,9,33,4)])

# We check the correlations of the residuals.
cov2cor(totalCredit.model4$var.coef)
totalCredit.model4


# We compare with the automatic arima model.
totalCredit.autoModel <- auto.arima(totalCredit.ts, d = 1, D = 0)
totalCredit.autoModel
jarque.bera.test(residuals(totalCredit.autoModel))
qqPlot(residuals(totalCredit.autoModel))
t.test(residuals(totalCredit.autoModel))
Box.test(residuals(totalCredit.autoModel), lag=12)
tsdisplay(totalCredit.autoModel$residuals, plot.type="spectrum")

##########################################################################
# 4. Forecasting
##########################################################################
# USE MODEL 4 FOR FORECASTING

autoplot(forecast(totalCredit.model4, h=12))


##########################################################################
# 5. RMSE
##########################################################################


getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]   #train data end
  test.start <- time(x)[length(x)-h+1]  #test data start
  train <- window(x,end=train.end) #extract train data
  test <- window(x,start=test.start)  #extract test data
  fit <- Arima(train,...) # fit model with train data
  fc <- forecast(fit,h=h) # forecast with model
  return(forecast::accuracy(fc,test)[2,"RMSE"]) #compare forecast with test data, extract the rmse
}

model1rmse <- getrmse(totalCredit.ts,order=c(0,1,0), seasonal=c(0,0,0),h=12)
model2rmse <- getrmse(totalCredit.ts,order=c(1,1,0), seasonal=c(0,0,0),h=12)
model3rmse <- getrmse(totalCredit.ts,order=c(1,1,1), seasonal=c(1,0,1),h=12)
model4rmse <- getrmse(totalCredit.ts,order=c(1,1,1), seasonal=c(1,0,0),h=12)
model5rmse <- getrmse(totalCredit.ts,order=c(1,1,1), seasonal=c(1,0,1),h=12)

rmseTotal <- c(model1rmse, model2rmse, model3rmse, model4rmse, model5rmse)
rmseTotal

plot(rmseTotal, xlab="model number", ylab="RMSE")
