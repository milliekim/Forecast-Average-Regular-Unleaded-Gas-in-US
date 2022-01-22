######################################################################
# Project: Forecasting Average Regular Unleaded Gas 
#######################################################################

library(fpp3)
library(tidyverse)
library(urca)

# Load gas data set  
FREDrevised= readr::read_csv("REGUNLEADEDgas.csv")
FREDrevised
view(FREDrevised)

# examining data for NAs:
FREDrevised %>% summarise(count = sum(is.na(APU000074714)))
FREDrevised %>% summarise(count = sum(is.na(DATE)))
FREDrevised %>% summarise(count = sum(is.na(CPIAUCSL)))


# convert tibble into tsibble and renaming columns
# DATE format is year-month-day(1/1/1990 -9/1/2021)
AVGgas= FREDrevised %>% rename(GAS=APU000074714,
                               INFLATION= CPIAUCSL) %>% 
  mutate(DATE= yearmonth(DATE)) %>% 
  as_tsibble(index= DATE)
# No NAs present in GAS or CRUDEOIL

# GAS data set is not seasonally adjusted.
# INFLATION data set is seasonally adjusted. 

# AVERAGEgas obtained from: https://fred.stlouisfed.org/series/APU000074714
# INFLATION obtained from: https://fred.stlouisfed.org/series/CPIAUCSL
#CPIAUCLS.csv as INFLATION data

AVGgas
view(AVGgas)


### Create holdout data set: last four observations are "held out" to create 
# training/hold-out set. 
### This will set the hold-out data set to run from January 1990 to May 2021.

# hold out data(last 4 observations): 2021 June - 2021 Sept

# create restricted training/hold out set by filtering out last four months
GAShold= AVGgas %>% filter_index("1990 Jan" ~ "2021 May")

# create autoplot of GAS data
GAShold %>% autoplot(GAS) + ylab("Average Price Unleaded Regular Gas")

# Inflation as an Exogenous variable:
# create autoplot of GAS data
GAShold %>% autoplot(INFLATION)


# A plot of the GAShold data set presents with significant volatility and nonlinear trend, 
# seen especially from January 2000 to the end of the data set, therefore a 
# Box-Cox transformation is warranted. 
# A Box-Cox transformation is clearly recommended to stabilize the volatility in the data set 
#to prepare the data for the use of linear methods on the data set. The Box-Cox transformation 
# transforms  the data set to follow a more normally distributed data set, so that 
# the application of tests and confidence limits can be applied more readily. 

# For the GAShold data set we will consider both the Guerrero feature in R and 
# the log-transformation to determine the best value to apply to the data set to
# reduce volatility.


# Considering a Box-Cox transformation:
GAShold %>% gg_season(GAS)
# increasing and changing seasonal volatility is an appropriate reason to apply 
# Gas prices tend to have daily volatility
# the Box_Cox transformation along with the changing magnitude of the variance in the data.

# Prior to 1999, little volatility is seen in the data set, however, volatility is seen to 
# increase over the years.


GAShold %>% gg_season(INFLATION)
# Since inflation is a Consumer Price Index, we will take the log transformation to get the 
#   value of inflation

### Transform data set using Box-Cox transformation.
# obtaining an initial value of lambda using the Guerrero feature in R:
lambda = GAShold %>% features(GAS, guerrero) %>% pull(lambda_guerrero)
lambda

# The estimated lambda value of -0.51 will have the effect of transforming very 
# large data points into smaller numbers;reducing the volatility of the data set.

# added column, "transform", which is the Box-Cox transformation of GAS column
GAShold= GAShold %>% 
  mutate(transform=box_cox(GAS, lambda))

GAShold
view(GAShold)
# plot of gas holdout set;

# Comparing the Box-Cox transformation with the log transformation of GAS
GAShold %>% autoplot(transform) + ylab("Lambda-transform of AVG Price Unleaded Reg Gas")

GAShold %>% autoplot(log(GAS)) + ylab("Log-transformed of AVG Price Unleaded Reg Gas")

# We will consider the simpler, log transformation as the lambda transformation could be 
# effected by outliers in the data set, such as that seen in the 2008 financial crisis as 
# price of barrel of crude oil fell from $133.88 to $39.09 in less than a year. 
# This event occurred in conjunction with a rise in unemployment as companies reduced output 
# since demand was failing.


# The log-transformation of GAS data seems to have improved the volatility in 
# the data better than the lambda estimation. We will use the log-transformation on the
# GAS data.

# The GAShold data does not appear to be generated from a covariance-stationary model. 
# A covariance stationary model exhibits: 
# 1. The expected mean of Yt is constant for all Yt, 
# 2. The expected variance is constant for all t, and finally,
# 3. the covariance and correlation of Yt 
# with its past lagged values, Yt-k, is also constant, meaning the errors as it relates to 
# previous lags of Yt are also constant for all t.

# The data does not appear to be covariance stationary since the upward trend in the 
# data suggests a violation of the the mean for all t. 
# Potential seasonal patterns in the data also violate this requirement.

# Differencing and de-trending of the data set may be needed to achieve covariance-stationarity. 
# We will test for these conditions using the KPSS test.


######## STEP 1: BOX-Jenkins Methodology###########################

# Check for Stationarity

# Checking for seasonal differencing of GAS and INFLATION data sets:
GAShold %>% features(log(GAS), unitroot_nsdiffs)

GAShold %>% features(log(INFLATION), unitroot_nsdiffs)

# The test returns a 0; therefore, no seasonal root is found and no seasonal 
# differencing is needed.

# Now, we will check for a non-seasonal root by applying the KPSS unit root test.
# Checking for non-seasonal differencing; KPSS test
GAShold %>% features(log(GAS), unitroot_kpss)

# The null hypothesis is that the data set is trend-stationary. 
# The KPSS test for a unit root has returned 0.01; therefore, 
# we reject the null hypothesis that the data is trend-stationary because it has 
# a p-value of less than 0.05 or less than %5, hence, 
# the KPSS statistic of 5.065712 is greater than the 5% critical value of 0.463. 
# A rejection of a null hypothesis means that the data is not trend-stationary 
# and the data needs differencing. 

# evaluation of model after first differencing
GAShold %>% features(difference(log(GAS)), unitroot_kpss)

# The differenced data set has revealed a KPSS test p-value of 0.1 or 10%. 
# Since the p-value exceeds 0.05 the series is considered trend-stationary and 
# the KPSS test statistic 0.03666673 < the 5% KPSS critical value, 0.463, therefore, 
# we fail to reject the null hypothesis.

# Consequently, we set d=1 and D=0.

# Irregular and volatile seasonal patterns seen on gg_season(GAShold) display of data
GAShold %>% gg_season(INFLATION)

# Deterministic trends: From the log-transformed GAShold plot, there appears to be 
# no consistent trend over the entire time span of the data set.
# The GAShold data set appears to trend upward from approximately 1998 
# then levels off starting at approximately 2008.

GAShold %>% autoplot(log(GAS)) + ylab("Log-transformed of AVG Price Unleaded Reg Gas")

# Other variation (anything other than trend/seasonality): an obvious "shock" appears 
# to have occurred in 2008. 
# This could correspond to the impact of the 2008 financial crisis as discussed earlier. 
# It does not appear the data has returned to its prior status before the 2008 financial crisis.


####### STEP 2: Box-Jenkins Methodology###############################

# Make initial guesses about the most appropriate model

# Visualization of ACF, PACF plots:
GAShold %>% filter_index("1990 Jan"~"2021 September") %>%
             gg_tsdisplay(difference(log(GAS)), lag_max=60, plot_type="partial")


# A distinct sinusoidal, periodic geometric behavior is demonstrated in ACF plot 
# which could demonstrate AR(p) process, with significant p lags seen on the PACF plot 
# at lag 1 and 2; 
# this could indicate an AR(2) model. 
# Potential seasonal lags are noted  at lags 6, 11, 25, 28, 29, 48, 54, and 60 
# suggesting a SAR(P) component. 

# A significant lag 1 seen on ACF plot could represent an MA(1) process with 
# sinusoidal decay noted in the corresponding PACF plot.

# Selected 60 lags to view any possible seasonality and periodicity in the data.

# We will proceed with testing out various SARIMA models:

####### STEP 3: Box-Jenkins Methodology##################################

# Estimate your candidate models and select one


# Testing of various ARIMA/SARIMA models:

fit= GAShold %>% model(model1= ARIMA(log(GAS)~pdq(2,1,0)+PDQ(0,0,0)),
                       model2= ARIMA(log(GAS)~pdq(2,1,0)+PDQ(1,0,0)),
                       model3= ARIMA(log(GAS)~pdq(2,1,1)+PDQ(0,0,0)),
                       model4= ARIMA(log(GAS)~pdq(2,1,1)+PDQ(1,0,0)),
                       model5= ARIMA(log(GAS)~pdq(2,1,2)+PDQ(1,0,0)),
                       model6= ARIMA(log(GAS)~pdq(2,1,1)+PDQ(2,0,0)),
                       model7= ARIMA(log(GAS)~pdq(2,1,2)+PDQ(2,0,0)),
                       model8= ARIMA(log(GAS)~pdq(3,1,1)+PDQ(2,0,0)),
                       model9= ARIMA(log(GAS)~pdq(4,1,0)+PDQ(2,0,0)),
                       model10= ARIMA(log(GAS)~0+pdq(4,1,1)+PDQ(4,0,0)),
                       model11= ARIMA(log(GAS)~pdq(4,1,1)+PDQ(1,0,0)),
                       model12= ARIMA(log(GAS)~0+pdq(4,1,1)+PDQ(1,0,1)),
                       model13= ARIMA(log(GAS)~0+pdq(4,1,2)+PDQ(1,0,1)),
                       model14= ARIMA(log(GAS)~0+pdq(4,1,3)+PDQ(4,0,2))
  
)

glance(fit)

# Model with lowest BIC = -1157.64
report(GAShold %>% model(ARIMA(log(GAS)~pdq(2,1,0)+PDQ(0,0,0))))
gg_tsresiduals(GAShold %>% model(ARIMA(log(GAS)~pdq(2,1,0)+PDQ(0,0,0))))


# Model with lowest AICc: -1190.8
report(GAShold %>% model(ARIMA(log(GAS)~0+pdq(4,1,3)+PDQ(4,0,2))))
gg_tsresiduals(GAShold %>% model(ARIMA(log(GAS)~0+pdq(4,1,3)+PDQ(4,0,2))))

#model with second lowest AIC:
report(GAShold %>% model(ARIMA(log(GAS)~0+pdq(4,1,2)+PDQ(1,0,1))))
gg_tsresiduals(GAShold %>% model(ARIMA(log(GAS)~0+pdq(4,1,2)+PDQ(1,0,1))))


# Best model with lowest AICc: SARIMA(4,1,3)x(4,0,2) with AIC=-1190.8; with marginally 
# significant lag at 25

# Best model with lowest BIC: ARIMA(2,1,0)x(0,0,0) with BIC: -1157.64

# We will proceed with the SARIMA(4,1,3)x(4,0,2) model since this model 
# appears to have less significant lags than the former model.

#################################################################################
# We have selected to forecast the Average Regular Unleaded Gas data set with a 
# multivariate regression model and neural net model.
#################################################################################

# We will now proceed with examining Inflation as an exogenous variable to 
# add to the GAShold SARIMA(4,1,3)x(4,0,2) model.


# Testing several models using the exogenous variable, INFLATION

# We take the lagged difference of the log(INFLATION) because gas prices 
#   are based on the lagged value of inflation.

ARIMAgas1= GAShold %>% model(ARIMA(log(GAS)~0+pdq(4,1,3)+PDQ(4,0,2)+ lag(INFLATION)))
ARIMAgas1 %>% gg_tsresiduals(lag=60)
report(ARIMAgas1)
augment(ARIMAgas1) %>% features(.innov, ljung_box, lags=60, Dof=15)

residuals(ARIMAgas1) %>% gg_tsdisplay(.resid, plot_type="partial", lag_max=60)
#AICc: -1179.47  BIC: -1121.86  AIC: -1180.8  Ljung-Box: 0.977


ARIMAgas2= GAShold %>% model(ARIMA(log(GAS)~0+pdq(2,1,1)+PDQ(0,0,0)+lag(INFLATION)))
ARIMAgas2 %>% gg_tsresiduals(lag=60)
report(ARIMAgas2)
augment(ARIMAgas2) %>% features(.innov, ljung_box, lags=60, Dof=5)

residuals(ARIMAgas2) %>% gg_tsdisplay(.resid, plot_type="partial", lag_max=60)
# AICc: -1167.79   BIC: -1148.3  AIC: -1167.95  Ljung-Box: 0.502


ARIMAgas3= GAShold %>% model(ARIMA(log(GAS)~0+pdq(4,1,2)+PDQ(1,0,1)+lag(INFLATION)))
ARIMAgas3 %>% gg_tsresiduals(lag=60)
report(ARIMAgas3)
augment(ARIMAgas3) %>% features(.innov, ljung_box, lags=60, Dof=10)

residuals(ARIMAgas3) %>% gg_tsdisplay(.resid, plot_type="partial", lag_max=60)
# BIC: -1144.71  Ljung-Box: 0.936

# Forecasting the ARIMA with Exogenous variables:

# STEP 4: Box-Jenkins Methodology

# Confirm preliminary final model passes diagnosic test: Ljung-Box test

# ARIMAgas1 is chosen as our final model with the exogenous variable since it has the 
#lowest AICc
# of the models tested with the exogenous variable.
ARIMAgas1= GAShold %>% model(ARIMA(log(GAS)~0+pdq(4,1,3)+PDQ(4,0,2)+ lag(INFLATION)))
ARIMAgas1 %>% gg_tsresiduals(lag=60)
report(ARIMAgas1)
augment(ARIMAgas1) %>% features(.innov, ljung_box, lags=60, Dof=15)
#AICc: -1179.47  BIC: -1121.86  AIC: -1180.8  Ljung-Box: 0.963
# Ljung-Box reveals test reveals a p-value of 0.963, thus we fail to reject the null hypothesis
# We fail to reject that the first 60 residual autocorrelation coefficients are jointly zero.
# Thus, we have sufficient evidence to conclude our model is appropriately fitted to the data; 
#hence, we are clear to forecast with this model.

# Forecasting the ARIMA with the inflation exogenous variable:
future= new_data(GAShold, 4) %>% mutate(INFLATION= c(263.161, 264.793, 266.832, 268.551))

MODELforecast= ARIMAgas1 %>% forecast(future) %>% 
  autoplot(AVGgas %>% filter_index("2019 Jan"~"2021 September"), level=95)

MODELforecast

# observations of the series:
finalMODEL= ARIMAgas1 %>% forecast(future)

compare= filter_index(AVGgas, "2021 June" ~ "2021 September") %>% 
  mutate(FORE=finalMODEL$.mean)

compare                         

# Analysis:

# The forecast appears to be contained within the 95% confidence bands however it 
# deviates away from the known data just after July.
# The forecast predicts a dramatic decrease in gas prices which is not indicated in 
# the actual data set.
# The forecast could cause investors to not trust the forecasted gas prices and therefore 
# to not invest as money in gas delivery contracts since the price of gas was forecasted lower.


########################################

# Second Model - Neural net 
#########################################
# Second model: Neural net Autoregressive forecasting method
# Applied lagged values of the log-transform GAS prices as inputs.
# We applied the feed-forward network with a single hidden layer in which 
#    there are 4 lagged input Autoregressive values, p=4, and a seasonal Autoregressive input, 
# P=4.
# Defining a model with AR(p=4) and a Seasonal AR(P=4) components coinciding with 
# lowest AICc model.
# To account for the potential volatility of daily gas prices, 
#    we have added daily Fourier terms for a thirteen-day period and 2 weekly Fourier terms.
# We also acknowledge the possiblity of multiple seasonalities as noted by 
# the gg_season plot of GAS and understanding that gas prices can change on a daily basis.
# Therefore, we have applied daily Fourier terms for a thirteen-day period and 2 weekly Fourier
# terms.
netMODEL= GAShold %>% model(NNETAR(log(GAS)~AR(P=4, p=4) + fourier(period=13, K=2)))

netMODEL %>% forecast(h=4, times=100) %>% 
  autoplot(AVGgas %>% filter_index("2020 Jan"~"2021 September"), level=95)

# observations of the series:
finalMODEL2= netMODEL %>% forecast(h=4, times=100) %>% 
  autoplot(AVGgas %>% filter_index("2020 Jan"~"2021 September"), level=95)

compare= filter_index(AVGgas, "2021 June" ~ "2021 September") %>% 
  mutate(FORE2=finalMODEL2$.mean)

compare


# Analysis:

# The neural net 4-step ahead forecasted model appears to forecast the monthly 
#    gas prices just under the known data set . 
# The forecast is well within the 95% confidence bands and 
#    appears more accurate than the Dynamic Regression with SARIMA errors model.
# By following the indicated forecast, investors can feel confident regarding their 
# purchases for the future.

netMODEL6= GAShold %>% model(NNETAR(log(GAS)~AR(P=4, p=4) + fourier(period=13, K=2)))

netMODEL6 %>% forecast(h=6, times=100) %>% 
  autoplot(AVGgas %>% 
             filter_index("2020 Jan"~"2022 Jan"), level=95) + 
                   labs(y= "Average Price of Unleaded Regular Gas")

# observations of the series:
finalMODEL3= netMODEL6 %>% forecast(h=6, times=100) %>% 
  filter_index("2020 Jan"~"2021 September")

compare= filter_index(AVGgas, "2021 June" ~ "2021 November") %>% 
  mutate(FORE3=finalMODEL3$.mean)

View(compare)


# Analysis:

# We chose the neural net autoregressive forecasting method as is appears to have 
# forecasted the  known data set more accurately than the Dynamic Regression with
# SARIMA errors method.
# It appears gas prices trend down following the cessation of the known data set 
# indicating a decrease in average gas prices.
# This impact could influence investors to purchase more gas in the coming months
#   for gas delivery since gas prices will be decreasing. 
# The forecast is well within the 95% confidence bands and investors can have confidence in 
#   purchasing more gas since the forecast appears to be fairly accurate.
























