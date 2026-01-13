# POST SUBMISSION WORK

# Load relevant libraries
library(tidyverse)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(fpp2) # package from the "Forecasting in R" DataCamp course
library(glmnet)
library(caret)
library(randomForest)
library(ranger)
library(writexl)
library(gridExtra)
library(car)
library(MASS)
library(fpp3)
library(lmtest)
library(sandwich)
library(dynlm)
library(zoo)
library(xts)
library(astsa)
library(vars)


#### PART 1: General relationship between BTC and USDT ####


# Load in data

## 1) Daily BTC and Tether transactions data
DailyData <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/daily_btc_usdt.csv")

names(DailyData)


## 2) Daily BTC and Tether transactions data CLEAN
DailyDataClean <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Daily_data_clean.csv")

names(DailyDataClean)

DailyDataClean <- DailyDataClean %>% mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close)))
DailyDataClean <- DailyDataClean %>% mutate(USDT_diff_log_transactions = log(total_transactions) - lag(log(total_transactions)))
DailyDataClean <- DailyDataClean %>% mutate(USDT_diff_log_avg_transactions = log(average_transaction_size) - lag(log(average_transaction_size)))
DailyDataClean <- DailyDataClean %>% mutate(BTC_log_volume = log(btc_volume) - lag(log(btc_volume))) 
DailyDataClean <- DailyDataClean %>% mutate(BTC_volatility = abs(BTC_log_returns)) 

names(DailyDataClean)



## 3) Tether (USDT) data cleaned
USDTDataClean <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Tether_Clean.csv")

names(USDTDataClean)




cor(DailyData$Close, DailyData$total_transactions, use = "complete.obs") # -0.0021 (weak negative) 









##### MODELS: For each model, need to take into account time dependence #####

# Genrate new variables 
## Generate lags
### Tether transactions lags
DailyData <- DailyData %>%
  mutate(
    lag1_trans = dplyr::lag(total_transactions, 1),
    lag2_trans = dplyr::lag(total_transactions, 2),
    lag3_trans = dplyr::lag(total_transactions, 3)
  )


DailyData <- DailyData %>%
  mutate(
    lag4_trans = dplyr::lag(total_transactions, 4),
    lag5_trans = dplyr::lag(total_transactions, 5)
  )


### BTC/USD lags
DailyData <- DailyData %>%
  mutate(
    lag1_btc = dplyr::lag(Close, 1),
    lag2_btc = dplyr::lag(Close, 2),
    lag3_btc = dplyr::lag(Close, 3)
  )



# Create columns of BTC price difference
## Close - Open
DailyData <- DailyData %>%
  mutate(
    BTC_Difference = Close - Open
    )

## Binary: if BTC increased, 1, else 0
DailyData <- DailyData %>%
  mutate(
    BTC_Increase = ifelse(Close - Open > 0, 1, 0)
    )





# MODEL FAMILY 1: Probabilistic models
Logit <- glm(BTC_Increase ~ log(total_transactions), data = DailyData, family = binomial(link = "logit"))

summary(Logit)


# Probit model
Probit <- glm(BTC_Increase ~ total_transactions, data = DailyData, family = binomial(link = "probit"))

summary(Probit) # Exact same AIC in Logit and Probit model (AIC = 128.4)

plot(predict(Logit, type="response") ~ DailyData$total_transactions)



# Logistic regression with time dependence
DailyData$BTC_Increase1 <- dplyr::lag(DailyData$BTC_Increase, 1)
DailyData$tt_lag1 <- dplyr::lag(DailyData$total_transactions, 1)



Logit_TD <- glm(BTC_Increase ~ total_transactions + tt_lag1 + BTC_Increase1, 
                      data = DailyData, 
                      family = binomial(link = "logit"))

summary(Logit_TD)





# Logistic regression with average transactionn size rather than total
Logit_TD_AvgTr <- glm(BTC_Increase ~ average_transaction_size + BTC_Increase1, 
                      data = DailyData, 
                      family = binomial(link = "logit"))

summary(Logit_TD_AvgTr)


# Same with Probit model
Probit_AvgTr <- glm(BTC_Increase ~ average_transaction_size, data = DailyData, family = binomial(link = "probit"))

summary(Probit_AvgTr) # AIC = 128.71









# MODEL FAMILY 2: Distributed lag models (regression model includes not only current but also lagged values of explanatory variables)



## Model with one lagged value
DLM <- lm(Close ~ total_transactions + lag1_trans, data = DailyData)

summary(DLM)

AIC(DLM) # 1646.743
BIC(DLM) # 1656.698



## Model with one lagged BTC value, one Tether lagged value

DLM_AR <- lm(Close ~ lag1_btc + total_transactions + lag1_trans, data = DailyData)

summary(DLM_AR) # Probably slightly better model for p values than the one with no BTC lag


AIC(DLM_AR) # 1413.162
BIC(DLM_AR) # 1425.605



## Model with 3 Tether lags

DLM_3 <- lm(Close ~ total_transactions + lag1_trans + lag2_trans + lag3_trans, data = DailyData)

summary(DLM_3) # This confirms that BTC lag is needed


AIC(DLM_3) # 1608.321
BIC(DLM_3) # 1623.116




## Model with one BTC lag and 3 Tether lags

DLM_AR3 <- lm(Close ~ lag1_btc + total_transactions + lag1_trans + lag2_trans + lag3_trans, data = DailyData)

summary(DLM_AR3) # Slightly better p values


AIC(DLM_AR3) # 1380.019
BIC(DLM_AR3) # 1397.281




DailyData_zoo <- zoo(
  DailyData[, c("Close", "total_transactions")],
  order.by = DailyData$date
)

DLM_AR4 <- dynlm(Close ~ L(Close, 1) + L(total_transactions, 0:4), data = DailyData_zoo)
summary(DLM_AR4)
AIC(DLM_AR4) # 1364.669
BIC(DLM_AR4) # 1384.304


DLM_AR5 <- dynlm(Close ~ L(Close, 1) + L(total_transactions, 0:5), data = DailyData_zoo)
summary(DLM_AR5) # highest R squared
AIC(DLM_AR5) # 1340.162
BIC(DLM_AR5) # 1362.146


DLM_AR6 <- dynlm(Close ~ L(Close, 1) + L(total_transactions, 0:6), data = DailyData_zoo)
summary(DLM_AR6) # higher standard error, less significant estimats, lower R squared
AIC(DLM_AR6) # 1326.391
BIC(DLM_AR6) # 1350.7


DLM_AR7 <- dynlm(Close ~ L(Close, 1) + L(total_transactions, 0:7), data = DailyData_zoo)
summary(DLM_AR7)
AIC(DLM_AR7) # 1313.047
BIC(DLM_AR7) # 1339.654


DLM_AR8 <- dynlm(Close ~ L(Close, 1) + L(total_transactions, 0:8), data = DailyData_zoo)
summary(DLM_AR8)
AIC(DLM_AR8) # 1298.321
BIC(DLM_AR8) # 1327.201


DLM_AR9 <- dynlm(Close ~ L(Close, 1) + L(total_transactions, 0:9), data = DailyData_zoo)
summary(DLM_AR9)
AIC(DLM_AR9) # 1284.511
BIC(DLM_AR9) # 1315.639


DLM_AR10 <- dynlm(Close ~ L(Close, 1) + L(total_transactions, 0:10), data = DailyData_zoo)
summary(DLM_AR10) # Only BTC lag is significant, lower R squared
AIC(DLM_AR10) # 1267.977
BIC(DLM_AR10) # 1301.325



# Double check model with differenced data

DailyDataClean_zoo <- zoo(
  DailyDataClean[, c("BTC_log_returns", "USDT_diff_log_transactions")],
  order.by = DailyDataClean$date
)

DailyDataClean_zoo_avgt <- zoo(
  DailyDataClean[, c("BTC_log_returns", "diff_log_average_USDT")],
  order.by = DailyDataClean$date
)

# Average transaction size model
DLM_AR_avgt_diff <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(diff_log_average_USDT, 0:1), data = DailyDataClean_zoo_avgt)
summary(DLM_AR_avgt_diff) # 
AIC(DLM_AR_avgt_diff) # 
BIC(DLM_AR_avgt_diff) # 





DLM_AR_diff_simp <- dynlm(BTC_log_returns ~ USDT_diff_log_transactions, data = DailyDataClean_zoo)
summary(DLM_AR_diff_simp) # R^2 = 0.02
AIC(DLM_AR_diff_simp) # -241
BIC(DLM_AR_diff_simp) # -234


DLM_AR0_diff <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + USDT_diff_log_transactions, data = DailyDataClean_zoo)
summary(DLM_AR0_diff) # R^2 = 0.02143
AIC(DLM_AR0_diff) # -239
BIC(DLM_AR0_diff) # -229


DLM_AR1_diff <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(USDT_diff_log_transactions, 0:1), data = DailyDataClean_zoo)
summary(DLM_AR1_diff) # R^2 = 0.03661
AIC(DLM_AR1_diff) # -238
BIC(DLM_AR1_diff) # -225


DLM_AR2_diff <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(USDT_diff_log_transactions, 0:2), data = DailyDataClean_zoo)
summary(DLM_AR2_diff) # R^2 = 0.04116
AIC(DLM_AR2_diff) # -232
BIC(DLM_AR2_diff) # -217

 
DLM_AR3_diff <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(USDT_diff_log_transactions, 0:3), data = DailyDataClean_zoo)
summary(DLM_AR3_diff) # R^2 = 0.05375
AIC(DLM_AR3_diff) # -228
BIC(DLM_AR3_diff) # -211


DLM_AR4_diff <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(USDT_diff_log_transactions, 0:4), data = DailyDataClean_zoo)
summary(DLM_AR4_diff) # R^2 = 0.05667
AIC(DLM_AR4_diff) # -226
BIC(DLM_AR4_diff) # -207


## Best model is lag5(transactions)
DLM_AR5_diff <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(USDT_diff_log_transactions, 0:5), data = DailyDataClean_zoo)
summary(DLM_AR5_diff) # low R squared (= 0.05623), high p value, low F statistic
AIC(DLM_AR5_diff) # -221
BIC(DLM_AR5_diff) # -199




# MODEL FAMILY 3: ARIMA MODELS

# Analysis of the Tether transactions data
## ts data
Datats <- xts(DailyDataClean[, 2:12], 
              order.by = as.Date(DailyDataClean$date))



## Auto.arima to select the best ARs for the USDT transactions
fit_USDT <- auto.arima(Datats$total_transactions)

fit_USDT_nl <- auto.arima(Datats$total_transactions, lambda = 0) # for some reason this gave better model

summary(fit_USDT) # nonsensical model
summary(fit_USDT_nl) # AIC=106.69   AICc=106.97   BIC=114.19


## Test seasonal ARIMA
fit_USDTs <- auto.arima(DailyDataClean$total_transactions, seasonal = TRUE)

summary(fit_USDTs) # nonsensical model



## Auto.arima does not recognise the differencing of the data, thus it gives nonsensincal models (ARIMA(0,0,0). Hence: Differencing data
### Difference data singularly
dUSDT <- diff(Datats$total_transactions)

### Difference data seasonally, as it looks like there is a 7 day pattern
ddUSDT <- diff(dUSDT, lag = 7) 


### Visualise differencing results 
acf2(Datats$total_transactions)
acf2(dUSDT, max.lag  = 90) # ACF: high lag 1 (suggests MA(1)) and others oscillating around 0, PACF: other than lag 1, others small: Hence, the trend has been removed
acf2(ddUSDT, max.lag = 90) # despite what graphed data may look like, differencing seasonally results in overdifferencing due to large spikes at lag 7 and 13


# Conclusion: difference singularly, NOT seasonally




# Average USDT transaction size differencing
davgUSDT <- diff(DailyDataClean$average_transaction_size)
ddavgUSDT <- diff(davgUSDT, lag = 7)
davgUSDT_onlylag <- diff(DailyDataClean$average_transaction_size, lag = 7)

acf2(DailyDataClean$average_transaction_size, max.lag = 90) # high ACF(1,2) and high PACF(1)
acf2(davgUSDT, max.lag = 90) 					      # high ACF and PACF at lag 1
acf2(ddavgUSDT, max.lag = 90) 				      # a lot more lags which are higher than 0.2
acf2(davgUSDT_onlylag) 						      # a lot of lags are high

# Conclusion: ARIMA(0,1,1) on logs or ARMA(0,1) on diff(log(avg))

fit_BTC_avgUSDT_as_xreg <- Arima(
    DailyDataClean$BTC_log_returns,
    order = c(0,0,1),
    xreg  = DailyDataClean$diff_log_average_USDT
)

summary(fit_BTC_avgUSDT_as_xreg) # high AIC and BIC values





## From the analysis of the ACFs, try 2 models: ARIMA(0,1,1) and ARIMA(1,1,0)
### ARIMA(0,1,1)
sarima(DailyDataClean$total_transactions, 0,1,1) # AIC = 40.61783  AICc = 40.6194  BIC = 40.70171


### ARIMA(1,1,0)
sarima(DailyDataClean$total_transactions, 1,1,0) # doesn't work






## Force the differenceing on the auto.arima models
### Singular differencing
fit_USDTd <- auto.arima(Datats$total_transactions, d=1, D=0)

summary(fit_USDTd) #  ARIMA(0,1,3): AIC=3609.82   AICc=3610.29   BIC=3619.77


### Seasonal differencing
fit_USDTD <- auto.arima(Datats$total_transactions, d=1, D=1)

summary(fit_USDTd) # AIC=3609.82   AICc=3610.29   BIC=3619.77 # Same model as before


### ARIMA (0,1,3)
sarima(DailyDataClean$total_transactions, 0,1,3) # AIC = 40.58216  AICc = 40.58751  BIC = 40.72197




## Conclusion: best model is between ARIMA(0,1,1) and ARIMA(0,1,3). Compare these models
fit_USDT_011 <- sarima(DailyDataClean$total_transactions, 0,1,1) # AIC = 40.61783  AICc = 40.6194  BIC = 40.70171

fit_USDT_013 <- sarima(DailyDataClean$total_transactions, 0,1,3) # AIC = 40.58216  AICc = 40.58751  BIC = 40.72197
summary(fit_USDT_013)


checkresiduals(fit_USDT_011$fit)
checkresiduals(fit_USDT_013$fit) 
# Residuals check: ARIMA(0,1,3) has lower AICc. Both models have p>0.05 which means residuals appear to be random (no soignificnt autocorrelation). However, p value for ARIMA(0,1,3) much higher
# Hence, ARIMA(0,1,3) better






# Use USDT model for Bitcoin?

## MA residuals from the Tether ARIMA model
USDT_MAresiduals <- fit_USDT_013$fit$residuals


btc_lag1  <- stats::lag(DailyDataClean$btc_close, 1)
tx_diff   <- diff(DailyDataClean$total_transactions)
ma_lag1   <- stats::lag(USDT_MAresiduals, 1)
ma_lag2   <- stats::lag(USDT_MAresiduals, 2)
ma_lag3   <- stats::lag(USDT_MAresiduals, 3)

btc_lag1  <- as.numeric(btc_lag1)
ma_lag1   <- as.numeric(ma_lag1)
ma_lag2   <- as.numeric(ma_lag2)
ma_lag3   <- as.numeric(ma_lag3)


## Difference BTC data
### Difference data singularly
dBTC <- diff(Datats$btc_close)

### Difference data seasonally, as it looks like there is a 7 day pattern
ddBTC <- diff(dUSDT, lag = 7) 


### Visualise differencing results 
acf2(Datats$btc_close) # ACF tapers off, PACF cuts off after lag 1: Hence, AR(1)
acf2(dBTC, max.lag  = 90) # no more autocorrelation
acf2(ddBTC, max.lag = 90) # overdifferencing

# Hence, ARIMA model for BTC could be ARIMA(1,1,0)
fit_BTC_simple <- sarima(Datats$btc_close, 1,1,0) # AIC = 15.906  AICc = 15.90756  BIC = 15.98988
summary(fit_BTC_simple)











fit_BTC_xreg <- Arima(
    dBTC,
    order = c(1,0,0),
    xreg  = dUSDT
)

summary(fit_BTC_xreg) # high AIC and BIC values





btc_diff <- diff(DailyDataClean$btc_close)

## Align lengths here:
btc_diff <- tail(btc_diff, nrow(xreg_df))



xreg_df <- data.frame(
  btc_lag1,
  tx_diff = c(NA, tx_diff),   # pad with leading NA to match length
  ma_lag1
)

# remove rows with NA
xreg_df <- na.omit(xreg_df)

xreg_mat <- as.matrix(xreg_df)


## Make sure xreg and dependent variable are same length
n <- nrow(xreg_df)
btc_trimmed <- tail(DailyDataClean$btc_close, n)



qr_x <- qr(xreg_mat)
qr_x$rank
ncol(xreg_mat)

findLinearCombos(xreg_mat)



## BTC model consistent with USDT model
fit_BTC <- auto.arima(y = btc_diff, xreg = xreg_mat)

summary(fit_BTC)


## Simple Auto ARIMA model 
fit_BTC_AAsimple <- auto.arima(y = Datats$btc_close, xreg = Datats$total_transactions)

summary(fit_BTC_AAsimple) # ARIMA(0,1,0), high AIC and BIC values






# Model with log(BTC) and log(transactions)
## ts data
Datats <- xts(DailyDataClean[, 2:15], 
              order.by = as.Date(DailyDataClean$date))


fit_BTC_AA_logs <- auto.arima(y = Datats$BTC_log_returns, xreg = Datats$USDT_diff_log_transactions)

summary(fit_BTC_AA_logs) # ARIMA(0,0,0)




# Models with log variables
## ARIMA with xreg
fit_BTC_arima <- arima(DailyDataClean$BTC_log_returns, order = c(1, 0, 0), xreg = DailyDataClean$USDT_diff_log_transactions)

summary(fit_BTC_arima) # RMSE = 0.06014192, MAPE = 133.3797, MASE = 0.6880479
AIC(fit_BTC_arima) # -239.7939
BIC(fit_BTC_arima) # -229.8393


## ARIMA without xreg
fit_BTC_arima_simple <- arima(DailyDataClean$BTC_log_returns, order = c(1, 0, 0))

summary(fit_BTC_arima_simple) # RMSE = 0.06083788, MAPE = 124.4856 (better error term here), MASE = 0.6944777
AIC(fit_BTC_arima_simple) # -239.7474
BIC(fit_BTC_arima_simple) # -232.2814: BIC better



















# MODEL FAMILY 3: Granger causality test

btc_diff <- diff(DailyDataClean$btc_close)
tx_diff   <- diff(DailyDataClean$total_transactions)

Differenced_data <- as.data.frame(na.omit(cbind(btc_diff, tx_diff)))

grangertest(btc_diff ~ tx_diff, order = 5, data = Differenced_data)   # USDT → BTC: p = 0.9865
grangertest(tx_diff ~ btc_diff, order = 5, data = Differenced_data)   # BTC → USDT: p = 0.09255, significant at the 10% level




## Model with logged variables
grangertest(BTC_log_returns ~ USDT_diff_log_transactions, order = 5, data = DailyDataClean)   # USDT → BTC: p = 0.9824
grangertest(USDT_diff_log_transactions ~ BTC_log_returns, order = 5, data = DailyDataClean)   # BTC → USDT: p = 0.3146

grangertest(BTC_log_returns ~ USDT_diff_log_transactions, order = 1, data = DailyDataClean)   # USDT → BTC: p = 0.9719
grangertest(USDT_diff_log_transactions ~ BTC_log_returns, order = 1, data = DailyDataClean)   # BTC → USDT: significant at 10% level




## Model with diff(log(avgUSDT))
grangertest(BTC_log_returns ~ diff_log_average_USDT, order = 5, data = DailyDataClean)   # USDT → BTC: p = 0.6743
grangertest(diff_log_average_USDT ~ BTC_log_returns, order = 5, data = DailyDataClean)   # BTC → USDT: p = 0.5269

grangertest(BTC_log_returns ~ diff_log_average_USDT, order = 1, data = DailyDataClean)   # USDT → BTC: p = 0.1358
grangertest(diff_log_average_USDT ~ BTC_log_returns, order = 1, data = DailyDataClean)   # BTC → USDT: significant at the 10% level





# MODEL FAMILY 4: Vector Auto Regression (VAR) and IRFs
VARselect(Differenced_data, lag.max = 10, type = "const")$selection

fit_VAR <- VAR(Differenced_data, p = 1, type = "const")
summary(fit_VAR)


## Impulse Response Functions (IRFs)
plot(irf(fit_VAR, impulse = "tx_diff", response = "btc_diff", boot = TRUE))
plot(irf(fit_VAR, impulse = "btc_diff", response = "tx_diff", boot = TRUE))
# Conclusion: there is a BTC effect on USDT, not the other way around



# VAR of logged variables
Differenced_data_logs <- as.data.frame(na.omit(
					cbind(BTC_log_returns = DailyDataClean$BTC_log_returns, 
						USDT_diff_log_transactions = DailyDataClean$USDT_diff_log_transactions)))

VARselect(Differenced_data_logs, lag.max = 10, type = "const")$selection


fit_VAR_logs <- VAR(Differenced_data_logs, p = 1, type = "const")
summary(fit_VAR_logs)


## Impulse Response Functions (IRFs)
plot(irf(fit_VAR_logs, impulse = "USDT_diff_log_transactions", response = "BTC_log_returns", boot = TRUE)) # 95% area is around 0 so no effect
plot(irf(fit_VAR_logs, impulse = "BTC_log_returns", response = "USDT_diff_log_transactions", boot = TRUE)) # about 10% fall after shock
# Conclusion: there is a BTC effect on USDT, not the other way around








# Data just with max transactions
DailyDataClean_MaxUSDTDaily <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/BTC_USDT_max_transactions_everyday.csv")

names(DailyDataClean_MaxUSDTDaily)

DailyDataClean_MaxUSDTDaily <- DailyDataClean_MaxUSDTDaily %>% 
					 mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close))) %>%
					 mutate(USDT_log_transactions = log(amount) - lag(log(amount)))


DailyDataMaxUSDT_zoo <- zoo(
  DailyDataClean_MaxUSDTDaily[, c("BTC_log_returns", "USDT_log_transactions")],
  order.by = DailyDataClean_MaxUSDTDaily$date
)

DailyDataMaxUSDT_zoo <- na.omit(DailyDataMaxUSDT_zoo)


## DLM_AR(5)
DLM_AR5_diff_maxUSDT <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(USDT_log_transactions, 0:5), data = DailyDataMaxUSDT_zoo)
summary(DLM_AR5_diff_maxUSDT) # R^2 = 0.1181, non significant estimates
AIC(DLM_AR5_diff_maxUSDT) # -226.759
BIC(DLM_AR5_diff_maxUSDT) # -204.8817


## DLM_AR(1)
DLM_AR1_diff_maxUSDT <- dynlm(BTC_log_returns ~ L(BTC_log_returns, 1) + L(USDT_log_transactions, 0:1), data = DailyDataMaxUSDT_zoo)
summary(DLM_AR1_diff_maxUSDT) # R^2 = 0.06594, non significant values 
AIC(DLM_AR1_diff_maxUSDT) # -228.732
BIC(DLM_AR1_diff_maxUSDT) # -228.3453


