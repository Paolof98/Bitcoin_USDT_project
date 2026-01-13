#### PART 3: ADRESSES ####

# 3Mb... has already been analysed in the Grant Property Tokens section
# Look into 1KY...

# Bitcoin data with net 1KY... transactions daily
BTC_Net1KY_Data <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Bitcoin_and_netKY_transactions.csv")

# BTC returns
BTC_Net1KY_Data <- BTC_Net1KY_Data %>% mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close)))

# Volatility = abs(BTC returns)
BTC_Net1KY_Data <- BTC_Net1KY_Data %>% mutate(BTC_volatility = abs(BTC_log_returns))


# diff(log(volume))
BTC_Net1KY_Data <- BTC_Net1KY_Data %>% mutate(BTC_diff_log_volume = log(btc_volume) - lag(log(btc_volume)))



names(BTC_Net1KY_Data)
head(BTC_Net1KY_Data)



# Is it white noise?
acf2(BTC_Net1KY_Data$oneky_nettransactions) # looks like stationary data
Box.test(BTC_Net1KY_Data$oneky_nettransactions, lag = 20, type = "Ljung") # p>0.05, fail to reject null hypothesis that residuals are indipendently distributed (no significant autocorrelation), similar to white noise




# Analysis of extreme net flow days, 1KY
## Define extreme net flows of 1KY both ways
### 95% and 5% to get lowest and highest
q_low  <- quantile(BTC_Net1KY_Data$oneky_nettransactions, 0.05, na.rm = TRUE)
q_high <- quantile(BTC_Net1KY_Data$oneky_nettransactions, 0.95, na.rm = TRUE)


### Define variable
BTC_Net1KY_Data <- BTC_Net1KY_Data %>%
  mutate(extreme =
    case_when(
      BTC_Net1KY_Data$oneky_nettransactions <= q_low  ~ "Extreme Outflow",
      BTC_Net1KY_Data$oneky_nettransactions >= q_high ~ "Extreme Inflow",
      TRUE ~ "Normal"
    )
  )





head(BTC_Net1KY_Data)
BTC_Net1KY_Data$date <- as.Date(BTC_Net1KY_Data$date, format = "%d/%m/%Y")


ExtremeDays_filtered <- as.Date(ExtremeDays_filtered$date, format = "%d/%m/%Y")



## Filter
### Filter for extreme days
ExtremeDays_filtered <- BTC_Net1KY_Data %>% dplyr::filter(extreme != "Normal")

head(ExtremeDays_filtered)

write_xlsx(ExtremeDays_filtered, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/ExtremeDays1KY_filtered.xlsx")


### Filter for normal days
NormalDays_filtered <- BTC_Net1KY_Data %>% dplyr::filter(extreme == "Normal")

head(NormalDays_filtered)




## As date given that need to do operations with them
ExtremeDays_filtered$date <- as.Date(ExtremeDays_filtered$date, format = "%d/%m/%Y")

head(ExtremeDays_filtered)


# Event study with +-5 days on BTC returns
## Events dates
Extreme_1KY_dates <- tibble(date = as.Date(ExtremeDays_filtered$date, format = "%d/%m/%Y"))

head(Extreme_1KY_dates)

## +-5 days
Five_days_before_1KY <- Extreme_1KY_dates - 5
Four_days_before_1KY <- Extreme_1KY_dates - 4
Three_days_before_1KY <- Extreme_1KY_dates - 3
Two_days_before_1KY <- Extreme_1KY_dates - 2
One_day_before_1KY <- Extreme_1KY_dates - 1
One_day_after_1KY <- Extreme_1KY_dates + 1
Two_days_after_1KY <- Extreme_1KY_dates + 2
Three_days_after_1KY <- Extreme_1KY_dates + 3
Four_days_after_1KY <- Extreme_1KY_dates + 4
Five_days_after_1KY <- Extreme_1KY_dates + 5



## Join with BTC
### Event day
BTC_ExtremeDay <- ExtremeDays_filtered %>% 
		   dplyr::select(date, BTC_log_returns) %>% 
		   rename(Extreme_day = date, t = BTC_log_returns)

### 5 days before
BTC_ED_5DaysBefore <- Five_days_before_1KY %>% 
		   	     left_join(ExtremeDays, by = "date") %>% 
		   	     dplyr::select(date, BTC_log_returns) %>% 
		   	     rename("t-5" = BTC_log_returns)


### 4 days before
BTC_ED_4DaysBefore <- Four_days_before_1KY %>% 
		   	     left_join(ExtremeDays, by = "date") %>% 
		   	     dplyr::select(date, BTC_log_returns) %>% 
		   	     rename("t-4" = BTC_log_returns)


### 3 days before
BTC_ED_3DaysBefore <- Three_days_before_1KY %>% 
		   	     left_join(ExtremeDays, by = "date") %>% 
		   	     dplyr::select(date, BTC_log_returns) %>% 
		   	     rename("t-3" = BTC_log_returns)


### 2 days before
BTC_ED_2DaysBefore <- Two_days_before_1KY %>% 
		   	     left_join(ExtremeDays, by = "date") %>% 
		   	     dplyr::select(date, BTC_log_returns) %>% 
		   	     rename("t-2" = BTC_log_returns)


### 1 day before
BTC_ED_1DayBefore <- One_day_before_1KY %>% 
		   	    left_join(ExtremeDays, by = "date") %>% 
		   	    dplyr::select(date, BTC_log_returns) %>% 
		   	    rename("t-1" = BTC_log_returns)


### 1 day after
BTC_ED_1DayAfter <- One_day_after_1KY %>% 
		   	   left_join(ExtremeDays, by = "date") %>% 
		   	   dplyr::select(date, BTC_log_returns) %>% 
		   	   rename("t+1" = BTC_log_returns)


### 2 days after
BTC_ED_2DaysAfter <- Two_days_after_1KY %>% 
		   	    left_join(ExtremeDays, by = "date") %>% 
		   	    dplyr::select(date, BTC_log_returns) %>% 
		   	    rename("t+2" = BTC_log_returns)


### 3 days after
BTC_ED_3DaysAfter <- Three_days_after_1KY %>% 
		   	    left_join(ExtremeDays, by = "date") %>% 
		   	    dplyr::select(date, BTC_log_returns) %>% 
		   	    rename("t+3" = BTC_log_returns)

### 4 days after
BTC_ED_4DaysAfter <- Four_days_after_1KY %>% 
		   	    left_join(ExtremeDays, by = "date") %>% 
		   	    dplyr::select(date, BTC_log_returns) %>% 
		   	    rename("t+4" = BTC_log_returns)


### 5 days after
BTC_ED_5DaysAfter <- Five_days_after_1KY %>% 
		   	    left_join(ExtremeDays, by = "date") %>% 
		   	    dplyr::select(date, BTC_log_returns) %>% 
		   	    rename("t+5" = BTC_log_returns)





## Bind all of them
### Bind
Extreme1KY_Outcomes <- cbind(BTC_ED_5DaysBefore,
			    BTC_ED_4DaysBefore,
			    BTC_ED_3DaysBefore,
			    BTC_ED_2DaysBefore,
			    BTC_ED_1DayBefore,
			    BTC_ExtremeDay,
			    BTC_ED_1DayAfter,
			    BTC_ED_2DaysAfter,
			    BTC_ED_3DaysAfter,
			    BTC_ED_4DaysAfter,
			    BTC_ED_5DaysAfter)


### Remove the dates and order
Extreme1KY_Outcomes <- Extreme1KY_Outcomes %>% 
		    dplyr::select(Extreme_day, "t-5", "t-4", "t-3", "t-2", "t-1", t, "t+1", "t+2", "t+3", "t+4", "t+5")




write_xlsx(Extreme1KY_Outcomes, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Extreme1KY_Outcomes.xlsx") 


BTC_Net1KY_Data_zoo <- zoo(
  BTC_Net1KY_Data[, c("BTC_volatility", "Extreme_Dummy", "BTC_diff_log_volume")],
  order.by = BTC_Net1KY_Data$date
)




# BTC Volatility, 1KY
names(BTC_Net1KY_Data)
head(BTC_Net1KY_Data)
names(ExtremeDays_filtered)

## Measure expected value given extreme vs expected value given normal day
### Average volatility on extreme days
mean(ExtremeDays_filtered$BTC_volatility, na.rm = TRUE) # 0.0408, BTC close price is slightly less volatile on days when there are extreme 1KY transactions

### Average volatility on normal days
mean(NormalDays_filtered$BTC_volatility, na.rm = TRUE) # 0.0489


## Regression with dummy variable
### Define binary variable
BTC_Net1KY_Data <- BTC_Net1KY_Data %>%
  mutate(Extreme_Dummy = ifelse(extreme == "Normal", 0, 1))


### Regeression
OLS_Volatility <- lm(BTC_volatility ~ Extreme_Dummy, data = BTC_Net1KY_Data)
summary(OLS_Volatility) # not statistically significant model

### Regression with lagged effects
OLS_Volatility_l <- dynlm(BTC_volatility ~ L(Extreme_Dummy, 0:4), data = BTC_Net1KY_Data_zoo)
summary(OLS_Volatility_l) # I found that model with lag(4) is good
AIC(OLS_Volatility_l)
BIC(OLS_Volatility_l)

Box.test(residuals(OLS_Volatility_l), lag = 10, type = "Ljung")


## Wilcox test: volatility
wilcox.test(BTC_volatility ~ Extreme_Dummy, data = BTC_Net1KY_Data)







# BTC volume, 1KY
## Differencing
acf2(BTC_Net1KY_Data$btc_volume)
acf2(diff(BTC_Net1KY_Data$btc_volume))


names(BTC_Net1KY_Data)


## Measure expected value given extreme vs expected value given normal day
### Average change in volume on extreme days
mean(ExtremeDays_filtered$BTC_diff_log_volume, na.rm = TRUE) # 0.06497009. On days when there are extreme inflows/ouflows with 1KY, volume changes by 6.5%

### Average change in volume on normal days
mean(NormalDays_filtered$BTC_diff_log_volume, na.rm = TRUE) # -0.019. On days when there aren't extreme inflows/ouflows with 1KY, volume changes by -1.9%

mean(BTC_Net1KY_Data$BTC_log_returns, na.rm = TRUE) # -0.007552194
mean(BTC_Net1KY_Data$BTC_diff_log_volume, na.rm = TRUE) # -0.009162287

## Regression
### Regression same day
OLS_Volume <- lm(BTC_diff_log_volume ~ Extreme_Dummy, data = BTC_Net1KY_Data)
summary(OLS_Volume) # high p = 0.2217, R^2 = 0.01711, not significant estimates

### Regression with lagged effects
OLS_Volume_l <- dynlm(BTC_diff_log_volume ~ L(Extreme_Dummy, 0:1), data = BTC_Net1KY_Data_zoo)
summary(OLS_Volume_l) # beta 3 is significant. Best models: L(0:3) or L(1:3)
AIC(OLS_Volume_l)
BIC(OLS_Volume_l)


## Wilcox test: volatility
wilcox.test(BTC_diff_log_volume ~ Extreme_Dummy, data = BTC_Net1KY_Data)







# Event study BTC volume, 1KY
## Join with BTC
### Event day
BTC_V_ExtremeDay <- ExtremeDays_filtered %>% 
		   dplyr::select(date, BTC_diff_log_volume) %>% 
		   rename(Extreme_day = date, t = BTC_diff_log_volume)

### 5 days before
BTC_V_ED_5DaysBefore <- Five_days_before_1KY %>% 
		   	     left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-5" = BTC_diff_log_volume)


### 4 days before
BTC_V_ED_4DaysBefore <- Four_days_before_1KY %>% 
		   	     left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-4" = BTC_diff_log_volume)


### 3 days before
BTC_V_ED_3DaysBefore <- Three_days_before_1KY %>% 
		   	     left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-3" = BTC_diff_log_volume)


### 2 days before
BTC_V_ED_2DaysBefore <- Two_days_before_1KY %>% 
		   	     left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-2" = BTC_diff_log_volume)


### 1 day before
BTC_V_ED_1DayBefore <- One_day_before_1KY %>% 
		   	    left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t-1" = BTC_diff_log_volume)


### 1 day after
BTC_V_ED_1DayAfter <- One_day_after_1KY %>% 
		   	   left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	   dplyr::select(date, BTC_diff_log_volume) %>% 
		   	   rename("t+1" = BTC_diff_log_volume)


### 2 days after
BTC_V_ED_2DaysAfter <- Two_days_after_1KY %>% 
		   	    left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+2" = BTC_diff_log_volume)


### 3 days after
BTC_V_ED_3DaysAfter <- Three_days_after_1KY %>% 
		   	    left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+3" = BTC_diff_log_volume)

### 4 days after
BTC_V_ED_4DaysAfter <- Four_days_after_1KY %>% 
		   	    left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+4" = BTC_diff_log_volume)


### 5 days after
BTC_V_ED_5DaysAfter <- Five_days_after_1KY %>% 
		   	    left_join(BTC_Net1KY_Data, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+5" = BTC_diff_log_volume)





## Bind all of them
### Bind
Extreme1KY_Outcomes_V <- cbind(BTC_V_ED_5DaysBefore,
			    BTC_V_ED_4DaysBefore,
			    BTC_V_ED_3DaysBefore,
			    BTC_V_ED_2DaysBefore,
			    BTC_V_ED_1DayBefore,
			    BTC_V_ExtremeDay,
			    BTC_V_ED_1DayAfter,
			    BTC_V_ED_2DaysAfter,
			    BTC_V_ED_3DaysAfter,
			    BTC_V_ED_4DaysAfter,
			    BTC_V_ED_5DaysAfter)


### Remove the dates and order
Extreme1KY_Outcomes_V <- Extreme1KY_Outcomes_V %>% 
		    dplyr::select(Extreme_day, "t-5", "t-4", "t-3", "t-2", "t-1", t, "t+1", "t+2", "t+3", "t+4", "t+5")




write_xlsx(Extreme1KY_Outcomes_V, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Extreme1KY_Outcomes_Volume.xlsx") 






# Remove Event window rows

## The dates are:
Extreme_1KY_dates
Five_days_before_1KY <- Extreme_1KY_dates - 5
Four_days_before_1KY <- Extreme_1KY_dates - 4
Three_days_before_1KY <- Extreme_1KY_dates - 3
Two_days_before_1KY <- Extreme_1KY_dates - 2
One_day_before_1KY <- Extreme_1KY_dates - 1
One_day_after_1KY <- Extreme_1KY_dates + 1
Two_days_after_1KY <- Extreme_1KY_dates + 2
Three_days_after_1KY <- Extreme_1KY_dates + 3
Four_days_after_1KY <- Extreme_1KY_dates + 4
Five_days_after_1KY <- Extreme_1KY_dates + 5

## Bind
Event_Window_1KYED <- rbind(Five_days_before_1KY,
			    Four_days_before_1KY,
			    Three_days_before_1KY,
			    Two_days_before_1KY,
			    One_day_before_1KY,
			    Extreme_1KY_dates,
			    One_day_after_1KY,
			    Two_days_after_1KY,
			    Three_days_after_1KY,
			    Four_days_after_1KY,
			    Five_days_after_1KY)

## Unique values
Event_Window_1KYED <- unique(Event_Window_1KYED)

Event_Window_1KYED <- dplyr::arrange(Event_Window_1KYED) # doesn't order but checks out anyway

nrow(Event_Window_1KYED) # 78 dates

## Now need to remove these dates from the data
DailyData_NoEventWindow_1KYED <- DailyData_Issuancebinary %>% anti_join(Event_Window_1KYED, by = "date")

head(DailyData_NoEventWindow_1KYED)
head(DailyData_Issuancebinary)

nrow(DailyData_NoEventWindow_1KYED)
nrow(DailyData_Issuancebinary)

write_xlsx(DailyData_NoEventWindow_1KYED, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/DailyData_NoEventWindow.xlsx")

## Join to have Event Window data
DailyData_EventWindow_1KYED <- Event_Window_1KYED %>% left_join(DailyData_Issuancebinary, by = "date")

head(DailyData_EventWindow_1KYED)
head(DailyData_Issuancebinary)

nrow(DailyData_EventWindow_1KYED)
nrow(DailyData_Issuancebinary)


nrow(DailyData_NoEventWindow_1KYED) + nrow(DailyData_EventWindow_1KYED)



# Measures of central tendency and dispersion of no event window data
names(DailyData_NoEventWindow_1KYED)


mean(DailyData_NoEventWindow_1KYED$log_returns, na.rm = TRUE) # 0.003628276
mean(DailyData_NoEventWindow_1KYED$BTC_diff_log_volume, na.rm = TRUE) # -0.06848353
mean(DailyData_NoEventWindow_1KYED$BTC_volatility, na.rm = TRUE) # 0.04165487

sd(DailyData_NoEventWindow_1KYED$log_returns, na.rm = TRUE) # 0.05479767
sd(DailyData_NoEventWindow_1KYED$BTC_diff_log_volume, na.rm = TRUE) # 0.1737224
sd(DailyData_NoEventWindow_1KYED$BTC_volatility, na.rm = TRUE) # 0.0340268








# IRF for extreme 1KY net flow days?
## Data
BTC_E <- BTC_Net1KY_Data$BTC_log_returns
Extreme1KY_Dummy <- BTC_Net1KY_Data$Extreme_Dummy

Var_Extreme1KY <- as.data.frame(na.omit(cbind(BTC_E, Extreme1KY_Dummy)))

## VAR
VARselect(Var_Extreme1KY, lag.max = 5, type = "const") # 1

fit_VAR_E1KY <- VAR(Var_Extreme1KY, p = 1, type = "const")

summary(fit_VAR_E1KY) # p = 0.4041


## IRF
IRF_E1KY <- irf(
  fit_VAR_E1KY,
  impulse  = "Extreme1KY_Dummy",
  response = "BTC_E",
  n.ahead  = 10,
  boot     = TRUE
)

plot(IRF_E1KY)













# Top 5% of transactions: are they all from 1KY?
## Define extreme USDT transactions
### 95% to get highest
q_high_general <- quantile(USDTDataClean$amount, 0.95, na.rm = TRUE)


### Define variable
USDTDataClean <- USDTDataClean %>%
  mutate(extreme =
    ifelse(
      USDTDataClean$amount >= q_high_general, "High transaction", "Generic"
  ))

USDTDataClean <- USDTDataClean %>% arrange(date)

head(USDTDataClean)

# Top 5% table
USDT_Top5pc <- USDTDataClean %>% filter(extreme  == "High transaction")

write_xlsx(USDT_Top5pc, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/USDTClean with high transactions identification.xlsx") 
# Top 5% are not all from 1KY or 1NT. On 22/02 there is another interesting high Simple Send transaction. Lets look at particular days









