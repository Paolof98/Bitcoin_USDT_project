#### PART 3: ADRESSES ####

# 3Mb... has already been analysedin the Grant Property Tokens section
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
ExtremeDays <- BTC_Net1KY_Data %>%
  mutate(extreme =
    case_when(
      BTC_Net1KY_Data$oneky_nettransactions <= q_low  ~ "Extreme Outflow",
      BTC_Net1KY_Data$oneky_nettransactions >= q_high ~ "Extreme Inflow",
      TRUE ~ "Normal"
    )
  )


head(ExtremeDays)
ExtremeDays$date <- as.Date(ExtremeDays$date, format = "%d/%m/%Y")


head(ExtremeDays_filtered)



## Filter
ExtremeDays_filtered <- ExtremeDays %>% dplyr::filter(extreme != "Normal")

head(ExtremeDays_filtered)


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




write_xlsx(Extreme1KY_Outcomes, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Extreme1KY_Outcomes.xlsx") # not getting a full table






# BTC Volatility
## Wilcox test: volatility
wilcox.test(BTC_volatility ~ extreme, data = DailyDataClean)

## Regeression
lm(abs(BTC_log_returns) ~ ExtremeDummy, data = DailyDataClean)





# BTC volume
## Differencing
acf2(BTC_Net1KY_Data$btc_volume)
acf2(diff(BTC_Net1KY_Data$btc_volume))




