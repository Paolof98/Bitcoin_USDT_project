#### PART 4: INTERESTING DAYS ####

# The interesting days I found are:
## 30 January
## 31 January: Already analysed because I looked at Revoke Property Tokens
## 22 February: Highest daily average of USDT transactions
## 18 March: 1KA grants money, the only transaction from this address
## 20 March: Highest USDT transaction (30mln USDT)

# Because I looked at 31 January, no need to look at 30 and 31 Jan now
# Look at what happened around 22 February
# Look at what happened around 18 March, because that's when 1KA makes its only transaction


# 22 February 2018
## USDT data from the 17th to the 27th of February
Data_17to27Feb <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/BTC and USDT 17 to 27 Feb.csv")

names(Data_17to27Feb)

## Bitcoin returns: log(BTC_close)-log(BTC_close(t-1))
Data_17to27Feb <- Data_17to27Feb %>% mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close)))

# Volatility = abs(BTC returns)
Data_17to27Feb <- Data_17to27Feb %>% mutate(BTC_volatility = abs(BTC_log_returns))

# diff(log(volume))
Data_17to27Feb <- Data_17to27Feb %>% mutate(BTC_diff_log_volume = log(btc_volume) - lag(log(btc_volume)))

head(Data_17to27Feb)

write_xlsx(Data_17to27Feb, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Data 17 to 27 Feb.xlsx")

# sum of day before and after is that there are negative returns, then they increase later.  After 3 to 4 days higher returns and volume, volatility noisier. Volatile volume


Data_No22Feb_EventWindow <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Data no 22 Feb event window.csv")

names(Data_No22Feb_EventWindow)

## Bitcoin returns: log(BTC_close)-log(BTC_close(t-1))
Data_No22Feb_EventWindow <- Data_No22Feb_EventWindow %>% mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close)))

# Volatility = abs(BTC returns)
Data_No22Feb_EventWindow <- Data_No22Feb_EventWindow %>% mutate(BTC_volatility = abs(BTC_log_returns))

# diff(log(volume))
Data_No22Feb_EventWindow <- Data_No22Feb_EventWindow %>% mutate(BTC_diff_log_volume = log(btc_volume) - lag(log(btc_volume)))


mean(Data_No22Feb_EventWindow$BTC_log_returns, na.rm = TRUE) # -0.008617247
mean(Data_No22Feb_EventWindow$BTC_diff_log_volume, na.rm = TRUE) # -0.0104544
mean(Data_No22Feb_EventWindow$BTC_volatility, na.rm = TRUE) # 0.04767388







# 18 March 2018
Data_No18Mar_EventWindow <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Data no 18 Mar event window.csv")

names(Data_No18Mar_EventWindow)

## Bitcoin returns: log(BTC_close)-log(BTC_close(t-1))
Data_No18Mar_EventWindow <- Data_No18Mar_EventWindow %>% mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close)))

# Volatility = abs(BTC returns)
Data_No18Mar_EventWindow <- Data_No18Mar_EventWindow %>% mutate(BTC_volatility = abs(BTC_log_returns))

# diff(log(volume))
Data_No18Mar_EventWindow <- Data_No18Mar_EventWindow %>% mutate(BTC_diff_log_volume = log(btc_volume) - lag(log(btc_volume)))


mean(Data_No18Mar_EventWindow$BTC_log_returns, na.rm = TRUE) # -0.008617247
mean(Data_No18Mar_EventWindow$BTC_diff_log_volume, na.rm = TRUE) # -0.0104544
mean(Data_No18Mar_EventWindow$BTC_volatility, na.rm = TRUE) # 0.05105032




# 18 January 2018
Data_No18Jan_EventWindow <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Data no 18 Jan event window.csv")

names(Data_No18Jan_EventWindow)

## Bitcoin returns: log(BTC_close)-log(BTC_close(t-1))
Data_No18Jan_EventWindow <- Data_No18Jan_EventWindow %>% mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close)))

# Volatility = abs(BTC returns)
Data_No18Jan_EventWindow <- Data_No18Jan_EventWindow %>% mutate(BTC_volatility = abs(BTC_log_returns))

# diff(log(volume))
Data_No18Jan_EventWindow <- Data_No18Jan_EventWindow %>% mutate(BTC_diff_log_volume = log(btc_volume) - lag(log(btc_volume)))


mean(Data_No18Jan_EventWindow$BTC_log_returns, na.rm = TRUE) # -0.008617247
mean(Data_No18Jan_EventWindow$BTC_diff_log_volume, na.rm = TRUE) # -0.0104544
mean(Data_No18Jan_EventWindow$BTC_volatility, na.rm = TRUE) # 0.0492311
mean(Data_No18Jan_EventWindow$btc_return, na.rm = TRUE) # -0.00406962 matches Excel

ifelse(mean(Data_No18Jan_EventWindow$BTC_log_returns, na.rm = TRUE) == mean(Data_No22Feb_EventWindow$BTC_log_returns, na.rm = TRUE), "TRUE", "FALSE") # TRUE





