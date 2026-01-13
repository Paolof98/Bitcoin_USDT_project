#### PART 2: TRANSACTION TYPES ####

# Relevant data
## 1) Tether (USDT) data cleaned
USDTDataClean <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Tether_Clean.csv")

names(USDTDataClean)
head(USDTDataClean)

## 2) Bitcoin data clean
BitcoinDataClean <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Bitcoin_Clean.csv")

names(BitcoinDataClean)
head(BitcoinDataClean)


## Bitcoin returns: log(BTC_close)-log(BTC_close(t-1))
BitcoinDataClean <- BitcoinDataClean %>% mutate(log_returns = log(btc_close) - lag(log(btc_close)))

# Volatility = abs(BTC returns)
BitcoinDataClean <- BitcoinDataClean %>% mutate(BTC_volatility = abs(log_returns))

# diff(log(volume))
BitcoinDataClean <- BitcoinDataClean %>% mutate(BTC_diff_log_volume = log(btc_volume) - lag(log(btc_volume)))

head(BitcoinDataClean)


write_xlsx(BitcoinDataClean, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/BTC_Data_Clean_Vars.xlsx")





# TYPE 1: Grant Property Tokens

names(BitcoinDataClean)
head(BitcoinDataClean)
summary(BitcoinDataClean$log_returns)
mean(BitcoinDataClean$log_returns, na.rm = TRUE)



## Minting events: the days when Grant Property Token transactions happened
Tether_GPT <- USDTDataClean %>% filter(tx_type == "Grant Property Tokens")



## Vector of the Mining events
Minting_Events <- tibble(date = as.Date(Tether_GPT$date, format = "%d/%m/%Y"))

BitcoinDataClean$date <- as.Date(BitcoinDataClean$date, format = "%d/%m/%Y")






# Table with events and BTC returns
## Join with BTC log returns
BTC_GPTDays <- Minting_Events %>% 
		   left_join(BitcoinDataClean, by = "date") %>% 
		   dplyr::select(date, log_returns) %>% 
		   rename(GPT_day = date, t = log_returns)


names(BTC_GPTDays)
head(BTC_GPTDays)


## Create tables for every day before and after (+-5)
Five_days_before <- Minting_Events - 5
Four_days_before <- Minting_Events - 4
Three_days_before <- Minting_Events - 3
Two_days_before <- Minting_Events - 2
One_day_before <- Minting_Events - 1
One_day_after <- Minting_Events + 1
Two_days_after <- Minting_Events + 2
Three_days_after <- Minting_Events + 3
Four_days_after <- Minting_Events + 4
Five_days_after <- Minting_Events + 5

## Join
### 5 days before
BTC_GPT_5DaysBefore <- Five_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-5" = log_returns)


### 4 days before
BTC_GPT_4DaysBefore <- Four_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-4" = log_returns)


### 3 days before
BTC_GPT_3DaysBefore <- Three_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-3" = log_returns)


### 2 days before
BTC_GPT_2DaysBefore <- Two_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-2" = log_returns)


### 1 day before
BTC_GPT_1DayBefore <- One_day_before %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t-1" = log_returns)


### 1 day after
BTC_GPT_1DayAfter <- One_day_after %>% 
		   	   left_join(BitcoinDataClean, by = "date") %>% 
		   	   dplyr::select(date, log_returns) %>% 
		   	   rename("t+1" = log_returns)


### 2 days after
BTC_GPT_2DaysAfter <- Two_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+2" = log_returns)


### 3 days after
BTC_GPT_3DaysAfter <- Three_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+3" = log_returns)

### 4 days after
BTC_GPT_4DaysAfter <- Four_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+4" = log_returns)


### 5 days after
BTC_GPT_5DaysAfter <- Five_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+5" = log_returns)



## Bind all of them
### Bind
GPT_Outcomes <- cbind(BTC_GPT_5DaysBefore,
			    BTC_GPT_4DaysBefore,
			    BTC_GPT_3DaysBefore,
			    BTC_GPT_2DaysBefore,
			    BTC_GPT_1DayBefore,
			    BTC_GPTDays,
			    BTC_GPT_1DayAfter,
			    BTC_GPT_2DaysAfter,
			    BTC_GPT_3DaysAfter,
			    BTC_GPT_4DaysAfter,
			    BTC_GPT_5DaysAfter)


### Remove the dates and order
GPT_Outcomes <- GPT_Outcomes %>% 
		    dplyr::select(GPT_day, "t-5", "t-4", "t-3", "t-2", "t-1", t, "t+1", "t+2", "t+3", "t+4", "t+5")




write_xlsx(GPT_Outcomes, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/GPT_Outcomes.xlsx")




# Table with Volume
## Join with BTC diff(log(volume))
BTC_GPTDays_vm <- Minting_Events %>% 
		   left_join(BitcoinDataClean, by = "date") %>% 
		   dplyr::select(date, BTC_diff_log_volume) %>% 
		   rename(GPT_day = date, t = BTC_diff_log_volume)


names(BTC_GPTDays_vm)
head(BTC_GPTDays_vm)


## Join
### 5 days before
BTC_GPT_5DaysBefore_vm <- Five_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-5" = BTC_diff_log_volume)


### 4 days before
BTC_GPT_4DaysBefore_vm <- Four_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-4" = BTC_diff_log_volume)


### 3 days before
BTC_GPT_3DaysBefore_vm <- Three_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-3" = BTC_diff_log_volume)


### 2 days before
BTC_GPT_2DaysBefore_vm <- Two_days_before %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, BTC_diff_log_volume) %>% 
		   	     rename("t-2" = BTC_diff_log_volume)


### 1 day before
BTC_GPT_1DayBefore_vm <- One_day_before %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t-1" = BTC_diff_log_volume)


### 1 day after
BTC_GPT_1DayAfter_vm <- One_day_after %>% 
		   	   left_join(BitcoinDataClean, by = "date") %>% 
		   	   dplyr::select(date, BTC_diff_log_volume) %>% 
		   	   rename("t+1" = BTC_diff_log_volume)


### 2 days after
BTC_GPT_2DaysAfter_vm <- Two_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+2" = BTC_diff_log_volume)


### 3 days after
BTC_GPT_3DaysAfter_vm <- Three_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+3" = BTC_diff_log_volume)

### 4 days after
BTC_GPT_4DaysAfter_vm <- Four_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+4" = BTC_diff_log_volume)


### 5 days after
BTC_GPT_5DaysAfter_vm <- Five_days_after %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, BTC_diff_log_volume) %>% 
		   	    rename("t+5" = BTC_diff_log_volume)



## Bind all of them
### Bind
GPT_Outcomes_vm <- cbind(BTC_GPT_5DaysBefore_vm,
			    BTC_GPT_4DaysBefore_vm,
			    BTC_GPT_3DaysBefore_vm,
			    BTC_GPT_2DaysBefore_vm,
			    BTC_GPT_1DayBefore_vm,
			    BTC_GPTDays_vm,
			    BTC_GPT_1DayAfter_vm,
			    BTC_GPT_2DaysAfter_vm,
			    BTC_GPT_3DaysAfter_vm,
			    BTC_GPT_4DaysAfter_vm,
			    BTC_GPT_5DaysAfter_vm)


### Remove the dates and order
GPT_Outcomes_vm <- GPT_Outcomes_vm %>% 
		    dplyr::select(GPT_day, "t-5", "t-4", "t-3", "t-2", "t-1", t, "t+1", "t+2", "t+3", "t+4", "t+5")


head(GPT_Outcomes_vm)


write_xlsx(GPT_Outcomes_vm, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/GPT_Outcomes_volume.xlsx")

mean(BitcoinDataClean$BTC_diff_log_volume, na.rm = TRUE) # -0.009162287









# IRF of minting event shock on BTC returns
# If GPT, 1, hence 0
names(USDTDataClean)

USDTDataClean <- USDTDataClean %>% mutate(is_GPT = ifelse(tx_type == "Grant Property Tokens", 1, 0))


# Group USDT data
USDTDataClean_Daily <- USDTDataClean %>% 
	dplyr::select(date, amount, fee, is_GPT) %>%
	dplyr::group_by(date) %>% 
	dplyr::summarise(total_daily_transactions = sum(amount), 
			     daily_fees = sum(fee),
			     is_GPT = sum(is_GPT))


# Join with BTC
BitcoinDataClean$date <- as.Date(USDTDataClean_Daily$date)

DailyData_Issuancebinary <- USDTDataClean_Daily %>% 
					left_join(BitcoinDataClean, by = "date")

head(DailyData_Issuancebinary)



## Data needed
BTC <- DailyData_Issuancebinary$log_returns
USDT_issuance <- DailyData_Issuancebinary$is_GPT


VAR_Issuance_Data <- as.data.frame(na.omit(cbind(BTC, USDT_issuance)))


# VAR
VARselect(VAR_Issuance_Data, lag.max = 5, type = "const")


fit_VAR_Issuance <- VAR(VAR_Issuance_Data, p = 1, type = "const")

summary(fit_VAR_Issuance)

fit_VAR_Issuance_2 <- VAR(VAR_Issuance_Data, p = 2, type = "const")

summary(fit_VAR_Issuance_2) # VAR(2) is weak model, p = 0.7373, its the other way around that has strong estimations


# IRF
## IRF with VAR(1)
IRF_Issuance <- irf(
  fit_VAR_Issuance,
  impulse  = "USDT_issuance",
  response = "BTC",
  n.ahead  = 10,
  boot     = TRUE
)

plot(IRF_Issuance)


## IRF with VAR(2)
IRF_Issuance_2 <- irf(
  fit_VAR_Issuance_2,
  impulse  = "USDT_issuance",
  response = "BTC",
  n.ahead  = 10,
  boot     = TRUE
)

plot(IRF_Issuance_2) # DON'T USE








# Calculate values in days when there is no GPT transactions

DailyData_Issuancebinary_noGPT <- DailyData_Issuancebinary %>% dplyr::filter(is_GPT == 0)

mean(DailyData_Issuancebinary_noGPT$BTC_diff_log_volume, na.rm = TRUE) # -0.01609015

mean(DailyData_Issuancebinary_noGPT$BTC_volatility, na.rm = TRUE) # 0.04836897


var(DailyData_Issuancebinary_noGPT$BTC_diff_log_volume, na.rm = TRUE) # 0.03895581
var(DailyData_Issuancebinary_noGPT$btc_volume, na.rm = TRUE) # 1.508119e+19
var(DailyData_Issuancebinary_noGPT$BTC_diff_log_volume, na.rm = TRUE)






# Remove Event window rows

## The dates are:
Minting_Events
Five_days_before <- Minting_Events - 5
Four_days_before <- Minting_Events - 4
Three_days_before <- Minting_Events - 3
Two_days_before <- Minting_Events - 2
One_day_before <- Minting_Events - 1
One_day_after <- Minting_Events + 1
Two_days_after <- Minting_Events + 2
Three_days_after <- Minting_Events + 3
Four_days_after <- Minting_Events + 4
Five_days_after <- Minting_Events + 5

## Bind
Event_Window <- rbind(Five_days_before,
			    Four_days_before,
			    Three_days_before,
			    Two_days_before,
			    One_day_before,
			    Minting_Events,
			    One_day_after,
			    Two_days_after,
			    Three_days_after,
			    Four_days_after,
			    Five_days_after)

## Unique values
Event_Window <- unique(Event_Window)

Event_Window <- dplyr::arrange(Event_Window) # doesn't order but checks out anyway

nrow(Event_Window) # 43 dates

## Now need to remove these dates from the data
DailyData_NoEventWindow <- DailyData_Issuancebinary %>% anti_join(Event_Window, by = "date")

head(DailyData_NoEventWindow)
head(DailyData_Issuancebinary)

nrow(DailyData_NoEventWindow)
nrow(DailyData_Issuancebinary)

write_xlsx(DailyData_NoEventWindow, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/DailyData_NoEventWindow.xlsx")

## Join to have Event Window data
DailyData_EventWindow <- Event_Window %>% left_join(DailyData_Issuancebinary, by = "date")

head(DailyData_EventWindow)
head(DailyData_Issuancebinary)

nrow(DailyData_EventWindow)
nrow(DailyData_Issuancebinary)


nrow(DailyData_NoEventWindow) + nrow(DailyData_EventWindow)



# Measures of central tendency and dispersion of no event window data
names(DailyData_NoEventWindow)


mean(DailyData_NoEventWindow$log_returns, na.rm = TRUE) # -0.009074
mean(DailyData_NoEventWindow$BTC_diff_log_volume, na.rm = TRUE) #  -0.005320802
mean(DailyData_NoEventWindow$BTC_volatility, na.rm = TRUE) # 0.05264858

sd(DailyData_NoEventWindow$log_returns, na.rm = TRUE) # 0.06354307
sd(DailyData_NoEventWindow$BTC_diff_log_volume, na.rm = TRUE) # 0.2147943
sd(DailyData_NoEventWindow$BTC_volatility, na.rm = TRUE) # 0.03594654











# TYPE 2: Simple Send
# large simple sends (e.g., exchange inflows) might impact BTC markets.
# Idea: Discretise Simple send transactions into caegories (Small, big transactions) and see what happens
# OR just filter for the large Simple Send transactions

## Filter by Simple Send
USDT_SimpleSend <- USDTDataClean %>% filter(tx_type == "Simple Send")

## Bins
USDT_SimpleSend <- USDT_SimpleSend %>%
	mutate(
    amount_bin = cut(
      amount,
      breaks = c(0, 100, 1000, 10000, 100000, 1000000, Inf),
      labels = c("0–100", "100–1k", "1k–10k", "10k-100k", "100k–1m", "1m+"),
      right = FALSE
    )
  )

amount_counts <- USDT_SimpleSend %>%
  group_by(amount_bin) %>%
  summarise(n = n())

## Distribution of Simple Send transactions by amount
ggplot(amount_counts, aes(x = amount_bin, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "",
    x = "Amount Range",
    y = "Number of Transactions"
  ) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 16),
	  axis.title.y = element_text(size = 16),
	  axis.text.x = element_text(size = 16), 
	  axis.text.y = element_text(size = 16)
	  )




## Distributions of Simple Send and GPT. Simple Send has much higher volatility, GPT has much higher values
ggplot(USDTDataClean, aes(x = amount, colour = tx_type)) +
  geom_density(linewidth = 1.2) +
  scale_x_log10() +
  labs(
    title = "Distribution of Tether Transaction Amounts by Type",
    x = "Amount (log scale)",
    y = "Density"
  ) +
  theme_minimal()






USDTDataClean <- USDTDataClean %>%
  mutate(
    amount_bin = cut(
      amount,
      breaks = c(0, 100, 1000, 10000, 1000000, Inf),
      labels = c("0–100", "100–1k", "1k–10k", "10k–1m", "1m+"),
      right = FALSE
    )
  )




USDTDataClean %>%
  count(tx_type, amount_bin) %>%
  ggplot(aes(x = amount_bin, y = n, fill = tx_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Transaction Amount Distribution by Type",
    x = "Amount Range",
    y = "Number of Transactions"
  ) +
  theme_minimal() 



ggplot(USDTDataClean, aes(x = tx_type, y = amount, fill = tx_type)) +
  geom_violin() +
  scale_y_log10() +
  labs(
    title = "Distribution of Amounts by Transaction Type",
    x = "Transaction Type",
    y = "Amount (log scale)"
  ) +
  theme_minimal()





# Established before that there isn't a statistically significant correlation with all USDT transactions and BTC close. However:
# Model with only Simple Send transactions that are above 10k (150k + transactions) or above 100k (35k+ transactions)

## Join Simple Send USDT with BTC data
names(BitcoinDataClean)
names(BTC_USDTSimpleSend_HighTransactions)
names(USDT_SimpleSend)
names(USDTSimpleSend_HighTransactions)
head(USDTSimpleSend_HighTransactions_DailySum)
names(BTC_USDTSimpleSend_HighTransactions)
head(BTC_USDTSimpleSend_HighTransactions)



### USDT: filter for Simple Send transactions over 100k
USDTSimpleSend_HighTransactions <- dplyr::filter(USDT_SimpleSend, amount >= 100000) %>% dplyr::select(date, amount)

plot(x = USDTSimpleSend_HighTransactions$date, y = USDTSimpleSend_HighTransactions$amount)

### Group by date
USDTSimpleSend_HighTransactions_DailySum <- USDTSimpleSend_HighTransactions %>% 
							  group_by(date) %>% 
							  dplyr::summarise(Total_Amount_LargeTransactions = sum(amount))


### Difference in log transactions variable
USDTSimpleSend_HighTransactions_DailySum <- USDTSimpleSend_HighTransactions_DailySum %>% 
							  mutate(log_transactions = log(Total_Amount_LargeTransactions) - lag(log(Total_Amount_LargeTransactions)))



### Join with BTC data
BTC_USDTSimpleSend_HighTransactions <- USDTSimpleSend_HighTransactions_DailySum %>% 
		   	    			   left_join(BitcoinDataClean, by = "date")

write_xlsx(BTC_USDTSimpleSend_HighTransactions, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/BTC_USDTSimpleSend_HighTransactions.xlsx")



## Granger causality tests
### diff(log(BTC returns)) and diff(log(USDT transactions)) 
grangertest(log_returns ~ log_transactions, order = 5, data = BTC_USDTSimpleSend_HighTransactions) # USDT -> BTC
grangertest(log_transactions ~ log_returns, order = 5, data = BTC_USDTSimpleSend_HighTransactions) # BTC -> USDT


### BTC close and USDT transactions 
grangertest(btc_close ~ Total_Amount_LargeTransactions, order = 5, data = BTC_USDTSimpleSend_HighTransactions) # USDT -> BTC
grangertest(Total_Amount_LargeTransactions ~ btc_close, order = 5, data = BTC_USDTSimpleSend_HighTransactions) # BTC -> USDT: p < 0.1

### diff(log(BTC returns)) and log(transactions)
grangertest(log_returns ~ log(Total_Amount_LargeTransactions), order = 5, data = BTC_USDTSimpleSend_HighTransactions) # USDT -> BTC
grangertest(log(Total_Amount_LargeTransactions) ~ log_returns, order = 5, data = BTC_USDTSimpleSend_HighTransactions) # BTC -> USDT







## What about just for the one million+ transactions?
### USDT: filter for Simple Send transactions over 1 million
USDTSimpleSend_MoreOneMTransactions <- dplyr::filter(USDT_SimpleSend, amount >= 1000000) %>% dplyr::select(date, amount)


plot(x = USDTSimpleSend_MoreOneMTransactions$date, y = USDTSimpleSend_MoreOneMTransactions$amount)


### Group by date
USDTSimpleSend_MTransactions_DailySum <- USDTSimpleSend_MoreOneMTransactions %>% 
							  group_by(date) %>% 
							  dplyr::summarise(Total_Amount_LargeTransactions = sum(amount))


### Difference in log transactions variable
USDTSimpleSend_MTransactions_DailySum <- USDTSimpleSend_MTransactions_DailySum %>% 
						     mutate(log_transactions = log(Total_Amount_LargeTransactions) - lag(log(Total_Amount_LargeTransactions)))



### Join with BTC data
BTC_USDTSimpleSend_MTransactions <- USDTSimpleSend_MTransactions_DailySum %>% 
		   	    			   left_join(BitcoinDataClean, by = "date")



## Granger causality tests 
### diff(log(BTC returns)) and diff(log(USDT transactions)) 
grangertest(log_returns ~ log_transactions, order = 5, data = BTC_USDTSimpleSend_MTransactions) # USDT -> BTC
grangertest(log_transactions ~ log_returns, order = 5, data = BTC_USDTSimpleSend_MTransactions) # BTC -> USDT

### BTC close and USDT transactions 
grangertest(btc_close ~ Total_Amount_LargeTransactions, order = 5, data = BTC_USDTSimpleSend_MTransactions) # USDT -> BTC
grangertest(Total_Amount_LargeTransactions ~ btc_close, order = 5, data = BTC_USDTSimpleSend_MTransactions) # BTC -> USDT

### diff(log(BTC returns)) and log(transactions)
grangertest(log_returns ~ log(Total_Amount_LargeTransactions), order = 5, data = BTC_USDTSimpleSend_MTransactions) # USDT -> BTC
grangertest(log(Total_Amount_LargeTransactions) ~ log_returns, order = 5, data = BTC_USDTSimpleSend_MTransactions) # BTC -> USDT





# Even when taking into account only the highest transactions (1 million +), no Granger causality between the two variables









# TYPE 3: Revoke Property Tokens
## There is only one RPT event, so could use this as an eventt and see what happened to BTC returns before and after


## Filter by Revoke Property Tokens
USDT_RPT <- USDTDataClean %>% filter(tx_type == "Revoke Property Tokens")

USDT_RPT # 31/01/2018


Revoke_Event <- tibble(date = as.Date(USDT_RPT$date))


# Table with events and returns
## Create tables for every day before and after (+-5)
Five_days_before_R <- Revoke_Event - 5
Four_days_before_R <- Revoke_Event - 4
Three_days_before_R <- Revoke_Event - 3
Two_days_before_R <- Revoke_Event - 2
One_day_before_R <- Revoke_Event - 1
One_day_after_R <- Revoke_Event + 1
Two_days_after_R <- Revoke_Event + 2
Three_days_after_R <- Revoke_Event + 3
Four_days_after_R <- Revoke_Event + 4
Five_days_after_R <- Revoke_Event + 5

# Join with BTC
BTC_RPTDay <- Revoke_Event %>% 
		   left_join(BitcoinDataClean, by = "date") %>% 
		   dplyr::select(date, log_returns) %>% 
		   rename(RPT_day = date, t = log_returns)

### 5 days before
BTC_RPT_5DaysBefore <- Five_days_before_R %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-5" = log_returns)


### 4 days before
BTC_RPT_4DaysBefore <- Four_days_before_R %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-4" = log_returns)


### 3 days before
BTC_RPT_3DaysBefore <- Three_days_before_R %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-3" = log_returns)


### 2 days before
BTC_RPT_2DaysBefore <- Two_days_before_R %>% 
		   	     left_join(BitcoinDataClean, by = "date") %>% 
		   	     dplyr::select(date, log_returns) %>% 
		   	     rename("t-2" = log_returns)


### 1 day before
BTC_RPT_1DayBefore <- One_day_before_R %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t-1" = log_returns)


### 1 day after
BTC_RPT_1DayAfter <- One_day_after_R %>% 
		   	   left_join(BitcoinDataClean, by = "date") %>% 
		   	   dplyr::select(date, log_returns) %>% 
		   	   rename("t+1" = log_returns)


### 2 days after
BTC_RPT_2DaysAfter <- Two_days_after_R %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+2" = log_returns)


### 3 days after
BTC_RPT_3DaysAfter <- Three_days_after_R %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+3" = log_returns)

### 4 days after
BTC_RPT_4DaysAfter <- Four_days_after_R %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+4" = log_returns)


### 5 days after
BTC_RPT_5DaysAfter <- Five_days_after_R %>% 
		   	    left_join(BitcoinDataClean, by = "date") %>% 
		   	    dplyr::select(date, log_returns) %>% 
		   	    rename("t+5" = log_returns)



## Bind all of them
### Bind
RPT_Outcomes <- cbind(BTC_RPT_5DaysBefore,
			    BTC_RPT_4DaysBefore,
			    BTC_RPT_3DaysBefore,
			    BTC_RPT_2DaysBefore,
			    BTC_RPT_1DayBefore,
			    BTC_RPTDay,
			    BTC_RPT_1DayAfter,
			    BTC_RPT_2DaysAfter,
			    BTC_RPT_3DaysAfter,
			    BTC_RPT_4DaysAfter,
			    BTC_RPT_5DaysAfter)


### Remove the dates and order
RPT_Outcomes <- RPT_Outcomes %>% 
		    dplyr::select(RPT_day, "t-5", "t-4", "t-3", "t-2", "t-1", t, "t+1", "t+2", "t+3", "t+4", "t+5")




write_xlsx(RPT_Outcomes, "C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/RPT_Outcomes.xlsx")




# Remove RPT and calculate mean returns

EventWindow_Revoke <- rbind(Five_days_before_R,
				    Four_days_before_R,
				    Three_days_before_R,
				    Two_days_before_R,
				    One_day_before_R,
				    Revoke_Event,
				    One_day_after_R,
				    Two_days_after_R,
				    Three_days_after_R,
				    Four_days_after_R,
				    Five_days_after_R)



## Unique values
EventWindow_Revoke <- unique(EventWindow_Revoke)


## Now need to remove these dates from the data
DailyData_NoEventWindow_R <- DailyData_Issuancebinary %>% anti_join(EventWindow_Revoke, by = "date")

print(DailyData_NoEventWindow_R, n = 40)



# Measures of central tendency and dispersion of no event window data
names(DailyData_NoEventWindow_R)

mean(DailyData_NoEventWindow_R$log_returns, na.rm = TRUE) # -0.002441559
mean(DailyData_NoEventWindow_R$BTC_diff_log_volume, na.rm = TRUE) # -0.01103645
mean(DailyData_NoEventWindow_R$BTC_volatility, na.rm = TRUE) # 0.04596828

sd(DailyData_NoEventWindow_R$log_returns, na.rm = TRUE) # 0.05841097
sd(DailyData_NoEventWindow_R$BTC_diff_log_volume, na.rm = TRUE) # 0.1964065
sd(DailyData_NoEventWindow_R$BTC_volatility, na.rm = TRUE) # 0.03574009

















# Volatility of variables


StdDev_BTC_Close <- sd(DailyDataClean$BTC_log_returns, na.rm = TRUE)
StdDev_BTC_Volume <- sd(DailyDataClean$BTC_log_volume, na.rm = TRUE)
StdDev_USDT_Transactions <- sd(DailyDataClean$USDT_log_transactions, na.rm = TRUE)

StdDev_BTC_Close 
StdDev_BTC_Volume
StdDev_USDT_Transactions


