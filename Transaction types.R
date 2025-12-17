#### PART 2: TRANSACTION TYPES ####

# Relevant data
## 1) Tether (USDT) data cleaned
USDTDataClean <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Tether_Clean.csv")

names(USDTDataClean)


## 2) Bitcoin data clean
BitcoinDataClean <- readr::read_csv("C:/Users/Paolo/Desktop/Fideres assignment/Post submission work/Bitcoin_Clean.csv")

names(BitcoinDataClean)
head(BitcoinDataClean)


## Bitcoin returns: log(BTC_close)-log(BTC_close(t-1))
BitcoinDataClean <- BitcoinDataClean %>% mutate(log_returns = log(btc_close) - lag(log(btc_close)))




# TYPE 1: Grant Property Tokens
names(BitcoinDataClean)
head(BitcoinDataClean)
summary(BitcoinDataClean$log_returns)
mean(BitcoinDataClean$log_returns, na.rm = TRUE)



## Minting events: the days when Grant Property Token transactions happened
Tether_GPT <- USDTDataClean %>% filter(tx_type == "Grant Property Tokens")


## Vector of the Mining events
Minting_Events <- tibble(date = as.Date(Tether_GPT$date))






## Join
BTC_GPTDays <- Minting_Events %>% 
		   left_join(BitcoinDataClean, by = "date") %>% 
		   dplyr::select(date, log_returns) %>% 
		   rename(GPT_day = date, t = log_returns)


names(BTC_GPTDays)
head(BTC_GPTDays)


# Table with events and returns
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



# Calculations
## Average Abnormal Return (AAR)





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


























# Volatility of variables

## Bitcoin: 
DailyDataClean <- DailyDataClean %>% mutate(BTC_log_returns = log(btc_close) - lag(log(btc_close))) # differenced
DailyDataClean <- DailyDataClean %>% mutate(BTC_log_volume = log(btc_volume) - lag(log(btc_volume))) # differenced


## Tether total daily transactions
DailyDataClean <- DailyDataClean %>% mutate(USDT_log_transactions = log(total_transactions) - lag(log(total_transactions))) # differenced
DailyDataClean <- DailyDataClean %>% rename(USDT_diff_log_transactions = USDT_log_transactions)

StdDev_BTC_Close <- sd(DailyDataClean$BTC_log_returns, na.rm = TRUE)
StdDev_BTC_Volume <- sd(DailyDataClean$BTC_log_volume, na.rm = TRUE)
StdDev_USDT_Transactions <- sd(DailyDataClean$USDT_log_transactions, na.rm = TRUE)

StdDev_BTC_Close 
StdDev_BTC_Volume
StdDev_USDT_Transactions


