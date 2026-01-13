---------------------------------------------------- Fideres Assignment: relationship between Bitcoin prices and Tether transactions, post submission ---------------------------------------------------------------
--- PART 1: Data cleaning and making some tables
--- PART 2: Aggregate USDT data to daily
--- PART 3: Join to BTC data
--- PART 4: Create a table for the transaction types
--- PART 5: Sending and receiving adresses
--- PART 6: Addresses of interest



--- PART 1: Data cleaning and making some tables
--- Tether data clean
CREATE TABLE tether_clean AS
SELECT 
	TO_CHAR(block_time, 'YYYY-MM-DD') AS date,
	sending_address,
	reference_address,
	tx_type,
	amount,
	fee
FROM tether_q1

SELECT * FROM tether_clean ORDER BY date, amount


--- BTC data clean
CREATE TABLE btc_clean AS
SELECT 
	TO_CHAR("Date", 'YYYY-MM-DD') AS date,
	"Open" AS btc_open,
	"High" AS btc_high,
	"Low" AS btc_low,
	"Close" AS btc_close,
	"Volume" AS btc_volume,
	"Market Cap" AS btc_marketcap,
	"EOD_Return" AS btc_return
FROM eod_btc_data

SELECT * FROM btc_clean


--- Grant Property Tokens with addresses
CREATE TABLE tether_clean_GPT AS
SELECT
	date,
	sending_address,
	reference_address,
	amount
FROM tether_clean
WHERE tx_type = 'Grant Property Tokens'

SELECT * FROM tether_clean_GPT


SELECT
	date,
	sending_address,
	reference_address,
	amount
FROM tether_clean
WHERE NOT tx_type = 'Grant Property Tokens'
ORDER BY date





--- Revoke Property Tokens with addresses
CREATE TABLE tether_clean_RPT AS
SELECT
	date,
	sending_address,
	reference_address,
	amount
FROM tether_clean
WHERE tx_type = 'Revoke Property Tokens'

SELECT * FROM tether_clean_RPT







--- Each day the max USDT transaction, join to BTC and see relationship
SELECT 
	date,
	sending_address,
	reference_address,
	tx_type,
	MAX(amount) OVER (PARTITION BY tc.date) AS max_transaction,
	fee
FROM tether_clean AS tc --- review



CREATE TABLE max_usdt_transactions_daily AS
SELECT *
FROM (
    SELECT
        date,
		sending_address,
		reference_address,
		tx_type,
		amount AS max_transaction,
		fee,
        ROW_NUMBER() OVER (PARTITION BY date ORDER BY amount DESC) AS rn
    FROM tether_clean
) t
WHERE rn = 1; --- this works








SELECT * FROM tether_clean




--- How many transactions for each amount
SELECT 
    SUM(CASE WHEN amount < 100 THEN 1 ELSE 0 END) AS transactions_less_than_onehundred,
    SUM(CASE WHEN amount >= 100 AND amount < 1000 THEN 1 ELSE 0 END) AS tr_bet_hundred_and_thousand,
    SUM(CASE WHEN amount >= 1000 AND amount < 10000 THEN 1 ELSE 0 END) AS thousand_tenk,
    SUM(CASE WHEN amount >= 10000 AND amount < 100000 THEN 1 ELSE 0 END) AS tenk_hundredk,
    SUM(CASE WHEN amount >= 100000 AND amount < 1000000 THEN 1 ELSE 0 END) AS hundredk_onem,
    SUM(CASE WHEN amount >= 1000000 THEN 1 ELSE 0 END) AS more_than_onem
FROM tether_clean




--- The transaction with the highest value everyday
CREATE TABLE usdt_highest_transactions_everyday AS
SELECT DISTINCT ON (date)
    date,
    sending_address,
    reference_address,
    amount,
    amount * 100 / SUM(amount) OVER (PARTITION BY date) AS percentage_of_day_amount,
    tx_type,
    fee
FROM tether_clean
ORDER BY date, amount DESC;


SELECT * FROM highest_transactions_everyday

--- Now, check if there are trends on the addresses that appear the most on the highest_transactions_everyday table
SELECT 
	sending_address, 
	COUNT(sending_address) AS days_sendingaddress_washighest,
	SUM(amount) AS total_amount
FROM highest_transactions_everyday
GROUP BY sending_address
ORDER BY COUNT(sending_address) DESC


SELECT 
	reference_address, 
	COUNT(reference_address) AS days_referenceaddress_washighest,
	SUM(amount) AS total_amount
FROM highest_transactions_everyday
GROUP BY reference_address
ORDER BY COUNT(reference_address) DESC







--- Join max USDT transactions to BTC
CREATE TABLE btc_usdt_highesttransactions_everyday AS
SELECT 
	uhte.*, 
	bc.btc_open,
	bc.btc_high,
	bc.btc_low,
	bc.btc_close,
	bc.btc_volume,
	bc.btc_marketcap,
	bc.btc_return
FROM usdt_highest_transactions_everyday AS uhte
LEFT JOIN btc_clean AS bc
ON uhte.date = bc.date


SELECT * FROM btc_usdt_highesttransactions_everyday





--- Daily USDT total transactions
CREATE TABLE daily_usdt_clean AS
	SELECT 
		date,
		SUM(amount) AS total_transactions, --- aggregate transactions
		AVG(amount) AS average_transaction_size,
		SUM(fee) AS total_fee_per_transaction,
		AVG(fee) AS avg_fee_per_transaction
	FROM tether_clean
	GROUP BY date
	
	
	

--- Join clean data
CREATE TABLE daily_btc_usdt_clean AS
	SELECT 
		b.*, 
		t.total_transactions,
		t.average_transaction_size,
		t.total_fee_per_transaction,
		t.avg_fee_per_transaction
	FROM btc_clean AS b
	LEFT JOIN daily_usdt_clean AS t
		ON b.date = t.date

SELECT * FROM daily_btc_usdt_clean

		
		
		
		
		

--- PART 2: Aggregate USDT dat to daily
CREATE TABLE daily_usdt AS
	SELECT 
		TO_CHAR(block_time, 'YYYY-MM-DD') AS date, --- convert date format
		SUM(amount) AS total_transactions, --- aggregate transactions
		AVG(amount) AS average_transaction_size,
		SUM(fee) AS total_fee_per_transaction,
		AVG(fee) AS avg_fee_per_transaction
	FROM tether_q1
	GROUP BY block_time
	

SELECT * FROM daily_usdt --- date, total and average transactions, total and average fees




--- PART 3: Join to BTC data
CREATE TABLE daily_btc_usdt AS
	SELECT * FROM eod_btc_data AS ebd 
	LEFT JOIN daily_usdt AS du
		ON ebd."Date" = CAST(du.date AS DATE)


SELECT * FROM daily_btc_usdt




--- What are the days with the highest average transactions?
SELECT date, average_transaction_size,  total_transactions, daily_btc_usdt."Close"
FROM daily_btc_usdt
ORDER BY average_transaction_size DESC
LIMIT 20






--- PART 4: Table for the transaction types
------ USDT data aggregated by transaction type
CREATE TABLE usdt_by_trtype AS
	SELECT 
		tx_type,
		SUM(amount) AS total_transactions,
		SUM(fee) AS total_fees,
		AVG(fee) AS average_fees
	FROM tether_q1 tq 
	GROUP BY tx_type
	
SELECT * FROM usdt_by_trtype

	
------ USDT data filtered by Simple Send
CREATE TABLE usdt_simple_send_agg AS
WITH usdt_simple_send AS (
	SELECT 
		TO_CHAR(block_time, 'YYYY-MM-DD') AS date,
		sending_address,
		reference_address,
		tx_type,
		amount,
		fee
	FROM tether_q1 tq 
	WHERE tx_type = 'Simple Send'
)
	SELECT 
		date, 
		tx_type,
		SUM(amount) AS total_transactions, --- aggregate transactions
		SUM(fee) AS total_fee_per_transaction,
		AVG(fee) AS avg_fee_per_transaction
	FROM usdt_simple_send 
	GROUP BY tx_type, date

SELECT * FROM usdt_simple_send_agg





------ USDT data filtered by Grant Property Tokens
CREATE TABLE usdt_gpt_agg AS
WITH usdt_gpt AS (
	SELECT 
		TO_CHAR(block_time, 'YYYY-MM-DD') AS date,
		sending_address,
		reference_address,
		tx_type,
		amount,
		fee
	FROM tether_q1 tq 
	WHERE tx_type = 'Grant Property Tokens'
)
	SELECT 
		date, 
		tx_type,
		SUM(amount) AS total_transactions, --- aggregate transactions
		SUM(fee) AS total_fee_per_transaction,
		AVG(fee) AS avg_fee_per_transaction
	FROM usdt_gpt 
	GROUP BY tx_type, date

SELECT * FROM usdt_gpt_agg



------ USDT data filtered by Revoke Property Tokens
CREATE TABLE usdt_rpt_agg AS
WITH usdt_rpt AS (
	SELECT 
		TO_CHAR(block_time, 'YYYY-MM-DD') AS date,
		sending_address,
		reference_address,
		tx_type,
		amount,
		fee
	FROM tether_q1 tq 
	WHERE tx_type = 'Revoke Property Tokens'
)
	SELECT 
		date, 
		tx_type,
		SUM(amount) AS total_transactions, --- aggregate transactions
		SUM(fee) AS total_fee_per_transaction,
		AVG(fee) AS avg_fee_per_transaction
	FROM usdt_rpt 
	GROUP BY tx_type, date

SELECT * FROM usdt_rpt_agg




--- PART 5: Sending and receiving adresses

------ Grouped by sending address
CREATE TABLE daily_usdt_saddresses AS
	SELECT 
		TO_CHAR(block_time, 'YYYY-MM-DD') AS date, --- convert date format
		sending_address,
		SUM(amount) AS total_transactions, --- aggregate transactions
		SUM(fee) AS total_fee_per_transaction,
		AVG(fee) AS avg_fee_per_transaction
	FROM tether_q1
	GROUP BY block_time, sending_address
	

SELECT * FROM daily_usdt_saddresses --- date, total transactions, total and average fees, grouped by sending address


------ Grouped by reference address
CREATE TABLE daily_usdt_raddresses AS
	SELECT 
		TO_CHAR(block_time, 'YYYY-MM-DD') AS date, --- convert date format
		reference_address,
		SUM(amount) AS total_transactions, --- aggregate transactions
		SUM(fee) AS total_fee_per_transaction,
		AVG(fee) AS avg_fee_per_transaction
	FROM tether_q1
	GROUP BY date, reference_address
	

SELECT * FROM daily_usdt_raddresses --- date, total transactions, total and average fees, grouped by reference address






------ Grouped by sending address, aggregated no dates
CREATE TABLE usdt_saddresses AS
	SELECT 
		sending_address,
		SUM(amount) AS total_transactions, --- aggregate transactions
		SUM(fee) AS transaction_fee
	FROM tether_q1
	GROUP BY sending_address
	

SELECT * FROM usdt_saddresses --- total transactions, total and average fees, grouped by sending address
ORDER BY total_transactions DESC

------ Grouped by reference address, aggregated no dates
CREATE TABLE usdt_raddresses AS
	SELECT 
		reference_address,
		SUM(amount) AS total_transactions, --- aggregate transactions
		SUM(fee) AS transaction_fee
	FROM tether_q1
	GROUP BY reference_address
	

SELECT * FROM usdt_raddresses --- total transactions, total and average fees, grouped by reference address
ORDER BY total_transactions DESC


--- Table for Question 3.1
CREATE TABLE question3_1 AS
SELECT 
	daily_btc_usdt."Date" AS date, 
	daily_btc_usdt."Close" AS btc_close_price, 
	total_transactions
FROM daily_btc_usdt

SELECT * FROM question3_1






--- PART 6: Tether data just with the adresses of interest
------ The two sending addresses that grant tokens
CREATE TABLE tether_interesting_sendingaddresses_gpt AS
SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE sending_address IN ('3MbYQMMmSkC3AgWkj9FMo5LsPTW1zBTwXL', 
						  '1Kaecr9gsYjRDJ8AWqZTBjQ6fd7RUwHynX'
						  )

SELECT * FROM tether_interesting_sendingaddresses_gpt --- 3Mb...XL sent just to 1NT...fz and revoked some tokens, 1Ka...nX just to 1Mm..LU



------ The two sending addresses that grant tokens: do they receive anything?
CREATE TABLE tether_interesting_sendingaddresses_gpt_r AS
SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE reference_address IN ('3MbYQMMmSkC3AgWkj9FMo5LsPTW1zBTwXL', 
						    '1Kaecr9gsYjRDJ8AWqZTBjQ6fd7RUwHynX'
						    )

SELECT * FROM tether_interesting_sendingaddresses_gpt_r --- just 3Mb...XL receives once




------ Reference addressses in the GPT transactions, did they send anything?
CREATE TABLE tether_interesting_referenceaddresses_gpt AS
SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE sending_address IN ('1NTMakcgVwQpMdGxRQnFKyb3G1FAJysSfz', 
						  '1MmiapMcvzovzF2dFcfRJN34jyRwJxUYLU'
						  )

SELECT * FROM tether_interesting_referenceaddresses_gpt


------ Reference addresses from tether_interesting_referenceaddresses_gpt, did they send anything?
CREATE TABLE tether_interesting_referenceaddresses_gpt_again AS
SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE sending_address IN ('1KYiKJEfdJtap9QX2v9BXJMpz2SfU4pgZw', 
						  '1GjgKbj69hDB7YPQF9KwPEy274jLzBKVLh'
						  )

SELECT * FROM tether_interesting_referenceaddresses_gpt_again
ORDER BY amount DESC





------ What did 1KYiKJEfdJtap9QX2v9BXJMpz2SfU4pgZw and 1GjgKbj69hDB7YPQF9KwPEy274jLzBKVLh receive?
SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE reference_address IN ('1KYiKJEfdJtap9QX2v9BXJMpz2SfU4pgZw', 
						    '1GjgKbj69hDB7YPQF9KwPEy274jLzBKVLh'
						    )





------ Table with transactions involving 1KYiKJEfdJtap9QX2v9BXJMpz2SfU4pgZw
CREATE TABLE usdt_transactions_oneky AS
SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE 
	sending_address = '1KYiKJEfdJtap9QX2v9BXJMpz2SfU4pgZw' OR 
	reference_address = '1KYiKJEfdJtap9QX2v9BXJMpz2SfU4pgZw'


	
	
--- Days
------ 22/02: What transactions happened during this day?
--------- Create table
CREATE TABLE twentytwofebrange AS
SELECT 
    date,
    SUM(amount) AS total_transactions
FROM tether_clean
WHERE date > '2018-02-16' 
  AND date < '2018-02-23'
GROUP BY date
ORDER BY date;


--------- Join with BTC
SELECT 
	bc.*,
	ttfr.total_transactions
FROM btc_clean AS bc
LEFT JOIN twentytwofebrange AS ttfr
	ON bc.date = ttfr.date


SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE 
	sending_address = '1Co1dhYDeF76DQyEyj4B5JdXF9J7TtfWWE' 

SELECT
	date,
	sending_address,
	reference_address,
	amount,
	tx_type
FROM tether_clean
WHERE 
	reference_address = '1Co1dhYDeF76DQyEyj4B5JdXF9J7TtfWWE'


	
	

	
	
SELECT * FROM daily_usdt --- date, total transactions, total and average fees
SELECT * FROM daily_usdt_saddresses --- date, total transactions, fees per transaction, grouped by sending address
SELECT * FROM daily_usdt_raddresses --- date, total transactions, fees per transaction, grouped by reference address
SELECT * FROM daily_btc_usdt --- Daily Bitcoin prices and USDT transactions 
SELECT * FROM usdt_by_trtype --- USDT transactions aggregated by transaction type
SELECT * FROM usdt_simple_send_agg --- USDT Simple Send transactions 
SELECT * FROM usdt_gpt_agg --- USDT Grant Property Tokens transactions 
SELECT * FROM usdt_rpt_agg --- USDT Revoke Property Tokens transactions 
SELECT * FROM usdt_saddresses --- total transactions, total and average fees, grouped by sending address
SELECT * FROM usdt_raddresses --- total transactions, total and average fees, grouped by reference address
SELECT * FROM question3_1 --- answer to question 3.1: table with daily BTC price and USDT transaction volume
