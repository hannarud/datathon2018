
Sys.setlocale(category = "LC_CTYPE", locale = "Russian")

dataset <- readRDS("data/Dataset_BNB_20180710_for_clustering.rds")

# Все фичи генерим для каждого CLIENT_ID

library(dplyr)

# Create the function for mode computation
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# По сумме транзакций за все время
summarized_amounts_ever <- dataset %>% group_by(CLIENT_ID) %>%
  summarise(total_amount_of_transactions = n(),
            total_amount_paid = sum(TRAN_AMOUNT_BYN),
            max_sum_ever = max(TRAN_AMOUNT_BYN),
            mode_sum_ever = getmode(TRAN_AMOUNT_BYN),
            med_sum_ever = median(TRAN_AMOUNT_BYN),
            min_sum_ever = min(TRAN_AMOUNT_BYN))

# По количеству транзакций за каждый день
summarized_num_of_transaction_by_day <- dataset %>% group_by(CLIENT_ID, TRAN_DATE) %>%
  summarise(num_of_transaction_per_day = n())

summarized_num_of_transactions_by_day_by_client <- summarized_num_of_transaction_by_day %>%
  group_by(CLIENT_ID) %>%
  summarise(max_num_of_transactions_per_day = max(num_of_transaction_per_day),
            median_num_of_transactions_per_day = median(num_of_transaction_per_day))

# По количеству транзакций за каждый месяц
summarized_num_of_transaction_by_month <- dataset %>% group_by(CLIENT_ID, month, num_of_days_in_month) %>%
  summarise(num_of_transaction_per_month = n(),
            total_amount_of_payments_per_month = sum(TRAN_AMOUNT_BYN),
            average_sum_of_payments_per_month = mean(TRAN_AMOUNT_BYN))

summarized_num_of_transaction_by_month <- summarized_num_of_transaction_by_month %>%
  mutate(num_of_transaction_per_month_div_by_n_days = num_of_transaction_per_month/num_of_days_in_month,
         total_amount_of_payments_per_month_div_by_n_days = total_amount_of_payments_per_month/num_of_days_in_month)

# Забиваем на стандартизацию по количеству дней в месяце
# Считаем для каждого
summarized_num_of_transactions_by_month_by_client <- summarized_num_of_transaction_by_month %>%
  group_by(CLIENT_ID) %>%
  summarise(max_num_of_transactions_per_month_standartized = max(num_of_transaction_per_month),
            min_num_of_transactions_per_month_standartized = min(num_of_transaction_per_month),
            sigma_num_of_transactions_per_month_standartized = sd(num_of_transaction_per_month),
            max_total_amount_of_payments_per_month = max(total_amount_of_payments_per_month),
            min_total_amount_of_payments_per_month = min(total_amount_of_payments_per_month),
            sigma_total_amount_of_payments_per_month = sd(total_amount_of_payments_per_month),
            sigma_average_sum_of_payments_per_month = sd(average_sum_of_payments_per_month))

summarized_num_of_transactions_by_month_by_client$sigma_num_of_transactions_per_month_standartized[
  is.na(summarized_num_of_transactions_by_month_by_client$sigma_num_of_transactions_per_month_standartized)] <- 0
summarized_num_of_transactions_by_month_by_client$sigma_total_amount_of_payments_per_month[
  is.na(summarized_num_of_transactions_by_month_by_client$sigma_total_amount_of_payments_per_month)] <- 0
summarized_num_of_transactions_by_month_by_client$sigma_average_sum_of_payments_per_month[
  is.na(summarized_num_of_transactions_by_month_by_client$sigma_average_sum_of_payments_per_month)] <- 0

# По количеству транзакций за каждую неделю
summarized_num_of_transaction_by_week <- dataset %>% group_by(CLIENT_ID, week) %>%
  summarise(num_of_transaction_per_week = n(),
            total_amount_of_payments_per_week = sum(TRAN_AMOUNT_BYN))

# Пока для недели по юзерам не аггрегируем

# Процент транзакций пользователя в иностранной валюте - в числе транзакций и в сумме
summarized_transaction_by_currency <- dataset %>% group_by(CLIENT_ID, TRAN_CURRENCY) %>%
  summarise(sum_tran_by_currency = sum(TRAN_AMOUNT_BYN),
            num_of_tran_by_currency = n())

summarized_transaction_by_currency$in_BYN <- ifelse(summarized_transaction_by_currency$TRAN_CURRENCY == 933, "in_BYN", "non_BYN")

summarized_transaction_by_currency_sum <- summarized_transaction_by_currency %>% group_by(CLIENT_ID, in_BYN) %>%
  summarise(sum_BYN_or_not = sum(sum_tran_by_currency),
            num_BYN_or_not = sum(num_of_tran_by_currency))

summarized_transaction_by_currency_sum_finally <- data.frame(
  CLIENT_ID = unique(summarized_transaction_by_currency_sum$CLIENT_ID))

summarized_transaction_by_currency_sum_finally$percent_sum_BYN <- sapply(
  X = summarized_transaction_by_currency_sum_finally$CLIENT_ID, function(x) {
    sum_BYN = summarized_transaction_by_currency_sum$sum_BYN_or_not[summarized_transaction_by_currency_sum$in_BYN == "in_BYN"  & summarized_transaction_by_currency_sum$CLIENT_ID == x]
    if(length(sum_BYN) == 0) {
      sum_BYN = 0
    }
    sum_not_BYN = summarized_transaction_by_currency_sum$sum_BYN_or_not[summarized_transaction_by_currency_sum$in_BYN == "non_BYN" & summarized_transaction_by_currency_sum$CLIENT_ID == x]
    if(length(sum_not_BYN) == 0) {
      sum_not_BYN = 0
    }
    return(round(sum_BYN/(sum_BYN + sum_not_BYN)*100, digits = 2))
  })

summarized_transaction_by_currency_sum_finally$percent_transactions_BYN <- sapply(
  X = summarized_transaction_by_currency_sum_finally$CLIENT_ID, function(x) {
    num_BYN = summarized_transaction_by_currency_sum$num_BYN_or_not[summarized_transaction_by_currency_sum$in_BYN == "in_BYN"  & summarized_transaction_by_currency_sum$CLIENT_ID == x]
    if(length(num_BYN) == 0) {
      num_BYN = 0
    }
    num_not_BYN = summarized_transaction_by_currency_sum$num_BYN_or_not[summarized_transaction_by_currency_sum$in_BYN == "non_BYN" & summarized_transaction_by_currency_sum$CLIENT_ID == x]
    if(length(num_not_BYN) == 0) {
      num_not_BYN = 0
    }
    return(round(num_BYN/(num_BYN + num_not_BYN)*100, digits = 2))
  })

# Процент транзакций пользователя за границей (т.е. и страна не Беларусь, и валюта не белрубль)
summarized_transaction_by_currency_and_country <- dataset %>% group_by(CLIENT_ID, TRAN_COUNTRY, TRAN_CURRENCY) %>%
  summarise(sum_tran_by_currency = sum(TRAN_AMOUNT_BYN),
            num_of_tran_by_currency = n())

summarized_transaction_by_currency_and_country$abroad <- ifelse(
  summarized_transaction_by_currency_and_country$TRAN_COUNTRY != "BLR" & 
    summarized_transaction_by_currency_and_country$TRAN_CURRENCY != 933, "abroad", "in_BLR")

summarized_transaction_by_currency_and_country_sum <- summarized_transaction_by_currency_and_country %>%
  group_by(CLIENT_ID, abroad) %>%
  summarise(sum_abroad_or_not = sum(sum_tran_by_currency),
            num_abroad_or_not = sum(num_of_tran_by_currency))

summarized_transaction_by_currency_and_country_sum_finally <- data.frame(
  CLIENT_ID = unique(summarized_transaction_by_currency_and_country_sum$CLIENT_ID))

summarized_transaction_by_currency_and_country_sum_finally$percent_sum_abroad <- sapply(
  X = summarized_transaction_by_currency_and_country_sum_finally$CLIENT_ID, function(x) {
    sum_abroad = summarized_transaction_by_currency_and_country_sum$sum_abroad_or_not[
      summarized_transaction_by_currency_and_country_sum$abroad == "abroad"  &
        summarized_transaction_by_currency_and_country_sum$CLIENT_ID == x]
    if(length(sum_abroad) == 0) {
      sum_abroad = 0
    }
    sum_in_BLR = summarized_transaction_by_currency_and_country_sum$sum_abroad_or_not[
      summarized_transaction_by_currency_and_country_sum$in_BYN == "in_BLR" &
        summarized_transaction_by_currency_and_country_sum$CLIENT_ID == x]
    if(length(sum_in_BLR) == 0) {
      sum_in_BLR = 0
    }
    return(round(sum_abroad/(sum_abroad + sum_in_BLR)*100, digits = 2))
  })

summarized_transaction_by_currency_sum_finally$percent_num_abroad <- sapply(
  X = summarized_transaction_by_currency_sum_finally$CLIENT_ID, function(x) {
    sum_abroad = summarized_transaction_by_currency_and_country_sum$sum_abroad_or_not[
      summarized_transaction_by_currency_and_country_sum$abroad == "abroad"  &
        summarized_transaction_by_currency_and_country_sum$CLIENT_ID == x]
    if(length(sum_abroad) == 0) {
      sum_abroad = 0
    }
    sum_in_BLR = summarized_transaction_by_currency_and_country_sum$sum_abroad_or_not[
      summarized_transaction_by_currency_and_country_sum$in_BYN == "in_BLR" &
        summarized_transaction_by_currency_and_country_sum$CLIENT_ID == x]
    if(length(sum_in_BLR) == 0) {
      sum_in_BLR = 0
    }
    return(round(sum_abroad/(sum_abroad + sum_in_BLR)*100, digits = 2))
  })

# Процент интернет-транзакций пользователя - но пока непонятно, как их выщемить. Это однозначно транзакции в стране не Беларусь, но в белорусских рублях, а что еще?

# По MCC кодам проверить, на какие категории тратились деньги в процентах

# дынаміка: sum(month1+month2+month3) < sum(month4 + month5 + month6)

# Final table with features by client

length(unique(dataset$CLIENT_ID)) ## 31287
dim(unique(dataset[, c("CLIENT_ID", "GENDER", "IS_RESIDENT", "IS_CITIZEN", "CLIENT_REGION")])) # Все уникальные
dim(unique(dataset[, c("CLIENT_ID", "AGE")])) # Не все уникальные - возраст меняется - берем максимум

client_dataset <- dataset %>% group_by(CLIENT_ID, GENDER, IS_RESIDENT, IS_CITIZEN, CLIENT_REGION) %>%
  summarise(AGE = max(AGE))

temp <- unique(dataset[, c("CLIENT_ID", "IS_CREDIT_CARD")]) %>% filter(IS_CREDIT_CARD == 1) %>%
  rename(HAS_CREDIT_CARD = IS_CREDIT_CARD)
client_dataset <- left_join(client_dataset, temp, by = c("CLIENT_ID"))
client_dataset$HAS_CREDIT_CARD[is.na(client_dataset$HAS_CREDIT_CARD)] <- 0
rm(temp)

temp <- unique(dataset[, c("CLIENT_ID", "CARD_ID")]) %>% group_by(CLIENT_ID) %>%
  summarise(NUMBER_OF_CARDS = n())
client_dataset <- left_join(client_dataset, temp, by = c("CLIENT_ID"))
sum(is.na(client_dataset$NUMBER_OF_CARDS))
rm(temp)

temp <- unique(dataset[, c("CLIENT_ID", "CARD_CURRENCY")]) %>% filter(as.integer(CARD_CURRENCY) != 933) %>%
  distinct(CLIENT_ID) %>% mutate(HAS_FOREIGN_CURRENCY_CARD = c(1))
client_dataset <- left_join(client_dataset, temp, by = c("CLIENT_ID"))
client_dataset$HAS_FOREIGN_CURRENCY_CARD[is.na(client_dataset$HAS_FOREIGN_CURRENCY_CARD)] <- 0
rm(temp)

# Joining with previously obtained values
client_dataset <- left_join(client_dataset, summarized_transaction_by_currency_sum_finally, by = c("CLIENT_ID"))
client_dataset <- left_join(client_dataset, summarized_amounts_ever, by = c("CLIENT_ID"))
client_dataset <- left_join(client_dataset, summarized_num_of_transactions_by_day_by_client, by = c("CLIENT_ID"))
client_dataset <- left_join(client_dataset, summarized_num_of_transactions_by_month_by_client, by = c("CLIENT_ID"))

dim(client_dataset)

sum(is.na(client_dataset))

saveRDS(object = client_dataset, file = "data/client_dataset.rds")
