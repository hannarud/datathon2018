
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
client_dataset <- left_join(client_dataset, summarized_amounts_ever, by = c("CLIENT_ID"))
client_dataset <- left_join(client_dataset, summarized_num_of_transactions_by_day_by_client, by = c("CLIENT_ID"))
client_dataset <- left_join(client_dataset, summarized_num_of_transactions_by_month_by_client, by = c("CLIENT_ID"))

dim(client_dataset)

sum(is.na(client_dataset))

saveRDS(object = client_dataset, file = "data/client_dataset.rds")


# Закончили работать с суммами и количеством транзакций, теперь дополняем другой информацией







# Проверка, что не существует двух клиентов с одинаковым CONTRACT_ID
d <- function(x) duplicated(x) | duplicated(x, fromLast=TRUE)
# # One to one
# dataset[!d(dataset$CLIENT_ID) & !d(dataset$CONTRACT_ID),]
# # One to many
# dataset[d(dataset$CLIENT_ID) & !d(dataset$CONTRACT_ID),]
# Many to one
sum(!d(dataset$CLIENT_ID) & d(dataset$CONTRACT_ID)) # So 9 contracts belong to more than one client
which(!d(dataset$CLIENT_ID) & d(dataset$CONTRACT_ID))

dataset[which(!d(dataset$CLIENT_ID) & d(dataset$CONTRACT_ID)), ]
table(dataset$CLIENT_ID[dataset$CONTRACT_ID == 18734873], dataset$CONTRACT_ID[dataset$CONTRACT_ID == 18734873])

length(unique(toupper(dataset$CARD_ID))) ## 38739






