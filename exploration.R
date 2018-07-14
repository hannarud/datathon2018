#---------------------------sys-----------------------------------------------
Sys.setlocale(category = "LC_CTYPE", locale = "Russian")

require(data.table)
require(stringi)
require(countrycode)
require(stringr)

#--------------------------- preprocess ---------------------------------------

dataset <- as.data.table(read.table("data/Dataset_BNB_20180710.txt", sep = ';', header = TRUE, stringsAsFactors = FALSE))
saveRDS(dataset, "data/Dataset_BNB_20180710.rds")

dataset <- readRDS("data/Dataset_BNB_20180710.rds")

# convert cyrrilic to latin
dataset$CLIENT_REGION <- str_replace_all(dataset$CLIENT_REGION, 'ь','')
dataset$CLIENT_REGION <- str_replace_all(dataset$CLIENT_REGION, 'я','а')
dataset$CLIENT_REGION <- as.factor(sapply(dataset$CLIENT_REGION, function(x) {stringi::stri_trans_general(x, 'latin')}))

# mark binary as factor
dataset$IS_CITIZEN <- as.factor(dataset$IS_CITIZEN)
dataset$IS_RESIDENT <- as.factor(dataset$IS_RESIDENT)
dataset$IS_CREDIT_CARD <- as.factor(dataset$IS_CREDIT_CARD)

# Convert amounts to double
dataset$TRAN_AMOUNT <- as.double(dataset$TRAN_AMOUNT)
dataset$TRAN_AMOUNT_BYN <- as.double(dataset$TRAN_AMOUNT_BYN)

# convert country to 3 letters system
index_t = grepl("^[[:alpha:]]{2}$",dataset$TRAN_COUNTRY)
dataset[index_t]$TRAN_COUNTRY = countrycode(dataset$TRAN_COUNTRY[index_t], "iso2c", "iso3c")

# dates as Date type
dataset$TRAN_DATE <- as.Date(as.character(dataset$TRAN_DATE), format = "%d/%m/%Y")

# upper to cities
dataset$TRAN_CITY <- toupper(dataset$TRAN_CITY)

saveRDS(file = "data/BNB_lat.rds", dataset)
write.csv2(file = "data/BNB_lat.csv", dataset, sep = ";")

#--------------------------- primary analysis---------------------------------
#load clean RDS
dataset <- as.data.table(readRDS("data/BNB_lat.RDS"))

sum(is.na(dataset)) # Нет NA!!! COOL!!!

summary(dataset)

# TODO: Cyrillic!!! Re-encode to latin
table(dataset$CLIENT_REGION)

# Not all TRAN_ID are unique
length(unique(dataset$TRAN_ID))/nrow(dataset)
nrow(dataset) - length(unique(dataset$TRAN_ID)) ## 2160

sum(duplicated(dataset)) ## 643 complete duplicates
duplic <- which(duplicated(dataset))
dataset$TRAN_ID[duplic[1]]
dataset[dataset$TRAN_ID == dataset$TRAN_ID[duplic[1]], ]
# Omit complete duplicates
dataset <- dataset[- duplic, ]

# Find other duplicates by TRAN_ID
duplic_tran_id <- which(duplicated(dataset$TRAN_ID)) ## 1517
dataset[dataset$TRAN_ID == dataset$TRAN_ID[duplic_tran_id[1]], ]
# Check that all the rest of duplicates occur at CARD_ID
sum(duplicated(dataset[, setdiff(names(dataset), "CARD_ID")])) == length(duplic_tran_id)
# Maybe later we'll change it, but now we remove such duplicates
dataset <- dataset[- duplic_tran_id, ]

# So now TRAN_ID is key
length(unique(dataset$TRAN_ID)) == nrow(dataset)

table(dataset$TRAN_TYPE)
# "CASH" - снятие наличных (236111 штуки)
# "PMT" - payment, оплата (2283849 штуки)

length(table(dataset$MCC_CODE)) # 368 - их мало и они очень полезные!!!
# TODO: Потом перекодировать

table(dataset$MCC_CODE, dataset$TRAN_TYPE) # В принципе, CASH вписывается всего в 2 MCC_CODE

min(dataset$TRAN_DATE)
max(dataset$TRAN_DATE)

dataset$daytype <- ifelse(grepl("S(at|un)", weekdays(dataset$TRAN_DATE, abbr = TRUE)), "weekend", "weekday")
weekday_holiday <- as.Date(c("01/01/2018",
                             "08/03/2018",
                             "17/04/2018",
                             "01/05/2018",
                             "09/05/2018"), format = "%d/%m/%Y")
dataset$daytype[dataset$TRAN_DATE %in% weekday_holiday] <- "weekend"
semi_holiday <- as.Date(c("02/01/2018",
                          "20/01/2018",
                          "03/03/2018",
                          "09/03/2018",
                          "14/04/2018",
                          "16/04/2018",
                          "28/04/2018",
                          "30/04/2018"), format = "%d/%m/%Y")
dataset$daytype[dataset$TRAN_DATE %in% semi_holiday] <- "semi"
dataset$daytype <- as.factor(dataset$daytype)

dataset$month <- format(dataset$TRAN_DATE, "%m")
days_in_month <- data.frame(month = c("01", "02", "03", "04", "05", "06"),
                            num_of_days_in_month = c(31, 28, 31, 30, 31, 30))
dataset <- left_join(dataset, days_in_month, by = c("month"))
dataset$week <- strftime(dataset$TRAN_DATE, format = "%V")

library(ggplot2)

ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = dataset$TRAN_DATE,
                               fill = daytype),
                 bins = length(unique(dataset$TRAN_DATE))) + labs(fill = "Day")

length(unique(dataset$MERCHANT_ID))
# Maybe it makes sense to take 5 most popular merchant ID as factors as well, not sure now
merchant_tabl <- as.data.frame(table(dataset$MERCHANT_ID))

length(unique(dataset$TERMINAL_ID))
terminal_tabl <- as.data.frame(table(dataset$TERMINAL_ID))
terminal_tabl$Var1[grepl("^[A-Za-z]+$", terminal_tabl$Var1)]
terminal_tabl$Freq[terminal_tabl$Var1 == "na"]

length(unique(dataset$TERMINAL_LOCATION))
location_tabl <- as.data.frame(table(dataset$TERMINAL_LOCATION))

length(unique(toupper(dataset$TRAN_COUNTRY))) # TODO Kseniya

length(unique(toupper(dataset$TRAN_CITY)))

length(unique(toupper(dataset$CLIENT_ID))) ## 31287
length(unique(toupper(dataset$CONTRACT_ID))) ## 34015

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

# Many to one
sum(!d(dataset$CONTRACT_ID) & d(dataset$CARD)) # So no card belong to two contracts

table(dataset$CARD_CURRENCY) ## Factor

table(dataset$CARD_TYPE) ## Factor

table(dataset$IS_CREDIT_CARD)  ## Factor

table(dataset$GENDER) ## Factor

hist(dataset$AGE)

table(dataset$IS_RESIDENT) ## Factor

table(dataset$IS_CITIZEN) ## Factor

table(dataset$CLIENT_REGION)  ## Factor

saveRDS(dataset, "data/Dataset_BNB_20180710_for_clustering.rds")
