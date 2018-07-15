#data <- readRDS("data/BNB_lat.rds")

dataset <- readRDS("data/Dataset_BNB_20180710_for_clustering.rds")
add_feature <- read.table('data/is_payroll.txt',sep=';',header = T)

dataset <- left_join(dataset,add_feature)
dataset$IS_PAYROLL[is.na(dataset$IS_PAYROLL)] <- 1

# i <-1
# getChurn <- function(i,dataset) {
#   print(i)
#   client <- dataset$CLIENT_ID[i]
#   cur_date <- dataset$TRAN_DATE[i]
#   next_trans <- dataset[ (dataset$TRAN_DATE > cur_date) & (dataset$CLIENT_ID == client), ]
#   if(nrow(next_trans) > 0 ) {
#     next_date <- min(next_trans$TRAN_DATE)
#   } else {
#     next_date <- as.Date('2018-07-01')
#   }
#   return(next_date - cur_date > 25)
# }

#system.time(
#dataset$churn <- sapply(c(1:nrow(dataset)),getChurn,dataset)
#) # 0.5 s * 2519960

clients <- unique(dataset$CLIENT_ID)

end_date <- as.Date('2018-07-01')

#dataset$churn <- 1

getClientChurn <- function(client_history,end_date) {
  as.integer(diff.difftime(c(client_history$TRAN_DATE,end_date)) > 25)
}

# system.time(
# for(i in c(1:length(clients))) {
#   print(i)
#   client <- clients[i]
#   index <- (dataset$CLIENT_ID == client)
#   client_history <- dataset[ index, ]
#   dataset$churn[index] <- as.integer(diff.difftime(c(client_history$TRAN_DATE,end_date)) > 25)
# }
# )

clear_data <- dataset[dataset$TRAN_AMOUNT_BYN > 4,]
sum(clear_data$churn)

time_histories <- split(clear_data[,c("TRAN_ID","TRAN_DATE")],clear_data$CLIENT_ID)
client_histories <- split(clear_data,clear_data$CLIENT_ID)
values <- lapply(client_histories,getClientChurn,end_date)
churn <- unsplit(values,clear_data$CLIENT_ID)
sum(churn)

dataset$churn <- as.integer(dataset$TRAN_ID %in% clear_data$TRAN_ID[churn == 1])
sum(dataset$churn)
#all.equal(churn,dataset$churn)

saveRDS(dataset, "data/Dataset_BNB_churn.rds")
######################################################
dataset <- readRDS("data/Dataset_BNB_churn.rds")

client_histories <- split(dataset,dataset$CLIENT_ID)
values <- sapply(client_histories,nrow)
summary(values)
quantile(values,probs = seq(0.75, 1, 0.05))

bots <- names(client_histories)[values > 180]
bots_values <- sapply(client_histories,function(h) any(h$churn))
sum(bots_values & (values > 180))

last_date <- sapply(client_histories,function(h) tail(h$TRAN_DATE,1))
last_time <- as.integer(end_date - last_date)
sum(last_time>40)/length(client_histories)

payrolls <- sapply(client_histories,function(h) any(h$IS_PAYROLL == 1))

long_sleep <- sapply(client_histories,function(h) as.integer(max(c(0,diff.difftime(h$TRAN_DATE)))))
sum(long_sleep > 40)/length(client_histories)

sum((long_sleep > 40)|(last_time>40))/length(client_histories)

sum(long_sleep > 25)/sum((long_sleep > 25)|(last_time>25))

sum((last_time > 40) & (payrolls))/sum(last_time>40)

fired <- names(client_histories)[(last_time > 40) & (payrolls)]

length(intersect(fired,bots))

filtered <- union(fired,bots)

filtered_dataset <- dataset[! dataset$CLIENT_ID %in% filtered,]

saveRDS(filtered_dataset, "data/Dataset_BNB_filtered.rds")
######################################################
dataset <- readRDS("data/Dataset_BNB_filtered.rds")
client_histories <- split(dataset,dataset$CLIENT_ID)

# churn_id <- dataset$CLIENT_ID[dataset$churn == 1]
# churn_clients <- unique(churn_id)
# 
# 
# mul_churn <- unique(churn_id[duplicated(churn_id)])
# 
# lost_clients <- setdiff(churn_clients,mul_churn)
# 
# client_history <- dataset[ dataset$CLIENT_ID == mul_churn[3], ]
# 
# churn_unpaid <- dataset[(dataset$churn == 1) & (dataset$IS_PAYROLL == 1),]
# clients_unpaid <- unique(churn_unpaid$CLIENT_ID)
# length(intersect(clients_unpaid,lost_clients))
###################################################

####################################################
# dataset$month_tran <- NULL
# dataset$week_tran <- 0

histFeatures <- function(client_history) {
  index <- c(1:nrow(client_history))
  month_history <- lapply(index,function(j) {
    cur_date <- client_history$TRAN_DATE[j]
    client_history[(index < (j-1)) & (cur_date - client_history$TRAN_DATE < 30),]
  })
  client_history$month_tran <- sapply(month_history,nrow)
  client_history$month_sum <- sapply(month_history,function(h) sum(h$TRAN_AMOUNT_BYN))
  client_history$month_sum_cash <- sapply(month_history,function(h) sum(h$TRAN_AMOUNT_BYN[h$TRAN_TYPE == "CASH"]))
  client_history$month_sum_pmt <- sapply(month_history,function(h) sum(h$TRAN_AMOUNT_BYN[h$TRAN_TYPE == "PMT"]))
  client_history$month_cash <- sapply(month_history,function(h) sum(h$TRAN_TYPE == "CASH"))
  client_history$month_pmt <- sapply(month_history,function(h) sum(h$TRAN_TYPE == "PMT"))
  client_history$month_mcc <- sapply(month_history,function(h) length(unique(h$MCC_CODE)))
  client_history$month_countries <- sapply(month_history,function(h) length(unique(h$TRAN_COUNTRY)))
  client_history$month_cities <- sapply(month_history,function(h) length(unique(h$TRAN_CITY)))
  client_history$month_cards <- sapply(month_history,function(h) length(unique(h$CARD_ID)))
  client_history$month_credit <- sapply(month_history,function(h) as.integer(any(h$IS_CREDIT == 1)))
  client_history$prev_time <- c(0,as.integer(diff.difftime(client_history$TRAN_DATE)))
  client_history
}

lastMonthSum <- function(client_history) {
  index <- c(1:nrow(client_history))
  sapply(index,function(j) {
    cur_history <- client_history[index < (j-1),]
    cur_date <- client_history$TRAN_DATE[j]
    month_history <- cur_history[cur_date - cur_history$TRAN_DATE < 30,]

    sum(month_history$TRAN_AMOUNT)
  })
}

client_history <- client_histories[[1]]
system.time(
lastMonthSum(client_histories[[1]])
)
system.time(
values <- lapply(client_histories,lastMonthSum)
)

system.time(
  h<-histFeatures(client_histories[[1]])
)

churn_clients <- sapply(client_histories,function(h) any(h$churn))
sum(churn_clients)

last_date <- sapply(client_histories,function(h) tail(h$TRAN_DATE,1))
last_time <- as.integer(end_date - last_date)
sum(last_time>25)

lost_clients <- (last_time>25) & (churn_clients)
sum(lost_clients)
system.time(
  lost_hist<-lapply(client_histories[lost_clients],histFeatures)
)

returned_clients <- (last_time<=25) & (churn_clients)
sum(returned_clients)
system.time(
  returned_hist<-lapply(client_histories[returned_clients],histFeatures)
)

good_clients <- !(churn_clients)
sum(good_clients)
system.time(
  good_hist<-lapply(client_histories[good_clients],histFeatures)
)
rm(client_histories,dataset)

system.time(
  good <- Reduce(rbind,good_hist)
)
system.time(
  lost <- Reduce(rbind,lost_hist)
)

selectFeatures<- function(client_history){
  fields <- c(1,31:44)
  begin <- as.Date('2018-02-01')
  end <- as.Date('2018-06-01')
  index <- (client_history$TRAN_DATE < end) & (client_history$TRAN_DATE > begin)
  client_history[index,fields]
}
l <- selectFeatures(lost_hist[[1]])
system.time(
  lost_sel <- lapply(lost_hist,selectFeatures)
)
lost1 <- Reduce(rbind,lost_sel)
rm(lost_hist,lost_sel)

system.time(
  good_sel <- lapply(good_hist,selectFeatures)
)
system.time(
  good1 <- Reduce(rbind,good_sel)
)

system.time(
  returned_sel <- lapply(returned_hist,selectFeatures)
)
system.time(
  returned1 <- Reduce(rbind,returned_sel)
)
# month_tran <- unsplit(values,dataset$CLIENT_ID)
# 
# 
# for(i in c(1:length(clients[1]))) {
#   print(i)
#   client <- clients[i]
#   index <- (dataset$CLIENT_ID == client)
#   client_history <- dataset[ index, ]
#   #dataset$churn[index] <- as.integer(diff.difftime(c(client_history$TRAN_DATE,end_date)) > 25)
#   for(j in c(1:length(client_history))) {
#     cur_history <- client_history[c(1:(j-1)),]
#     cur_date <- client_history$TRAN_DATE[j]
#     month_history <- cur_history[cur_history$TRAN_DATE - cur_date < 30,]
#     week_history <- cur_history[cur_history$TRAN_DATE - cur_date < 7,]
#     
#     dataset$month_tran[index[j]] <- length(month_history)
#     dataset$week_tran[index[j]] <- length(month_history)
#   }
#   
#   
# }  




