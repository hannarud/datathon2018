#data <- readRDS("data/BNB_lat.rds")

dataset <- readRDS("data/Dataset_BNB_20180710_for_clustering.rds")
i <-1
getChurn <- function(i,dataset) {
  print(i)
  client <- dataset$CLIENT_ID[i]
  cur_date <- dataset$TRAN_DATE[i]
  next_trans <- dataset[ (dataset$TRAN_DATE > cur_date) & (dataset$CLIENT_ID == client), ]
  if(nrow(next_trans) > 0 ) {
    next_date <- min(next_trans$TRAN_DATE)
  } else {
    next_date <- as.Date('2018-07-01')
  }
  return(next_date - cur_date > 25)
}

#system.time(
#dataset$churn <- sapply(c(1:nrow(dataset)),getChurn,dataset)
#) # 0.5 s * 2519960

clients <- unique(dataset$CLIENT_ID)

end_date <- as.Date('2018-07-01')

dataset$churn <- 1

getClientChurn <- function(client_history,end_date) {
  as.integer(diff.difftime(c(client_history$TRAN_DATE,end_date)) > 25)
}

system.time(
for(i in c(1:length(clients))) {
  print(i)
  client <- clients[i]
  index <- (dataset$CLIENT_ID == client)
  client_history <- dataset[ index, ]
  dataset$churn[index] <- as.integer(diff.difftime(c(client_history$TRAN_DATE,end_date)) > 25)
}
)

dataset$month_tran <- 0
dataset$week_tran <- 0

for(i in c(1:length(clients[1]))) {
  print(i)
  client <- clients[i]
  index <- (dataset$CLIENT_ID == client)
  client_history <- dataset[ index, ]
  #dataset$churn[index] <- as.integer(diff.difftime(c(client_history$TRAN_DATE,end_date)) > 25)
  for(j in c(1:length(client_history))) {
    cur_history <- client_history[c(1:(j-1)),]
    cur_date <- client_history$TRAN_DATE[j]
    month_history <- cur_history[cur_history$TRAN_DATE - cur_date < 30,]
    week_history <- cur_history[cur_history$TRAN_DATE - cur_date < 7,]
    
    dataset$month_tran[index[j]] <- length(month_history)
    dataset$week_tran[index[j]] <- length(month_history)
  }
  
  
}  



