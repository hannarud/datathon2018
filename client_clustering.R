Sys.setlocale(category = "LC_CTYPE", locale = "Russian")

client_dataset <- readRDS("data/client_dataset.rds")

client_dataset$reg_gender_strata <- ifelse(client_dataset$GENDER == "M",
                                           ifelse(client_dataset$CLIENT_REGION == "Minsk",
                                                  "M_Minsk",
                                                  ifelse(client_dataset$CLIENT_REGION == "Minskaa obl.",
                                                         "M_MinskReg", "M_NoMinsk")),
                                           ifelse(client_dataset$CLIENT_REGION == "Minsk",
                                                  "F_Minsk",
                                                  ifelse(client_dataset$CLIENT_REGION == "Minskaa obl.",
                                                         "F_MinskReg", "F_NoMinsk")))

client_dataset$NUMBER_OF_CARDS_1_2_more <- ifelse(client_dataset$NUMBER_OF_CARDS > 2, "more",
                                                  ifelse(client_dataset$NUMBER_OF_CARDS == 2, "two", "one"))
# Encode all features to factors
cols_of_interest <- setdiff(names(client_dataset), c("CLIENT_ID", "GENDER", "IS_RESIDENT", "IS_CITIZEN",
                                                     "CLIENT_REGION", "HAS_CREDIT_CARD", "NUMBER_OF_CARDS",
                                                     "HAS_FOREIGN_CURRENCY_CARD", "reg_gender_strata", "NUMBER_OF_CARDS_1_2_more"))

for(i in cols_of_interest) {
  print(i)
  start = log(min(client_dataset[[i]]))
  if(is.infinite(start)) {
    start = 0
  }
  seqq = seq(from = start, to = log(max(client_dataset[[i]])), by = 1)
  if(length(seqq < 3)) {
    seqq = seq(from = start, to = log(max(client_dataset[[i]])), length.out = 4)
  }
  if(length(seqq > 6)) {
    seqq = seq(from = start, to = log(max(client_dataset[[i]])), length.out = 5)
  }
  
  client_dataset[[paste0(i, "_factor")]] <- cut(log(client_dataset[[i]]), breaks = seqq)
}


trial <- as.data.frame(table(client_dataset$GENDER,
                             client_dataset$IS_RESIDENT,
                             client_dataset$IS_CITIZEN,
                             client_dataset$HAS_CREDIT_CARD,
                             client_dataset$HAS_FOREIGN_CURRENCY_CARD,
                             client_dataset$reg_gender_strata,
                             client_dataset$NUMBER_OF_CARDS_1_2_more,
                             client_dataset$AGE_factor,
                             client_dataset$total_amount_of_transactions_factor))

trial <- trial[trial$Freq > 100, ]

apply(trial, 2, table)

# Выводы
# Мы можем исключить и выделить сразу в отдельные кластеры тех, у кого
# 


write.csv2(client_dataset, file = "data/client_dataset_discretized.csv", row.names = FALSE)






> names(client_dataset)
[1] "CLIENT_ID"                                              
[2] "GENDER"                                                 
[3] "IS_RESIDENT"                                            
[4] "IS_CITIZEN"                                             
[5] "CLIENT_REGION"                                          
[6] "AGE"                                                    
[7] "HAS_CREDIT_CARD"                                        
[8] "NUMBER_OF_CARDS"                                        
[9] "HAS_FOREIGN_CURRENCY_CARD"                              
[10] "total_amount_of_transactions"                           
[11] "total_amount_paid"                                      
[12] "max_sum_ever"                                           
[13] "mode_sum_ever"                                          
[14] "med_sum_ever"                                           
[15] "min_sum_ever"                                           
[16] "max_num_of_transactions_per_day"                        
[17] "median_num_of_transactions_per_day"                     
[18] "max_num_of_transactions_per_month_standartized"         
[19] "min_num_of_transactions_per_month_standartized"         
[20] "sigma_num_of_transactions_per_month_standartized"       
[21] "max_total_amount_of_payments_per_month"                 
[22] "min_total_amount_of_payments_per_month"                 
[23] "sigma_total_amount_of_payments_per_month"               
[24] "sigma_average_sum_of_payments_per_month"                
[25] "reg_gender_strata"                                      
[26] "NUMBER_OF_CARDS_1_2_more"                               
[27] "AGE_factor"                                             
[28] "total_amount_of_transactions_factor"                    
[29] "total_amount_paid_factor"                               
[30] "max_sum_ever_factor"                                    
[31] "mode_sum_ever_factor"                                   
[32] "med_sum_ever_factor"                                    
[33] "min_sum_ever_factor"                                    
[34] "max_num_of_transactions_per_day_factor"                 
[35] "median_num_of_transactions_per_day_factor"              
[36] "max_num_of_transactions_per_month_standartized_factor"  
[37] "min_num_of_transactions_per_month_standartized_factor"  
[38] "sigma_num_of_transactions_per_month_standartized_factor"
[39] "max_total_amount_of_payments_per_month_factor"          
[40] "min_total_amount_of_payments_per_month_factor"          
[41] "sigma_total_amount_of_payments_per_month_factor"        
[42] "sigma_average_sum_of_payments_per_month_factor" 










# write.csv2(x = client_dataset, file = "docs/client_dataset.csv", row.names = FALSE)

library(ggplot2)
library(grid)
library(gridExtra)

pdf("reports/client_dataset_histograms.pdf", onefile=TRUE)
for(i in names(client_dataset)) {
  if(length(unique(client_dataset[[i]]) < 10)) {
    c <- ggplot(data = client_dataset, aes_string(x = i)) +
      geom_histogram(stat = "count") + ggtitle(i)
  } else {
    temp <- as.numeric(client_dataset[[i]])
    c <- ggplot(data = client_dataset) +
      geom_histogram(mapping = aes(x = temp), bins = min(c(length(unique(temp))/10, 100))) + ggtitle(i)
  }
  grid.arrange(c, ncol=1)
}
dev.off()

categories <- setdiff(names(client_dataset), c("CLIENT_ID", "CLIENT_REGION"))

pdf("reports/client_dataset_histograms_wrt_CLIENT_REGION.pdf", onefile=TRUE)
for(i in categories) {
  if(length(unique(client_dataset[[i]])) < 10) {
    c <- ggplot(data = client_dataset, mapping = aes_string(x = i)) + geom_bar(stat="count") +
      ggtitle(i) + facet_wrap(~ CLIENT_REGION, scales = "free")
  } else {
    bins_num = ifelse(length(unique(client_dataset[[i]])) > 20, 100, length(unique(client_dataset[[i]])))
    c <- ggplot(data = client_dataset, mapping = aes_string(x = i)) + geom_histogram(bins = bins_num) +
      ggtitle(i) + facet_wrap(~ CLIENT_REGION, scales = "free")
  }
  grid.arrange(c, ncol=1)
}
dev.off()

categories <- setdiff(names(client_dataset), c("CLIENT_ID", "GENDER", "CLIENT_REGION", "reg_gender_strata"))

pdf("reports/client_dataset_histograms_wrt_reg_gender_strata.pdf", onefile=TRUE)
for(i in categories) {
  if(length(unique(client_dataset[[i]])) < 10) {
    c <- ggplot(data = client_dataset, mapping = aes_string(x = i)) + geom_bar(stat="count") +
      ggtitle(i) + facet_wrap(~ reg_gender_strata, scales = "free")
  } else {
    bins_num = ifelse(length(unique(client_dataset[[i]])) > 20, 100, length(unique(client_dataset[[i]])))
    c <- ggplot(data = client_dataset, mapping = aes_string(x = i)) + geom_histogram(bins = bins_num) +
      ggtitle(i) + facet_wrap(~ reg_gender_strata, scales = "free")
  }
  grid.arrange(c, ncol=1)
}
dev.off()


client_dataset_age_ave <- client_dataset %>% group_by(reg_gender_strata, AGE) %>%
  summarise(age_ave_total_amount_paid = median(total_amount_paid))

ggplot(data = client_dataset_age_ave) + geom_line(mapping = aes(x = AGE, y = age_ave_total_amount_paid)) +
  ggtitle("Outcome vs Age") + facet_wrap(~ reg_gender_strata, scales = "free")




female_nominsk <- client_dataset[client_dataset$reg_gender_strata == "F_NoMinsk", ]


kmeans_female_nominsk <- kmeans(female_nominsk, centers = 2, nstart = 2)



clusters <- hclust(dist(client_dataset))
plot(clusters)