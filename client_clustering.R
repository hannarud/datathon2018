Sys.setlocale(category = "LC_CTYPE", locale = "Russian")

library(dplyr)

client_dataset <- readRDS("data/client_dataset_with_categories.rds")

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

categ_names <- names(client_dataset)[grep("[sn]um_Category_", names(client_dataset))]

cols_of_interest <- setdiff(cols_of_interest, categ_names)


for(i in cols_of_interest) {
  print(i)
  
  if(i %in% c("percent_transactions_BYN")) {
    start = log(min(100- client_dataset[[i]]))
    if(is.infinite(start)) {
      start = 0
    }
    seqq = seq(from = start, to = log(max(100 - client_dataset[[i]])), by = 1)
    if(length(seqq < 3)) {
      seqq = seq(from = start, to = log(max(100 - client_dataset[[i]])), length.out = 4)
    }
    if(length(seqq > 6)) {
      seqq = seq(from = start, to = log(max(100 - client_dataset[[i]])), length.out = 5)
    }
    
    client_dataset[[paste0(i, "_factor")]] <- cut(log(100 - client_dataset[[i]]), breaks = seqq)
  } else {
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
}

for(i in categ_names) {
  print(i)
  client_dataset[[paste0(i, "_factor")]] <- cut(log(client_dataset[[i]]), breaks = c(0, 20, 40, 60, 80, 100))
}

write.csv2(client_dataset, "data/client_dataset_discretized.csv", row.names = FALSE)

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
table(client_dataset$IS_RESIDENT) # Можно удалить 0
table(client_dataset$IS_CITIZEN) # Можно удалить 0
table(client_dataset$HAS_CREDIT_CARD) # Можно удалить 1
table(client_dataset$NUMBER_OF_CARDS_1_2_more) # Можно удалить more
table(client_dataset$total_amount_of_transactions_factor) # Можно удалить (5.93,7.91]

client_dataset_filtered <- filter(client_dataset, ! (IS_RESIDENT == 0) &
                                    ! (IS_CITIZEN == 0) &
                                    ! (HAS_CREDIT_CARD == 1) &
                                    ! (NUMBER_OF_CARDS_1_2_more == "more") &
                                    ! (as.character(total_amount_of_transactions_factor) == "(5.93,7.91]"))

trial <- as.data.frame(table(client_dataset$reg_gender_strata,
                             client_dataset$AGE_factor,
                             client_dataset$HAS_FOREIGN_CURRENCY_CARD,
                             client_dataset$NUMBER_OF_CARDS_1_2_more,
                             client_dataset$percent_sum_BYN_factor,
                             client_dataset$percent_transactions_BYN_factor,
                             client_dataset$percent_sum_abroad_factor,
                             client_dataset$percent_transactions_abroad_factor,
                             client_dataset$total_amount_of_transactions_factor,
                             client_dataset$total_amount_paid_factor))

# Теперь категорий слишком много, нужно что-то пообъединять

trial <- as.data.frame(table(client_dataset$reg_gender_strata,
                             client_dataset$AGE_factor,
                             client_dataset$HAS_FOREIGN_CURRENCY_CARD,
                             client_dataset$percent_sum_BYN_factor,
                             client_dataset$percent_transactions_BYN_factor,
                             client_dataset$percent_sum_abroad_factor,
                             client_dataset$percent_transactions_abroad_factor))

trial <- trial[trial$Freq > 100, ]

names(trial) <- c("reg_gender_strata",
                  "AGE_factor",
                  "HAS_FOREIGN_CURRENCY_CARD",
                  "percent_sum_BYN_factor",
                  "percent_transactions_BYN_factor",
                  "percent_sum_abroad_factor",
                  "percent_transactions_abroad_factor",
                  "Freq")
 


trial <- as.data.frame(table(client_dataset$HAS_FOREIGN_CURRENCY_CARD,
                             client_dataset$percent_sum_BYN_factor,
                             client_dataset$percent_transactions_BYN_factor,
                             client_dataset$percent_sum_abroad_factor,
                             client_dataset$percent_transactions_abroad_factor))

# Допустим, мы возьмем группу 280 пользователей

# group <- filter(client_dataset, HAS_FOREIGN_CURRENCY_CARD == 0 &
#          as.character(percent_sum_BYN_factor) == "(3.45,4.61]" &
#          as.character(percent_transactions_BYN_factor) == "(3.45,4.61]" &
#          as.character(percent_sum_abroad_factor) == "(3.45,4.61]" &
#          as.character(percent_transactions_abroad_factor) == "(3.45,4.61]")

group <- filter(client_dataset, HAS_FOREIGN_CURRENCY_CARD == 1 &
                  NUMBER_OF_CARDS_1_2_more == "one")


library(ggplot2)
# Barplot

group_summarized <- group %>% select(CLIENT_ID, starts_with("sum_Category_")) %>% select(- ends_with("factor"))

library(tidyr)
group_summarized_one <- group_summarized %>% gather(Category, percentage, sum_Category_Clothes:sum_Category_Transport)

bp<- ggplot(group_summarized_one, aes(x="", y=percentage, fill=Category))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie




library(ggplot2)
# Barplot

all_summarized <- client_dataset %>% select(CLIENT_ID, starts_with("sum_Category_")) %>% select(- ends_with("factor"))

library(tidyr)
all_summarized_one <- all_summarized %>% gather(Category, percentage, sum_Category_Clothes:sum_Category_Transport)

bp<- ggplot(all_summarized_one, aes(x="", y=percentage, fill=Category))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie







p <- ggplot(group_summarized_one, aes(x="", y=percentage, fill=Category)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip() + 
  theme(axis.text.y=element_text(angle=0, hjust=1))
p


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