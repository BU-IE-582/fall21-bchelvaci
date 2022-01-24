library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(ranger)
library(gbm)
library(tidyverse)
library(xgboost)
library(openxlsx)
library(MLmetrics)

#train dataset import edildi.
dataset <- fread(file = "train.csv")
dim(dataset)
#keep unique rows. time stamp ve unique id aynı olan iki row aynıdır mantığı kullanıldı. 
dataset <- distinct(dataset, time_stamp,unique_id, .keep_all= TRUE)
colnames(dataset)
str(dataset)

#Aşağıdaki kolonları çıkardım.
dataset[ ,c("businessunit","product_name","brand_name","Level1_Category_Name","Level2_Category_Name",
            "Level3_Category_Name","type") := NULL]

#Bütün blank değerleri na yaptım.
time_stamp <- dataset$time_stamp
dataset <- dataset[,-1] %>% mutate_all(na_if,"")
dataset$time_stamp <- time_stamp
rm(time_stamp)

#Bütün kolonlar için na sayıları.
dataset[, lapply(.SD, function(x) sum(is.na(x)))]
#selling price na ise 0 yap.
dataset[, sellingprice := ifelse(is.na(sellingprice), 0, sellingprice)]

#kolonların classlarını kontrol etme ve değiştirme.
str(dataset)
cols <- c("contentid","brand_id","category_id","Level1_Category_Id","Level2_Category_Id","Level3_Category_Id",
          "user_action","businessunit","product_gender","gender")

#make gender binary.Female ise 1, diğer türlü 0.
dataset[, gender := ifelse(gender == "F", 1, 0)]

#sort dataset by unique_id
dataset <- dataset[order(unique_id)]
gender <- aggregate(dataset, list(dataset$unique_id), FUN=head, 1)
gender <- gender$gender

#nihai tablo oluşturma
dataset_ult <- data.frame(unique_id=numeric(5618),num_of_login=numeric(5618),num_of_basket=numeric(5618),num_of_fav=numeric(5618),
                          num_of_ord=numeric(5618),num_of_search=numeric(5618),num_of_visit=numeric(5618),
                          num_of_gender_woman=numeric(5618),num_of_gender_man=numeric(5618),num_of_gender_unisex=numeric(5618))

dataset_ult <- as.data.table(dataset_ult)
dataset_ult$unique_id <- sort(unique(dataset$unique_id))

#number of visit, search, favorite, basket, order by user
user_action <- dataset %>% count(unique_id, user_action, sort = TRUE)
user_action <- user_action[order(unique_id,user_action)]

bool1 <- user_action$user_action == "basket"
basket <- user_action[bool1,]

for(i in 1:nrow(basket)){
  bask_id = basket$unique_id[i]
  index = which(bask_id == dataset_ult$unique_id)
  dataset_ult[index,3] = basket$n[i]
}

bool2 <- user_action$user_action == "favorite"
fav <- user_action[bool2,] 

for(i in 1:nrow(fav)){
  fav_id = fav$unique_id[i]
  index = which(fav_id == dataset_ult$unique_id)
  dataset_ult[index,4] = fav$n[i]
}

bool3 <- user_action$user_action == "order"
order <- user_action[bool3,]

for(i in 1:nrow(order)){
  order_id = order$unique_id[i]
  index = which(order_id == dataset_ult$unique_id)
  dataset_ult[index,5] = order$n[i]
}

bool4 <- user_action$user_action == "search"
search <- user_action[bool4,]

for(i in 1:nrow(search)){
  search_id = search$unique_id[i]
  index = which(search_id == dataset_ult$unique_id)
  dataset_ult[index,6] = search$n[i]
}

bool5 <- user_action$user_action == "visit"
visit <- user_action[bool5,] 

for(i in 1:nrow(visit)){
  visit_id = visit$unique_id[i]
  index = which(visit_id == dataset_ult$unique_id)
  dataset_ult[index,7] = visit$n[i]
}


#number of log ins by user
total_log  <- dataset %>% count(unique_id, sort = TRUE)
total_log <- total_log[order(unique_id)]
dataset_ult$num_of_login <- total_log$n

#number of log ins by user in work hours (8am-18pm)
dataset$is_inworkhour <- ifelse(hour(dataset$time_stamp)<=18 & hour(dataset$time_stamp)>=8,1,0)  
total_log_workhour <- dataset[, sum(is_inworkhour), by = unique_id]
total_log_workhour <- total_log_workhour[order(unique_id)]
dataset_ult$num_of_login_workhour <- total_log_workhour$V1

total_log_other_hour <- total_log$n-total_log_workhour$V1
dataset_ult$num_of_login_otherhour <- total_log_other_hour

#number of log ins by user at weekdays
days <- weekdays(dataset$time_stamp, abbreviate = TRUE)
dataset$is_weekday <- ifelse(days == "Sat",0,ifelse(days == "Sun",0,1))
total_log_week <- dataset[, sum(is_weekday), by = unique_id]
total_log_week <- total_log_week[order(unique_id)]
dataset_ult$num_of_login_weekday <- total_log_week$V1

total_log_weeknd <- total_log$n-total_log_week$V1
dataset_ult$num_of_login_weekend <- total_log_weeknd

#haftaiçi daha fazla mı girmiş
is_weekday_log_more <- ifelse(total_log_week$V1 > total_log_weeknd,1,0)
dataset_ult$is_weekday_log_more <- is_weekday_log_more

#mesai saatlerinde daha fazla mı girmiş
is_workhour_log_more <- ifelse(total_log_workhour$V1 > total_log_other_hour,1,0)
dataset_ult$is_workhour_log_more <- is_workhour_log_more

#total selling price by user
sell_price_total <- dataset[, sum(sellingprice), by = unique_id]
sell_price_total <- sell_price_total[order(unique_id)]
dataset_ult$sell_price_total <- sell_price_total$V1

#average selling price by user
sell_price_ave <- sell_price_total$V1/total_log$n
dataset_ult$sell_price_ave <- sell_price_ave

#baktığı ürünlerin ortalama fiyatı ortalamanın üzeinde mi
is_more_than_avg_selling <- ifelse(sell_price_ave > (sum(sell_price_ave)/5618),1,0)
dataset_ult$is_more_than_avg_selling <- is_more_than_avg_selling

#Kullanıcının en fazla baktığı brand id
#most_visited_brand <- dataset %>% count(unique_id, brand_id, sort = TRUE)
#most_visited_brand <- most_visited_brand[order(unique_id,-n)]
#most_visited_brand <- aggregate(most_visited_brand, list(most_visited_brand$unique_id), FUN=head, 1)
#most_visited_brand <- most_visited_brand$brand_id
#most_visited_brand  <- ifelse(is.na(most_visited_brand),"Bilinmiyor",most_visited_brand)
#dataset_ult$most_visited_brand_id <- most_visited_brand

#Kullanıcının baktığı unique brand id sayısı
visited_unique_brand_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(brand_id))
dataset_ult$visited_unique_brand_count <- visited_unique_brand_count$`n_distinct(brand_id)`

#Kullanıcının en fazla baktığı product gender kadın mı erkek mi unisex mi
most_visited_gender <- dataset %>% count(unique_id, product_gender, sort = TRUE)
most_visited_gender <- most_visited_gender[order(unique_id,-n)]
most_visited_gender <- aggregate(most_visited_gender, list(most_visited_gender$unique_id), FUN=head, 1)
most_visited_gender <- most_visited_gender$product_gender
most_visited_gender <- ifelse(is.na(most_visited_gender),"Unisex",most_visited_gender)

dataset_ult$most_visited_gender_woman <- ifelse(most_visited_gender == "KadÄ±n",1,0)
dataset_ult$most_visited_gender_man <- ifelse(most_visited_gender == "Erkek",1,0)                                
dataset_ult$most_visited_gender_unisex <- ifelse(most_visited_gender == "Unisex",1,0) 

#Kullanıcının product genderlara bakma sayısı
gender_action <- dataset %>% count(unique_id, product_gender, sort = TRUE)
gender_action <- gender_action[order(unique_id,gender_action)]

bool1 <- gender_action$product_gender == "KadÄ±n"
kadın <- gender_action[bool1,]

for(i in 1:nrow(kadın)){
  kad_id = kadın$unique_id[i]
  index = which(kad_id == dataset_ult$unique_id)
  dataset_ult[index,8] = kadın$n[i]
}

bool2 <- gender_action$product_gender == "Erkek"
erkek <- gender_action[bool2,]

for(i in 1:nrow(erkek)){
  erk_id = erkek$unique_id[i]
  index = which(erk_id == dataset_ult$unique_id)
  dataset_ult[index,9] = erkek$n[i]
}

bool3 <- gender_action$product_gender == "Unisex"
unisex <- gender_action[bool3,]

for(i in 1:nrow(unisex)){
  uni_id = unisex$unique_id[i]
  index = which(uni_id == dataset_ult$unique_id)
  dataset_ult[index,10] = unisex$n[i]
}


#Kullanıcının en fazla baktığı level 1 kategori
#most_visited_level1 <- dataset %>% count(unique_id, Level1_Category_Id, sort = TRUE)
#most_visited_level1 <- most_visited_level1[order(unique_id,-n)]
#most_visited_level1 <- aggregate(most_visited_level1, list(most_visited_level1$unique_id), FUN=head, 1)
#most_visited_level1 <- most_visited_level1$Level1_Category_Id
#most_visited_level1  <- ifelse(is.na(most_visited_level1),"Bilinmiyor",most_visited_level1)
#dataset_ult$most_visited_level1_id <- most_visited_level1

#Kullanıcının baktığı unique kategori 1 sayısı
visited_unique_cat1_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level1_Category_Id))
dataset_ult$visited_unique_cat1_count <- visited_unique_cat1_count$`n_distinct(Level1_Category_Id)`

#Kullanıcının en fazla baktığı level 2 kategori
#most_visited_level2 <- dataset %>% count(unique_id, Level2_Category_Id, sort = TRUE)
#most_visited_level2 <- most_visited_level2[order(unique_id,-n)]
#most_visited_level2 <- aggregate(most_visited_level2, list(most_visited_level2$unique_id), FUN=head, 1)
#most_visited_level2 <- most_visited_level2$Level2_Category_Id
#most_visited_level2  <- ifelse(is.na(most_visited_level2),"Bilinmiyor",most_visited_level2)
#dataset_ult$most_visited_level2_id <- most_visited_level2

#Kullanıcının baktığı unique kategori 2 sayısı
visited_unique_cat2_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level2_Category_Id))
dataset_ult$visited_unique_cat2_count <- visited_unique_cat2_count$`n_distinct(Level2_Category_Id)`

#Kullanıcının en fazla baktığı level 3 kategori
#most_visited_level3 <- dataset %>% count(unique_id, Level3_Category_Id, sort = TRUE)
#most_visited_level3 <- most_visited_level3[order(unique_id,-n)]
#most_visited_level3 <- aggregate(most_visited_level3, list(most_visited_level3$unique_id), FUN=head, 1)
#most_visited_level3 <- most_visited_level3$Level3_Category_Id
#most_visited_level3  <- ifelse(is.na(most_visited_level3),"Bilinmiyor",most_visited_level3)
#dataset_ult$most_visited_level3_id <- most_visited_level3

#Kullanıcının baktığı unique kategori 3 sayısı
visited_unique_cat3_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level3_Category_Id))
dataset_ult$visited_unique_cat3_count <- visited_unique_cat3_count$`n_distinct(Level3_Category_Id)`


dataset_ult$gender <- gender
gend_ind <- which(colnames(dataset_ult) == "gender")
dataset_ult <- dataset_ult[,-1]

#Fitting rf
#aşağıda 10 fold cv with 5 repeats ve bir çeşit stratifed sampling yapılıyor.
n_repeats <- 5
n_folds <- 10
fitControl <- trainControl(method = "repeatedcv",
                           number = n_folds,
                           repeats = n_repeats,
                           classProbs=TRUE,
                           summaryFunction=twoClassSummary)


rf_grid <- expand.grid(mtry = c(4, 6, 8, 10, 12),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1))
set.seed(1)                      
rf_fit <- train(make.names(gender) ~ ., data = dataset_ult, 
                method = "ranger",
                metric='ROC',
                trControl = fitControl, 
                tuneGrid = rf_grid)

rf_fit
rf_fit$bestTune
#mtry splitrule min.node.size
#4      gini             1
plot(rf_fit) 
predict(rf_fit)

#xgboost
#sadece numerik vektörler ile çalışıyormuş.
x_train <- as.matrix(dataset_ult %>% select(-gender))
y_train <- as.factor(dataset_ult$gender)
levels(y_train) <- c("Yes","No")

trControl <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  allowParallel = TRUE)

tuneGridXGB <- expand.grid(
  nrounds=c(400),
  max_depth = c(4, 6, 8),
  eta = c(0.05, 0.1),
  gamma = c(0.01),
  colsample_bytree = c(0.50,0.70, 0.90),
  subsample = c(0.50,0.70, 0.90),
  min_child_weight = c(1))

# train the xgboost learner, bir öncekinde round 350ydi.
set.seed(1)
xgbmod <- train(
  x = x_train,
  y = y_train,
  method = 'xgbTree',
  metric = 'ROC',
  trControl = trControl,
  tuneGrid = tuneGridXGB)

xgbmod$results
xgbmod$bestTune
# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
#400         4 0.05  0.01              0.9                1       0.9


#gbm
gbmGrid <- expand.grid(interaction.depth = c(3, 4, 6, 8), 
                       n.trees = c(500), 
                       shrinkage = seq(0.02, 0.1, 0.02),
                       n.minobsinnode = c(1))
set.seed(1)                        
gbm_fit <- train(make.names(gender) ~ ., data = dataset_ult, 
                  method = "gbm", 
                  trControl = fitControl,
                  metric='ROC',
                  tuneGrid = gbmGrid,
                  verbose=F)

gbm_fit$bestTune
#n.trees interaction.depth shrinkage n.minobsinnode
#500                 8      0.02              1
gbm_fit$results








#test dataset import edildi ve train ile aynı şekilde feature extraction uygulandı.
dataset <- fread(file = "test.csv")
dim(dataset)
#keep unique rows. time stamp ve unique id aynı olan iki row aynıdır mantığı kullanıldı. 
dataset <- distinct(dataset, time_stamp,unique_id, .keep_all= TRUE)
colnames(dataset)

#Aşağıdaki kolonları çıkardım.
dataset[ ,c("businessunit","product_name","brand_name","Level1_Category_Name","Level2_Category_Name",
            "Level3_Category_Name","type") := NULL]

#Bütün blank değerleri na yaptım.
time_stamp <- dataset$time_stamp
dataset <- dataset[,-1] %>% mutate_all(na_if,"")
dataset$time_stamp <- time_stamp
rm(time_stamp)

#Bütün kolonlar için na sayıları.
dataset[, lapply(.SD, function(x) sum(is.na(x)))]
#selling price na ise 0 yap.
dataset[, sellingprice := ifelse(is.na(sellingprice), 0, sellingprice)]

#kolonların classlarını kontrol etme ve değiştirme.
str(dataset)
cols <- c("contentid","brand_id","category_id","Level1_Category_Id","Level2_Category_Id","Level3_Category_Id",
          "user_action","businessunit","product_gender","gender")

#make gender binary.Female ise 1, diğer türlü 0.
dataset[, gender := ifelse(gender == "F", 1, 0)]

#sort dataset by unique_id
dataset <- dataset[order(unique_id)]
gender <- aggregate(dataset, list(dataset$unique_id), FUN=head, 1)
gender <- gender$gender

#nihai tablo oluşturma
dataset_test_ult <- data.frame(unique_id=numeric(2380),num_of_login=numeric(2380),num_of_basket=numeric(2380),num_of_fav=numeric(2380),
                               num_of_ord=numeric(2380),num_of_search=numeric(2380),num_of_visit=numeric(2380),
                               num_of_gender_woman=numeric(2380),num_of_gender_man=numeric(2380),num_of_gender_unisex=numeric(2380))

dataset_test_ult <- as.data.table(dataset_test_ult)
dataset_test_ult$unique_id <- sort(unique(dataset$unique_id))

#number of visit, search, favorite, basket, order by user
user_action <- dataset %>% count(unique_id, user_action, sort = TRUE)
user_action <- user_action[order(unique_id,user_action)]

bool1 <- user_action$user_action == "basket"
basket <- user_action[bool1,]

for(i in 1:nrow(basket)){
  bask_id = basket$unique_id[i]
  index = which(bask_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,3] = basket$n[i]
}

bool2 <- user_action$user_action == "favorite"
fav <- user_action[bool2,] 

for(i in 1:nrow(fav)){
  fav_id = fav$unique_id[i]
  index = which(fav_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,4] = fav$n[i]
}

bool3 <- user_action$user_action == "order"
order <- user_action[bool3,]

for(i in 1:nrow(order)){
  order_id = order$unique_id[i]
  index = which(order_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,5] = order$n[i]
}

bool4 <- user_action$user_action == "search"
search <- user_action[bool4,]

for(i in 1:nrow(search)){
  search_id = search$unique_id[i]
  index = which(search_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,6] = search$n[i]
}

bool5 <- user_action$user_action == "visit"
visit <- user_action[bool5,] 

for(i in 1:nrow(visit)){
  visit_id = visit$unique_id[i]
  index = which(visit_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,7] = visit$n[i]
}


#number of log ins by user
total_log  <- dataset %>% count(unique_id, sort = TRUE)
total_log <- total_log[order(unique_id)]
dataset_test_ult$num_of_login <- total_log$n

#number of log ins by user in work hours (8am-18pm)
dataset$is_inworkhour <- ifelse(hour(dataset$time_stamp)<=18 & hour(dataset$time_stamp)>=8,1,0)  
total_log_workhour <- dataset[, sum(is_inworkhour), by = unique_id]
total_log_workhour <- total_log_workhour[order(unique_id)]
dataset_test_ult$num_of_login_workhour <- total_log_workhour$V1

total_log_other_hour <- total_log$n-total_log_workhour$V1
dataset_test_ult$num_of_login_otherhour <- total_log_other_hour

#number of log ins by user at weekdays
days <- weekdays(dataset$time_stamp, abbreviate = TRUE)
dataset$is_weekday <- ifelse(days == "Sat",0,ifelse(days == "Sun",0,1))
total_log_week <- dataset[, sum(is_weekday), by = unique_id]
total_log_week <- total_log_week[order(unique_id)]
dataset_test_ult$num_of_login_weekday <- total_log_week$V1

total_log_weeknd <- total_log$n-total_log_week$V1
dataset_test_ult$num_of_login_weekend <- total_log_weeknd

#haftaiçi daha fazla mı girmiş
is_weekday_log_more <- ifelse(total_log_week$V1 > total_log_weeknd,1,0)
dataset_test_ult$is_weekday_log_more <- is_weekday_log_more

#mesai saatlerinde daha fazla mı girmiş
is_workhour_log_more <- ifelse(total_log_workhour$V1 > total_log_other_hour,1,0)
dataset_test_ult$is_workhour_log_more <- is_workhour_log_more

#total selling price by user
sell_price_total <- dataset[, sum(sellingprice), by = unique_id]
sell_price_total <- sell_price_total[order(unique_id)]
dataset_test_ult$sell_price_total <- sell_price_total$V1

#average selling price by user
sell_price_ave <- sell_price_total$V1/total_log$n
dataset_test_ult$sell_price_ave <- sell_price_ave

#baktığı ürünlerin ortalama fiyatı ortalamanın üzeinde mi
is_more_than_avg_selling <- ifelse(sell_price_ave > (sum(sell_price_ave)/5618),1,0)
dataset_test_ult$is_more_than_avg_selling <- is_more_than_avg_selling

#Kullanıcının en fazla baktığı brand id
#most_visited_brand <- dataset %>% count(unique_id, brand_id, sort = TRUE)
#most_visited_brand <- most_visited_brand[order(unique_id,-n)]
#most_visited_brand <- aggregate(most_visited_brand, list(most_visited_brand$unique_id), FUN=head, 1)
#most_visited_brand <- most_visited_brand$brand_id
#most_visited_brand  <- ifelse(is.na(most_visited_brand),"Bilinmiyor",most_visited_brand)
#dataset_test_ult$most_visited_brand_id <- most_visited_brand

#Kullanıcının baktığı unique brand id sayısı
visited_unique_brand_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(brand_id))
dataset_test_ult$visited_unique_brand_count <- visited_unique_brand_count$`n_distinct(brand_id)`

#Kullanıcının en fazla baktığı product gender kadın mı erkek mi unisex mi
most_visited_gender <- dataset %>% count(unique_id, product_gender, sort = TRUE)
most_visited_gender <- most_visited_gender[order(unique_id,-n)]
most_visited_gender <- aggregate(most_visited_gender, list(most_visited_gender$unique_id), FUN=head, 1)
most_visited_gender <- most_visited_gender$product_gender
most_visited_gender <- ifelse(is.na(most_visited_gender),"Unisex",most_visited_gender)

dataset_test_ult$most_visited_gender_woman <- ifelse(most_visited_gender == "KadÄ±n",1,0)
dataset_test_ult$most_visited_gender_man <- ifelse(most_visited_gender == "Erkek",1,0)                                
dataset_test_ult$most_visited_gender_unisex <- ifelse(most_visited_gender == "Unisex",1,0) 

#Kullanıcının product genderlara bakma sayısı
gender_action <- dataset %>% count(unique_id, product_gender, sort = TRUE)
gender_action <- gender_action[order(unique_id,gender_action)]

bool1 <- gender_action$product_gender == "KadÄ±n"
kadın <- gender_action[bool1,]

for(i in 1:nrow(kadın)){
  kad_id = kadın$unique_id[i]
  index = which(kad_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,8] = kadın$n[i]
}

bool2 <- gender_action$product_gender == "Erkek"
erkek <- gender_action[bool2,]

for(i in 1:nrow(erkek)){
  erk_id = erkek$unique_id[i]
  index = which(erk_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,9] = erkek$n[i]
}

bool3 <- gender_action$product_gender == "Unisex"
unisex <- gender_action[bool3,]

for(i in 1:nrow(unisex)){
  uni_id = unisex$unique_id[i]
  index = which(uni_id == dataset_test_ult$unique_id)
  dataset_test_ult[index,10] = unisex$n[i]
}


#Kullanıcının en fazla baktığı level 1 kategori
#most_visited_level1 <- dataset %>% count(unique_id, Level1_Category_Id, sort = TRUE)
#most_visited_level1 <- most_visited_level1[order(unique_id,-n)]
#most_visited_level1 <- aggregate(most_visited_level1, list(most_visited_level1$unique_id), FUN=head, 1)
#most_visited_level1 <- most_visited_level1$Level1_Category_Id
#most_visited_level1  <- ifelse(is.na(most_visited_level1),"Bilinmiyor",most_visited_level1)
#dataset_test_ult$most_visited_level1_id <- most_visited_level1

#Kullanıcının baktığı unique kategori 1 sayısı
visited_unique_cat1_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level1_Category_Id))
dataset_test_ult$visited_unique_cat1_count <- visited_unique_cat1_count$`n_distinct(Level1_Category_Id)`

#Kullanıcının en fazla baktığı level 2 kategori
#most_visited_level2 <- dataset %>% count(unique_id, Level2_Category_Id, sort = TRUE)
#most_visited_level2 <- most_visited_level2[order(unique_id,-n)]
#most_visited_level2 <- aggregate(most_visited_level2, list(most_visited_level2$unique_id), FUN=head, 1)
#most_visited_level2 <- most_visited_level2$Level2_Category_Id
#most_visited_level2  <- ifelse(is.na(most_visited_level2),"Bilinmiyor",most_visited_level2)
#dataset_test_ult$most_visited_level2_id <- most_visited_level2

#Kullanıcının baktığı unique kategori 2 sayısı
visited_unique_cat2_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level2_Category_Id))
dataset_test_ult$visited_unique_cat2_count <- visited_unique_cat2_count$`n_distinct(Level2_Category_Id)`

#Kullanıcının en fazla baktığı level 3 kategori
#most_visited_level3 <- dataset %>% count(unique_id, Level3_Category_Id, sort = TRUE)
#most_visited_level3 <- most_visited_level3[order(unique_id,-n)]
#most_visited_level3 <- aggregate(most_visited_level3, list(most_visited_level3$unique_id), FUN=head, 1)
#most_visited_level3 <- most_visited_level3$Level3_Category_Id
#most_visited_level3  <- ifelse(is.na(most_visited_level3),"Bilinmiyor",most_visited_level3)
#dataset_test_ult$most_visited_level3_id <- most_visited_level3

#Kullanıcının baktığı unique kategori 3 sayısı
visited_unique_cat3_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level3_Category_Id))
dataset_test_ult$visited_unique_cat3_count <- visited_unique_cat3_count$`n_distinct(Level3_Category_Id)`

dataset_test_ult <- dataset_test_ult[,-1]

#prediction

#mtry splitrule min.node.size
#4      gini             1
predicted1 <- predict(rf_fit, newdata =  dataset_test_ult, type = "prob", mtry = 4, splitrule = "gini", min.node.size = 1 )
sum(predicted1$X1<0.5)
write.xlsx(toString(paste(round(predicted1$X1,3),sep = ",")),"pred1.xlsx")


# nrounds max_depth  eta    gamma colsample_bytree min_child_weight subsample
#400         4      0.05    0.01          0.9               1         0.9
predicted2 <- predict(xgbmod, newdata =  dataset_test_ult, type = "prob", nrounds = 400, max_depth = 4, eta = 0.05,
                      gamma = 0.01, colsample_bytree = 0.9, min_child_weight = 1, subsample = 0.9)
length(predicted2$No)
sum(predicted2$No<0.5)
write.xlsx(toString(paste(round(predicted2$No,3),sep = ",")),"pred2.xlsx")


#n.trees interaction.depth shrinkage n.minobsinnode
#500                 8      0.02              1
predicted3 <- predict(gbm_fit, newdata =  dataset_test_ult, type = "prob", n.trees = 500, interaction.depth = 8, shrinkage = 0.02,
                      n.minobsinnode = 5 )
write.xlsx(toString(paste(round(predicted3$X1,3),sep = ",")),"pred3.xlsx")