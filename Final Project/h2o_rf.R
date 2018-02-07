library(data.table)

#test <- fread("test_sales.csv")
#test <- as.tibble(test) %>% select(-c(V1,id,full_date,store_nbr,class,cluster)) %>% filter(unit_sales>0)
#target <- test$unit_sales
#perish <- test$perishable
#test <- test %>% select(-unit_sales)
#write_csv(test,"test_clean.csv")

#saveRDS(target,"target.RDS")
#saveRDS(perish,"perish.RDS")

#train <- fread("/Users/brettscroggins/Desktop/train_sales.csv")
#train <- as.tibble(train) %>% filter(unit_sales>0)
#grouped <- train %>% group_by(item_nbr,store_nbr) %>% summarise(
#  avg_sales = mean(unit_sales)
#)
#train2 <- train %>% left_join(.,grouped)
#write.csv(train,"train_clean.csv")


#### Everything in your Desktop.
library(tidyverse)
library(h2o)
h2o.init(nthreads=7,max_mem_size="10G")

train <- h2o.importFile("/Users/brettscroggins/Desktop/train_clean.csv")
test <- h2o.importFile("/Users/brettscroggins/Desktop/test_clean.csv")

perishable  <- readRDS("perish.RDS")
target <- readRDS("target.RDS")

y <- "unit_sales"
x <- setdiff(names(train),c(y,"C1","V1","id","full_date","store_nbr","class","cluster"))

metrics_df <- data.frame(mtries = 12, ntrees = 50)
metrics_df$nwrmsle <- 0

for(i in 1:nrow(metrics_df)){
  num_trees <- metrics_df$ntrees[i]
  var <- metrics_df$mtries[i]
  ptr_statement <- paste0("Working on ",i/nrow(metrics_df)*100, "%", sep=" ")
  print(ptr_statement)
  rf_fit1 <- h2o.randomForest(x = x,
                              y = y,
                              training_frame = train,
                              model_id = "rf_fit1",
                              seed = 1,
                              ntrees = 2,
                              mtries = 11)
  y_pred <- h2o.predict(rf_fit1,newdata=test)
  y_pred_df <- as.data.frame(y_pred)
  y_pred_df$weights = 0.25 * perishable + 1
  y_pred_df$target <- target
  y_check <- y_pred_df %>% filter(predict>0 | target<0)
  val <- sqrt(sum(y_pred_df$weights * ((log(y_pred_df$predict+1) - log(y_pred_df$target+1))^2),na.rm=TRUE) / sum(y_pred_df$weights))
  metrics_df$nwrmsle[i] <- val
}

metrics_df

table = h2o.varimp(rf_fit1)

barplot(table$relative_importance)

xg_fit1 <- h2o.xgboost(x = x,
                       y = y,
                       training_frame = train,
                       ntrees = 100,
                       max_depth = 10,
                       min_rows = 2,
                       learn_rate = 0.1,
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

y_pred2 <- h2o.predict(xg_fit1,newdata=test)
y_pred_df2 <- as.data.frame(y_pred2)
y_pred_df2$weights = 0.25 * perishable + 1
y_pred_df2$target <- target
y_check2 <- y_pred_df2 %>% filter(predict>0 | target<0)
val2 <- sqrt(sum(y_pred_df2$weights * ((log(y_pred_df2$predict+1) - log(y_pred_df2$target+1))^2),na.rm=TRUE) / sum(y_pred_df2$weights))
metrics_df$nwrmsle2 <- val2
val2

h2o.shutdown()
