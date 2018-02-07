

#test <- fread("test_sales.csv")
#test <- as.tibble(test) %>% select(-c(V1,id,full_date,store_nbr,class,cluster)) %>% filter(unit_sales>0)
#target <- test$unit_sales
#perish <- test$perishable
#test <- test %>% select(-unit_sales)
#write_csv(test,"test_clean.csv")
#saveRDS(target,"target.RDS")
#saveRDS(perish,"perish.RDS")

#train <- fread("train_sales.csv")
#train <- as.tibble(train) %>% filter(unit_sales>0)
#write.csv(train,"train_clean.csv")


#### Everything in your Desktop.
library(tidyverse)
target <- readRDS("target.RDS")
perishable <- readRDS("perish.RDS")

library(h2o)
h2o.init(nthreads=7,max_mem_size="10G")

train<- h2o.importFile("train_clean.csv")
nrow(train)
test <- h2o.importFile("test_clean.csv")
nrow(test)


y <- "unit_sales"
x <- setdiff(names(train),c(y,"V1","id","full_date","store_nbr","class","cluster"))

metrics_df <- data.frame(mtries = rep(seq(5,15,by=3),each=3),ntrees = rep(c(25,50,100),4))
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
                              ntrees = num_trees,
                              mtries = var)
  y_pred <- h2o.predict(rf_fit1,newdata=test)
  y_pred_df <- as.data.frame(y_pred)
  y_pred_df$weights = 0.25 * perishable + 1
  y_pred_df$target <- target
  y_check <- y_pred_df %>% filter(predict>0 | target<0)
  val <- sqrt(sum(y_pred_df$weights * ((log(y_pred_df$predict+1) - log(y_pred_df$target+1))^2),na.rm=TRUE) / sum(y_pred_df$weights))
  metrics_df$nwrmsle[i] <- val
}

h2o.shutdown()




#mtries <- 5
#num_trees <- 50
#rf_fit1 <- h2o.randomForest(x = x,y = y,training_frame = train,
#                            model_id = "rf_fit1",seed = 1,
#                            ntrees = num_trees,mtries = mtries)

#y_pred <- h2o.predict(rf_fit1,newdata=test)
#y_pred_df <- as.data.frame(y_pred)
#y_pred_df['weights'] = 0.25 * perishable + 1
#y_pred_df$target <- target
#y_check <- y_pred_df %>% filter(predict>0 | target<0)
#val <- sqrt(sum(y_pred_df$weights * ((log(y_pred_df$predict+1) - log(y_pred_df$target+1))^2),na.rm=TRUE) / sum(y_pred_df$weights))
