# downloading packages

# if(!require(xgboost)) install.packages("xgboost")
# require(xgboost)

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type="source")
require(xgboost)

# setting directory

#setwd("C:/Users/mluczko001/Desktop/SER - 19/xgboost")

# loading data (https://archive.ics.uci.edu/ml/datasets/Adult)

data_train <- read.csv("./data/adult_train.csv", header = TRUE, sep = ",", dec = ".")
data_test <- read.csv("./data/adult_test.csv", header = TRUE, sep = ",", dec = ".")

str(data_train)
str(data_test)

# cleaning data

ind <- c(2,4,6:10,14:15)
for(i in ind){
  levels(data_train[,i])[levels(data_train[,i])==" ?"] <- " Unknown"
  levels(data_test[,i])[levels(data_test[,i])==" ?"] <- " Unknown"
}

data_train$flag <- as.numeric(data_train$flag) - 1
data_test$flag <- as.numeric(data_test$flag) - 1

# one - hot encoding

# if(!require(Matrix)) install.packages("Matrix")
require(Matrix)

data_train_sp <- sparse.model.matrix(flag~.-1, data = data_train)
y_train <- data_train$flag
data_test_sp <- sparse.model.matrix(flag~.-1, data = data_test)
y_test <- data_test$flag

# training model

par = list(objective = "binary:logistic",
           eval_metric = "logloss",
           eta = 0.8,
           max.depth = 4,
           lambda = 1,
           gamma = 0,
           subsample = 1,
           colsample_bytree = 1,
           nthread = 4)

set.seed(5112013)
model1 <- xgboost(data = data_train_sp, label = y_train, params = par, nrounds = 40)

# testing model

# if(!require(Metrics)) install.packages("Metrics")
require(Metrics)

logLoss(y_test, predict(model1, data_test_sp))

# verifying "importance"

imp <- head(xgb.importance(data_train_sp@Dimnames[[2]], model = model1), 10)
xgb.plot.importance(imp)

# In xgboost, each split tries to find the best feature and splitting point to optimize the objective. We can calculate the gain on each node, and it is the contribution from the selected feature. In the end we look into all the trees, and sum up all the contribution for each feature and treat it as the importance. If the number of features is large, we can also do a clustering on features before we make the plot. 

###########################
# advanced ways of training
###########################

xgb_mat_train <- xgb.DMatrix(data = data_train_sp,
                             label = y_train)
xgb_mat_test <- xgb.DMatrix(data = data_test_sp,
                            label = y_test)

watchlist <- list(xgb_mat_train, xgb_mat_test)
names(watchlist) <- c("xgb_mat_train", "xgb_mat_test")

model2 <- xgb.train(data = xgb_mat_train,
                    params = par,
                    nrounds = 40,
                    watchlist = watchlist,
                    verbose = 1,
                    print.every.n = 1, 
                    early.stop.round = NULL)

# plotting

xgb.plot.tree(feature_names = data_train_sp@Dimnames[[2]], model = model2, n_first_tree = 1)
xgb.plot.tree(feature_names = data_train_sp@Dimnames[[2]], model = model2, n_first_tree = 5)

xgb.plot.multi.trees(model = model2, feature_names = data_train_sp@Dimnames[[2]], features.keep = 3)

#if line above is not working
# source("plot_multiple_trees.R")
# xgb.plot.multi.trees(model = model2, feature_names = data_train_sp@Dimnames[[2]], features.keep = 3)

# now with cross-validation

model3 <- xgb.cv(data = xgb_mat_train,
                    params = par,
                    nrounds = 40,
                    nfold = 10,
                    stratified = TRUE,
                    showsd = TRUE,
                    verbose = 1,
                    print.every.n = 1, 
                    early.stop.round = NULL)

# tuning hyperparameters - integration with caret

# if(!require(caret)) install.packages("caret")
require(caret)

xgb_grid <- expand.grid(nrounds = c(10,50),
                        max_depth = c(4,6),
                        eta = c(0.01,0.05),
                        gamma = 1,
                        colsample_bytree = 0.8,
                        min_child_weight = 1
)

xgb_train_ctrl <- trainControl(method = "repeatedcv", 
                               number = 5,
                               repeats = 1,
                               search = "grid",
                               classProbs = TRUE,
                               summaryFunction = mnLogLoss,
                               allowParallel = T
                               )

y_train_factor <- as.factor(y_train)
levels(y_train_factor) <- c("No", "Yes")

model4 <- train(x = data_train_sp,
                y = y_train_factor,
                method = "xgbTree",
                trControl = xgb_train_ctrl,
                tuneGrid = xgb_grid,
                metric = "logLoss",
                nthread = 3
)

print(model4$bestTune)







