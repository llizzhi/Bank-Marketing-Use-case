library(dplyr)
library(ggplot2)
library(caret)
library(Rtsne)
library(xgboost)

data_tmp <- read.csv('bank-additional-full.csv', sep = ';')
data <- data_tmp

# data preprocessing
## regroup features
data$y <- ifelse(data$y == 'no', 0 , 1)
data$month <-  factor(data$month, levels = c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))

#data$job <- ifelse(data$job %in% c('entrepreneur', 'management'), 'entrepreneur or management',
#                   ifelse(data$job %in% c('housemaid', 'services'), 'housemaid or services', data$job))

# missing value
data[data == 'unknown'] <- NA
data <- droplevels(data)

# binary variable
binary_vars <- c('default', 'housing', 'loan')
for (col in binary_vars){
  data[, col] <- ifelse(data[, col] == 'yes', 1, 0)
}



#EDA
# Monthly aggregation
summary_by_month <- data %>% group_by(month) %>% summarise(base = n(), 
                                                           subscriptions = sum(y),
                                                           avg.emp.var.rate = mean(emp.var.rate),
                                                           avg.cons.price.idx = mean(cons.price.idx),
                                                           avg.cons.conf.idx = mean(cons.conf.idx), 
                                                           avg.euribor3m = mean(euribor3m),
                                                           avg.nr.employed = mean(nr.employed))
summary_by_month$subscription_rate <- summary_by_month$subscriptions/ summary_by_month$base

# subscriper profiling
## age
data_eda <- data

data_eda$age <- ifelse(data_eda$age < 25, '< 25',
                       ifelse((data_eda$age >= 25) & (data_eda$age < 35), '25-35',
                              ifelse((data_eda$age >= 35) & (data_eda$age < 45), '35-45',
                                     ifelse((data_eda$age >= 45) & (data_eda$age < 55), '45-55',
                                            ifelse((data_eda$age >= 55) & (data_eda$age < 65), '55-65',
                                                   ifelse(data_eda$age >= 65, '>= 65', data_eda$age))))))
data_eda$age <- factor(data_eda$age, level = c('< 25', '25-35', '35-45', '45-55', '55-65', '>= 65'))


#duration
data_eda$duration_tier <- ifelse((data_eda$duration < 30), '< 30s', 
                                 ifelse((data_eda$duration >= 30) & (data_eda$duration < 60), '30-60s',
                                        ifelse((data_eda$duration >= 60) & (data_eda$duration < 120), '1-2min',
                                               ifelse((data_eda$duration >= 120) & (data_eda$duration < 180), '2-3min',
                                                      ifelse((data_eda$duration >= 180) & (data_eda$duration < 300), '3-5min',
                                                             ifelse((data_eda$duration >= 300) & (data_eda$duration < 420), '5-7min',
                                                                    ifelse((data_eda$duration >= 420) & (data_eda$duration < 600), '7-10min',
                                                                           ifelse((data_eda$duration >= 600) & (data_eda$duration < 720), '10-12min',
                                                                                  ifelse((data_eda$duration >= 720) & (data_eda$duration < 900), '12-15min',
                                                                                         ifelse((data_eda$duration >= 900) & (data_eda$duration < 1200), '15-20min',
                                                                                                ifelse((data_eda$duration >= 1200), '>= 20min', data_eda$duration)))))))))))

data_eda$duration_tier <- factor(data_eda$duration_tier, level = c('< 30s', '30-60s', '1-2min', '2-3min', '3-5min', '5-7min', '7-10min', '10-12min', '12-15min', '15-20min', '>= 20min'))


# campaign
data_eda$campaign_tier <- ifelse(data_eda$campaign > 10, '> 10', as.character(data_eda$campaign))
data_eda$campaign_tier <- factor(data_eda$campaign_tier, level = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '> 10'))


#poutcome
data_eda$poutcome_type <- ifelse(data_eda$previous == 0, 'new', 
                                 ifelse(data_eda$poutcome == 'failure', 'contacted_failure',
                                        ifelse(data_eda$poutcome == 'success', 'contacted_success',
                                               ifelse(data_eda$poutcome == 'nonexistent', 'contacted_nonexistent', data_eda$poutcome))))
data_eda$poutcome_type <-  factor(data_eda$poutcome_type, levels = c('new', 'contacted_failure', 'contacted_success', 'contacted_nonexistent'))


# xgboost prediction model
##step1: one-hot-encoding
data_modelling <- data_tmp


## regroup features
data_modelling$y <- ifelse(data_modelling$y == 'no', 0 , 1)

# missing value
data_modelling[data_modelling == 'unknown'] <- NA
data_modelling <- droplevels(data_modelling)


# binary variable
binary_vars <- c('default', 'housing', 'loan')
for (col in binary_vars){
  data_modelling[, col] <- ifelse(data_modelling[, col] == 'yes', 1, 0)
}

# one hot encoding for categorical variable
one_hot_vars <- c('job', 'marital', 'education', 'contact', 'month', 'day_of_week', 'poutcome')

dmy <- dummyVars('~ .', data = data_modelling[, one_hot_vars])
data_modelling <- cbind(data_modelling[, -which(names(data_modelling) %in% one_hot_vars)], data.frame(predict(dmy, newdata = data_modelling)))

# split training set and testing set
testing_prop <- 0.1
testing_idx <- sample(1:nrow(data_modelling),size = round(nrow(data_modelling) * testing_prop),replace = FALSE)

testing <- data_modelling[testing_idx,]
training <- data_modelling[-testing_idx,]


dtrain <- xgb.DMatrix(data = data.matrix(training[,!names(training) %in% c('duration','y')]), label = training$y)
dtest <- xgb.DMatrix(data = data.matrix(testing[,!names(testing) %in% c('duration','y')]), label = testing$y)

watchlist <- list(train = dtrain, test = dtest)




#### use the bset parameters set to train the model (run two parameter set and choose the one with better test auc)
#set.seed(1234)
bst <- xgb.train(data = dtrain, watchlist = watchlist,
                 #eta = 0.1, 
                 #max.depth = 6, 
                 nthread = 4, 
                 nrounds = 1000, 
                 early_stopping_rounds = 100,
                 eval.metric = "auc",
                 objective = "binary:logistic",
                 verbose = TRUE, 
                 print_every_n = 10)

