# to find number of cases next week

# to do: maybe have multiple prediction layers
# i.e. decrease by more than 100%, decrease by about 50%, increase by about 50%, increase by at least 100%
# to do: add weather data, government response (amount of world population
# living under each category of government response)
# to do: some countries may be lagging behind by more or less than a week,
# so you can try to measure the correlation between world cases and the
# cases for different countries at different times. See which correlation is
# the largest.

# set this to the parent of the directory where you keep the data
setwd("Documents/homework/cse589d-01/project")
case.data = read.csv("source5/new_cases.csv", header=T)

total_rows <- 278;
total_cols <- 212;

# create dataset for predicting a certain number of weeks ahead
# accumulate data for 37 weeks starting from January 19
prepare.data <- function(case.data, num.weeks.ahead) {
  relevant.regions = c("World", "United.States", "China", "India", "Indonesia", "Pakistan", "Nigeria", "Brazil", "Bangladesh", "Russia", "Mexico");
  case.data = case.data[relevant.regions];
  week.data = read.table(text = "",
                         col.names = relevant.regions);
  total_week = rep(0, length(relevant.regions));
  i <- 21;
  while (i <= total_rows) {
    row <- case.data[i, relevant.regions];
    row[is.na(row)] <- 0;
    total_week <- data_add(total_week, row)
    if (i %% 7 == 0) {
      week.data[dim(week.data)[1] + 1, ] <- total_week;
      total_week = rep(0, total_cols - 1);
    }
    i = i + 1;
  }
  
  # collect data from weeks 2 through 37
  x = matrix(nrow = 37 - num.weeks.ahead, ncol = 11);
  y = c();
  i = 1 + num.weeks.ahead;
  # accumulate data for 39 weeks
  while (i <= 37) {
    # increase in world cases will be target feature
    target = week.data[i, 1];
    target_as_boolean = (week.data[i, 1] > week.data[i - num.weeks.ahead, 1]);
    # countries with the most cases (in previous week) can be our features
    features = week.data[i - num.weeks.ahead, ];
    y[i - num.weeks.ahead] = target_as_boolean;
    x[i - num.weeks.ahead, ] = as.numeric(features);
    i = i + 1;
  }
  
  data.frame(x, y = as.factor(y));
}

data_add <- function(row1, row2) {
  row1 + row2;
}

# custom inner product
kernel <- function(v1, v2) {
  total = 0;
  # add absolute number of cases
  total = total + sum(v1[1:2]*v2[1:2])/10000 # may want to change 10000?
  # add percent increase
  total = total + sum(v1[3:4]*v2[3:4])
  total
}

set.seed(50)
dat = prepare.data(case.data, 2);
num_instances = dim(dat)[1]
train = sample(num_instances, replace=F, 15);
train.data = dat[train, ]
test.data = dat[-train, ]
library(e1071)
my.svm <- svm(y ~ ., data = train.data, cost = 10, scale = FALSE);
train_accuracy <- mean(train.data$y == predict(my.svm, train.data))
print(train_accuracy) # 1
test_accuracy <- mean(test.data$y == predict(my.svm, test.data))
print(test_accuracy) # 0.8571429 with seed 50
library(caret)
confusionMatrix(predict(my.svm, test.data), test.data$y)
# test accuracy for 1 week:
# 0.8571429
# Confusion matrix for 1 week:
# Prediction FALSE TRUE
# FALSE     0    0
# TRUE      3   18
# test accuracy for 2 weeks:
# 0.9
# Confusion matrix for 2 weeks:
# Reference
# Prediction FALSE TRUE
# FALSE     0    0
# TRUE      2   18