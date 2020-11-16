# to find number of cases next week
# Uses Support Vector Regression to predict the number of cases in the next week
# based on the number of cases in United States, China, and India for the past two weeks

# set this to the parent of the directory where you keep the data
setwd("Documents/homework/cse589d-01/project")
case.data = read.csv("source5/new_cases.csv", header=T)

total_rows <- 278;
total_cols <- 212;

# create dataset for predicting a certain number of weeks ahead
# accumulate data for 37 weeks starting from January 19
prepare.data <- function(case.data, num.weeks.ahead) {
  relevant.regions = c("World", "United.States", "China", "India");
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
  x = matrix(nrow = 37 - num.weeks.ahead, ncol = length(relevant.regions));
  y = c();
  i = 1 + num.weeks.ahead;
  # accumulate data for 39 weeks
  while (i <= 37) {
    # increase in world cases will be target feature
    target = week.data[i, 1] - week.data[i - 1, 1];
    # countries with the most cases (in previous week) can be our features
    features = week.data[i - num.weeks.ahead, ];
    y[i - num.weeks.ahead] = target;
    x[i - num.weeks.ahead, ] = as.numeric(log(features));
    i = i + 1;
  }
  
  # remove -Inf from data
  for (i in 1:4) {
    avg = mean(x[, i][x[, i] != -Inf]);
    x[, i][x[, i] == -Inf] = avg;
  }
  
  #data.frame(x, y = as.factor(y));
  data.frame(x, y);
}

data_add <- function(row1, row2) {
  row1 + row2;
}

set.seed(50)
dat = prepare.data(case.data, 2);
num_instances = dim(dat)[1]
train = sample(num_instances, replace=F, 15);
train.data = dat[train, ]
test.data = dat[-train, ]
library(e1071)
my.svm <- svm(y ~ ., data = train.data);
library(Metrics)
train_accuracy <- rmse(train.data$y, predict(my.svm, train.data))
print(train_accuracy) # 38850.5
test_accuracy <- rmse(test.data$y, predict(my.svm, test.data))
print(test_accuracy) # 69034.92
