# to find number of cases next week

# Note: For this model and all of my previous ones, I used this dataset:
# https://ourworldindata.org/coronavirus-source-data

# Uses Support Vector Regression to predict the number of cases in the next week
# based on the number of cases in United States, China, and India for the past two weeks
# In this edition, the log of the number of cases (not the increase) is predicted from the log of the number of cases in those other places

# question (and to-do): does this model perform better than simply predicting the cases from last week?
# Answer: Apparently about the same, slightly worse.
# if not, maybe try to weight the last week of cases really highly? Or just go back to predicting increase/decrease?
# currently, the model doesn't consider previous worldwide cases as a descriptive feature at all.
# If it works better, I might try a model where the previous worldwide cases are weighted quite highly

# to do: add legend to plot, and dates?
# to do: try predicting more than one week ahead
# to do: add weather/economic data to model
# to do: Add legend on graph and convert week number to actual date

# set this to the directory where you keep the data
setwd("~/Documents/homework/cse589d-01/project/source5")
case.data = read.csv("new_cases.csv", header=T)

total_rows <- 278;
total_cols <- 212;

# Predicting using data from the countries with the most cases.
# How about only using the regions that give highest correlation with the target feature?
relevant.regions = c("World", "United.States", "India", "Brazil", "Russia", "Argentina");

# create dataset for predicting a certain number of weeks ahead
# accumulate data for 39 weeks, from January 27 (row 28 = 27 + 1) to November 22 (row 328 = 27 + 301)
prepare.week.data <- function(case.data) {
  case.data = case.data[relevant.regions];
  week.data = read.table(text = "",
                         col.names = relevant.regions);
  total_week = rep(0, length(relevant.regions));
  i <- 1; # i is number of days since January 20
  while (i <= 301) {
    row <- case.data[i + 27, ];
    row[is.na(row)] <- 0;
    total_week <- data_add(total_week, row)
    if (i %% 7 == 0) {
      week.data[dim(week.data)[1] + 1, ] <- total_week;
      total_week = rep(0, total_cols - 1);
    }
    i = i + 1;
  }
  week.data;
}

prepare.log.data <- function(week.data) {
  # make logarithmic data
  log.data = data.frame(matrix(nrow = dim(week.data)[1], ncol = length(relevant.regions)));
  names(log.data) = relevant.regions;
  for (r in relevant.regions) {
    log.data[r] = log(week.data[r])
    # replace -Inf with repeat of previous value
    for (i in 1:(dim(log.data[r])[1])) {
      if (log.data[i, r] == -Inf) {
        if (i == 1) {
          log.data[i, r] = 0;
        }
        else {
          log.data[i, r] = log.data[i - 1, r];
        }
      }
    }
  }
  
  #data.frame(x, y = as.factor(y));
  #data.frame(x, y);
  log.data;
}

# Finds the correlations between the worldwide cases (log scaled) per week and the number of
# cases in each relevant region n weeks ago, where n ranges from 1 to 5.
# Outputs a dataset where the descriptive feature for each relevant region
# is from the time offset with the maximum correlation to the worldwide data.
find.best.correlations <- function(log.data, num.weeks.ahead) {
  num.instances = dim(log.data)[1] - 4 - num.weeks.ahead;
  new.data = data.frame(matrix(nrow = num.instances, ncol = length(relevant.regions)));
  names(new.data) = relevant.regions;
  new.data$World = log.data$World[(5 + num.weeks.ahead):(dim(log.data)[1])];
  new.data$CurrWorld = log.data$World[5:(4 + num.instances)]
  for (r in relevant.regions[2:length(relevant.regions)]) {
    corrs = c(0, 0, 0, 0, 0);
    for (i in 1:5) {
      corrs[i] = abs(cor(log.data[i:(i - 1 + num.instances), r], new.data$World));
    }
    i = match(max(corrs), corrs);
    # lines for debugging
    print(r);
    print(i);
    print("correlation:");
    print(corrs[i]);
    new.data[r] = log.data[i:(i - 1 + num.instances), r];
    # rename columns into something more descriptive
    new_colname = sprintf("%s (%d weeks ago)", r, (5 - i));
    names(new.data)[names(new.data) == r] = new_colname;
  }
  new.data;
}

data_add <- function(row1, row2) {
  row1 + row2;
}

num.weeks.ahead = 4; # Predict new cases this many weeks ahead; change to 1, 2, or 4

set.seed(50)
week.data = prepare.week.data(case.data);
log.data = prepare.log.data(week.data);
new.data = find.best.correlations(log.data, num.weeks.ahead);

num_instances = dim(new.data)[1];
train = sample(num_instances, replace=F, 25);
train.data = new.data[train, ];
test.data = new.data[-train, ];
library(e1071)
my.svm <- svm(World ~ ., data = train.data);
library(Metrics)
train_accuracy <- rmse(train.data$World, predict(my.svm, train.data));
# smaller accuracy is better
print(train_accuracy) # 0.2343199 (1 week), 0.1674008 (2 week), 0.187108 (4 week)
train_correlation = cor(train.data$World, predict(my.svm, train.data));
# correlation close to 1 is better
print(train_correlation) # 0.9818743 (1 week), 0.9880779 (2 week), 0.9665862 (4 week)
test_accuracy <- rmse(test.data$World, predict(my.svm, test.data));
# smaller accuracy is better
print(test_accuracy) # 0.8051913 (1 week), 0.6700352 (2 week), 0.2281461 (4 week)
test_correlation <- cor(test.data$World, predict(my.svm, test.data));
# correlation close to 1 is better
print(test_correlation) # 0.8737907 (1 week), 0.8560593 (2 week), 0.9705397 (4 week)

# Plot actual vs predicted cases
plot(exp(new.data$World), xlab="Weeks since 2020-02-17", ylab="New cases per week")
points(exp(predict(my.svm, new.data)), type = "p", col = "blue")
legend(5, 4e+6, legend=c("Actual cases", "Predicted cases"), fill=c("black", "blue"))

# Future predictions!
# Predict one week in advance:
#currweek = data.frame(0, log.data$United.States[43], log.data$India[43], log.data$Brazil[43], log.data$Russia[43], log.data$Argentina[43], log.data$World[43])
#names(currweek) = names(new.data)
#exp(predict(my.svm, currweek)) # 2598020
# Predict two weeks in advance:
#currweek = data.frame(0, log.data$United.States[43], log.data$India[43], log.data$Brazil[43], log.data$Russia[43], log.data$Argentina[43], log.data$World[43])
#names(currweek) = names(new.data)
#exp(predict(my.svm, currweek)) # 3041765
# Predict four weeks in advance:
#currweek = data.frame(0, log.data$United.States[39], log.data$India[39], log.data$Brazil[39], log.data$Russia[39], log.data$Argentina[39], log.data$World[43])
#names(currweek) = names(new.data)
#exp(predict(my.svm, currweek)) # 2306293
