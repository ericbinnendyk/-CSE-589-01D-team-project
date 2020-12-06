# to find number of deaths next week

# Note: For this model and all of my previous ones, I used this dataset:
# https://ourworldindata.org/coronavirus-source-data

# Uses Support Vector Regression to predict the number of cases a certain number of deaths in the future
# based on the number of cases in United States, India, France, Brazil, and Russia for the past five weeks.
# In this edition, the log of the number of cases (not the increase) is predicted from the log of the number of cases in those other places

# question (and to-do): does this model perform better than simply predicting the cases from last week?
# if not, maybe try to weight the last week of worldwide cases really highly? Or just go back to predicting increase/decrease?

# to do: add weather/economic data to model

# set this to the directory where you keep the data
setwd("~/Documents/homework/cse589d-01/project/source5")
case.data = read.csv("new_deaths.csv", header=T)

total_cols <- 212;

# Predicting using data from the countries with the most cases.
# How about only using the regions that give highest correlation with the target feature?
relevant.regions = c("World", "United.States", "India", "France", "Brazil", "Russia");

# create dataset for predicting a certain number of weeks ahead
# accumulate data for 39 weeks, from January 27 (row 28 = 27 + 1) to October 25 (row 300 = 27 + 273)
prepare.week.data <- function(case.data) {
  case.data = case.data[relevant.regions];
  week.data = read.table(text = "",
                         col.names = relevant.regions);
  total_week = rep(0, length(relevant.regions));
  i <- 1; # i is number of days since January 26
  while (i <= 273) {
    row <- case.data[i + 27, ];
    row[is.na(row)] <- 0;
    total_week <- data_add(total_week, row)
    if (i %% 7 == 0) {
      # for some reason, some of the "new deaths" values are negative
      # let's just replace them with zero
      total_week[total_week < 0] = 0;
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
      print(r)
      print(i)
      print(log.data[i, r])
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

# Finds the correlations between the worldwide deaths (log scaled) per week and the number of
# deaths in each relevant region n weeks ago, where n ranges from 1 to 5.
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
      if (r == "France") {
        print(log.data[i:(i - 1 + num.instances), r]);
        print(new.data$World);
        print(corrs[i]);
      }
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
print(train_accuracy) # 0.2617766 (1 week),
train_correlation = cor(train.data$World, predict(my.svm, train.data));
# correlation close to 1 is better
print(train_correlation) # 0.9606406 (1 week),
test_accuracy <- rmse(test.data$World, predict(my.svm, test.data));
# smaller accuracy is better
print(test_accuracy) # 1.003322 (1 week)
test_correlation <- cor(test.data$World, predict(my.svm, test.data));
# correlation close to 1 is better
print(test_correlation) # 0.8979761 (1 week)

# Plot actual vs predicted cases
plot(exp(new.data$World), xlab="Weeks since 2020-03-01", ylab="New cases per week")
points(exp(predict(my.svm, new.data)), type = "p", col = "blue")
legend(20, 20000, legend=c("Actual cases", "Predicted cases"), fill=c("black", "blue"))

# Future predictions!
# Predict one week in advance:
currweek = data.frame(0, log.data$United.States[39], log.data$India[39], log.data$France[38], log.data$Brazil[39], log.data$Russia[39], log.data$World[39])
names(currweek) = names(new.data)
exp(predict(my.svm, currweek)) # 35349.21
# Predict two weeks in advance:
currweek = data.frame(0, log.data$United.States[39], log.data$India[39], log.data$France[38], log.data$Brazil[39], log.data$Russia[39], log.data$World[39])
names(currweek) = names(new.data)
exp(predict(my.svm, currweek)) # 33619.25
# Predict three weeks in advance:
currweek = data.frame(0, log.data$United.States[39], log.data$India[39], log.data$France[39], log.data$Brazil[39], log.data$Russia[39], log.data$World[39])
names(currweek) = names(new.data)
exp(predict(my.svm, currweek)) # 33578.8
# Predict four weeks in advance:
currweek = data.frame(0, log.data$United.States[39], log.data$India[35], log.data$France[36], log.data$Brazil[35], log.data$Russia[35], log.data$World[39])
names(currweek) = names(new.data)
exp(predict(my.svm, currweek)) # 38052.79
