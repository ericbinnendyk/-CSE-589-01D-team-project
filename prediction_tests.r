# to find number of cases next week

# set this to the parent of the directory where you keep the data
setwd("Documents/homework/cse589d-01/project")
case.data = read.csv("source5/new_cases.csv", header=T)

total_rows <- 278;
total_cols <- 212;

# find weekly cases from dataset
divide_up_by_weeks <- function(case.data) {
  new.data = read.table(text = "",
                        col.names = names(case.data)[2:total_cols]);
  total_week = rep(0, total_cols - 1);
  i <- 1;
  while (i < total_rows) {
    row <- case.data[i, 2:total_cols];
    row[is.na(row)] <- 0;
    total_week <- data_add(total_week, row)
    if (i %% 7 == 0) {
      new.data[dim(new.data)[1] + 1, ] <- total_week;
      total_week = rep(0, total_cols - 1);
    }
    i = i + 1;
  }
  new.data;
}

data_add <- function(row1, row2) {
  row1 + row2;
}

week.data <- divide_up_by_weeks(case.data);
# collect data from weeks 10 through 30
x = matrix(nrow = 20, ncol = 4);
y = c();
i = 10;
while (i < 30) {
  # extract united states cases for week i, for example
  # the target feature is whether the data increased from last week
  target = week.data[i, 201];
  target_as_boolean = (week.data[i, 201] > week.data[i - 1, 201]);
  # previous week's cases as feature set.
  features = c(week.data$Canada[i], week.data$Mexico[i]);
  prev_features = c(week.data$Canada[i - 1], week.data$Mexico[i - 1])
  # increases from yet previous week will be more features
  # probably will want percent increase
  pct_increases = (features - prev_features)/prev_features;
  # total feature set consists of previous week's cases (for each country) and the increase from the week before that.
  # we will probably want to choose only a selection of the most relevant countries
  data.row = c(features, pct_increases);
  y[i - 9] = target_as_boolean;
  x[i - 9, ] = data.row;
  i = i + 1;
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

dat = data.frame(x, y = as.factor(y))
train = sample(20, replace=F, 10);
train.data = dat[train, ]
test.data = dat[-train, ]
library(e1071)
my.svm <- svm(y ~ ., data = train.data, cost = 10, scale = FALSE);
i <- 1;
while (i <= 20) {
  predict(my.svm, dat[i, ]);
  i <- i + 1;
}

