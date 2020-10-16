# to find number of cases next week

# set this to the parent of the directory where you keep the data
setwd("Documents/homework/cse589d-01/project")
case.data = read.csv("source5/new_cases.csv", header=T)

total_rows <- 278;
total_cols <- 212;

# pseudocode for now
# find weekly cases from dataset
def divide_up_by_weeks(case.data):
  new.data = data.frame();
  total_week = [0] * total_rows;
  i <- 0;
  while i < total_rows:
    row = case.data[i, 2:total_cols];
    total_week = data_add(total_week, row)
    if (i % 7 == 0) {
      new.data[dim(new.data)[1]] <- total_week;
      total_week = [0] * total_rows;
    }
    i += 1;
  return new.data;

def data_add(row1, row2):
  return row1 + row2

week.data <- divide_up_by_weeks(case.data);
# collect data from weeks 10 through 30
x = matrix();
y = c();
i = 10;
while i < 30:
  # extract united states cases for week i, for example
  # the target feature is whether the data increased from last week
  target = week.data[i, 201];
  target_as_boolean = (week.data[i, 201] > week.data[i - 1, 201]);
  # previous week's cases as feature set.
  features = week.data[i - 1, ]
  # increases from yet previous week will be more features
  # probably will want percent increase
  pct_increases = (week.data[i - 1, ] - week.data[i - 2, ])/week.data[i - 2, ];
  # total feature set consists of previous week's cases (for each country) and the increase from the week before that.
  # we will probably want to choose only a selection of the most relevant countries
  data.row = c(features, pct_increases);
  y[i - 9] = target;
  x[i - 9, ] = data.row;

# custom inner product
def kernel(v1, v2):
  total = 0;
  # add absolute number of cases
  total += sum(v1[1:(total_cols - 1)]*v2[1:(total_cols - 1)])/10000 # may want to change 10000?
  # add percent increase
  total += sum(v1[total_cols:(2*total_cols - 2)]*v2[total_cols:(2*total_cols - 2)])

dat = data.frame(x, y = as.factor(y))
svm(y ~ ., data = dat, kernel = kernel, cost = 10, scale = FALSE);