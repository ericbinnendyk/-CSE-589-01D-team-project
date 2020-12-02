# covid-graph-draw.r
# draws graph of new COVID-19 cases per day from October 25 as well as my model's
# prediction

# change the next line to wherever you keep the data files
setwd("~/Documents/homework/cse589d-01/project/source5")
case.data = read.csv("new_cases.csv", header=T)

actual.cases = case.data$World[301:328]

week1.pred = 2212875;
week2.pred = 1944382;
week3.pred = 1937834;
week4.pred = 2195903;
predicted.cases = c(rep(week1.pred/7, 7), rep(week2.pred/7, 7), rep(week3.pred/7, 7), rep(week4.pred/7, 7));

yrange = range(c(actual.cases, predicted.cases))
plot(actual.cases, type="n", xlab="Days since 2020-10-25", ylab="New cases per day");
lines(actual.cases, type = "l", col = "red");
lines(predicted.cases, type = "l", col = "red");
