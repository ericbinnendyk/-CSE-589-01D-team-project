# covid-graph-draw.r
# draws graphs of new COVID-19 cases per day from October 25 as well as my model's
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

# 7-day graph
xrange = c(1, 7);
yrange = range(c(actual.cases[1:7], predicted.cases[1:7]));
plot(xrange, yrange, type="n", xlab="Days since 2020-10-25", ylab="New cases per day");
lines(actual.cases[1:7], type = "l");
lines(predicted.cases[1:7], type = "l", col = "red");
legend(1, 5.0e+5, legend=c("Actual cases", "Predicted cases"), fill=c("black", "red"));

# 14-day graph
xrange = c(1, 14);
yrange = range(c(actual.cases[1:14], predicted.cases[1:14]));
plot(xrange, yrange, type="n", xlab="Days since 2020-10-25", ylab="New cases per day");
lines(actual.cases[1:14], type = "l");
lines(predicted.cases[1:14], type = "l", col = "red");
legend(10, 4.0e+5, legend=c("Actual cases", "Predicted cases"), fill=c("black", "red"));

# 28-day graph
xrange = c(1, 28);
yrange = range(c(actual.cases, predicted.cases));
plot(xrange, yrange, type="n", xlab="Days since 2020-10-25", ylab="New cases per day");
lines(actual.cases, type = "l");
lines(predicted.cases, type = "l", col = "red");
legend(3, 6.5e+5, legend=c("Actual cases", "Predicted cases"), fill=c("black", "red"));

# 7-day error: 168788.4
mean(abs(actual.cases[1:7] - predicted.cases[1:7]))

# 14-day error: 219997.7
mean(abs(actual.cases[1:14] - predicted.cases[1:14]))

# 28-day error: 257139.3
mean(abs(actual.cases - predicted.cases))