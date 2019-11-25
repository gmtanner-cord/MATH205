# Investigation 5.7

# read in the data
golfers18 = read.delim("http://www.rossmanchance.com/iscam3/data/golfers18.txt")

# (c) create scatterplots
?plot
plot(golfers18$scoring_average~golfers18$driving_average)
plot(golfers18$scoring_average~golfers18$putting_average)

# for fun
plot(golfers18)

# (j)
?cor
cor(golfers18$birdie_converstion,golfers18$putting_average)
cor(golfers18$money_earned,golfers18$scoring_average)

cor(golfers18) # This gives an error. Why?
cor(golfers18[,2:7])
