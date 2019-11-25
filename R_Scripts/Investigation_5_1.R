# Investigation 5.1 R script updated 2019/11/03

# (b) create a barplot of proportions
judgeSampleSizes = c(354,730,405,226,111,552,597)
actualWomen = c(119,197,118,77,30,149,86)
actualMen = judgeSampleSizes - actualWomen
propWomen = actualWomen/judgeSampleSizes
propMen = 1 - propWomen
venires = matrix(c(propWomen,propMen), nrow = 2, ncol = 7, byrow = TRUE, dimnames = list(c("Women","Men"),paste("Judge ",1:7)))
barplot(venires, legend = T, ylab = "Proportion")

# (j) Compute the chi-squared test statistic
totalWomen = sum(actualWomen)
totalJurors = sum(judgeSampleSizes)
overallPi = totalWomen/totalJurors
expectedWomen = overallPi*judgeSampleSizes
expectedWomen
expectedMen = (1-overallPi)*judgeSampleSizes
expectedMen
chiSquared = sum((actualWomen - expectedWomen)^2/expectedWomen) + sum((actualMen-expectedMen)^2/expectedMen)
chiSquared

# (o) Use R chisq.test and get the residuals
ourData = data.frame(actualWomen,actualMen, row.names = paste("Judge ",1:7))
ourData
ourTest = chisq.test(ourData)
ourTest
ourTest$residuals

# (p) Chi-squared Test without Judge 7
ourData[1:6,]
ourNewTest = chisq.test(ourData[1:6,])
ourNewTest
ourNewTest$residuals

# (q) Check the expected counts - We want at least 5 for expected counts for most (80%) cells. 
ourTest$expected

# (r) For data that is in stacked format, here are some helpful commands:
SpockData <- read.delim("http://www.rossmanchance.com/iscam2/data/SpockData.txt")
table(SpockData)
chisq.test(table(SpockData))
chisq.test(SpockData$EV, SpockData$RV)
