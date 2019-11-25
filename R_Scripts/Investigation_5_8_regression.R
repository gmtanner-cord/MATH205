# Investigation 5.8

# Load data
HeightFoot = read.delim("http://www.rossmanchance.com/iscam2/data/HeightFoot.txt")

# Plot data
plot(HeightFoot$height~HeightFoot$foot)

# Correlation
cor(HeightFoot$height,HeightFoot$foot) # r
cor(HeightFoot$height,HeightFoot$foot)^2 # r-squared

# Regression Model
?lm
lm(HeightFoot$height~HeightFoot$foot)
summary(lm(HeightFoot$height~HeightFoot$foot))
sum(residuals(lm(HeightFoot$height~HeightFoot$foot))^2) # gives you the SSE

# Add regression line to plot. Must first have plot(HeightFoot$height~HeightFoot$foot)
abline(lm(HeightFoot$height~HeightFoot$foot))
