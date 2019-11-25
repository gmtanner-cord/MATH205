# Investigation 5.10

# import the data
Talley5K2013 = read.delim("http://www.rossmanchance.com/iscam2/data/Talley5K2013.txt")

# preliminary plot
plot(Talley5K2013$Time~Talley5K2013$Age)

# Oh yeah, there is an outlier that we should remove
# That point had a time of 79.4 minutes, but all other times were less than 60
Talley5K2013_clean = Talley5K2013[Talley5K2013$Time<70,] # keeps only the rows with time < 70

# Plot of clean data
plot(Talley5K2013_clean$Time~Talley5K2013_clean$Age)

# Regression
linearModel = lm(Talley5K2013_clean$Time~Talley5K2013_clean$Age)
summary(linearModel)
abline(linearModel)

# Do you want a confidence interval for the coefficients?
confint(linearModel, level = 0.90)

# optional long form calculations of Regression measures
# This is just to show you how the numbers given by summary(lm(~)) are calculated
sd_reg = sqrt(sum(residuals(linearModel)^2)/(length(Talley5K2013_clean$Time)-2))
SE_slope = sd_reg/sqrt((length(Talley5K2013_clean$Time)-1)*sd(Talley5K2013_clean$Age)^2)
t_slope = coefficients(linearModel)[[2]]/SE_slope

# Checking the Conditions for Inference on Regression.
# These conditions are necessary for the T-distribution to be valid.

# Condition 1 (L): The relationship between the mean of the response (y) and the explanatory is linear
# How to check: Look at the scatterplot. Does it look linear? Look the residuals. Is there aany curvature?
plot(residuals(linearModel)~Talley5K2013_clean$Age)

# Condition 2 (I): The observations are independent.
# How to check: This depends on your sampling and the relationship between observatins.
# One way this would be violated is if you were considering a single observational unit over time.

# Condition 3 (N): The distribution of the response variable at each value of x is normal.
# How to check: Create a histogram of the residuals. Does it look normal?
hist(residuals(linearModel))

# Condition 4 (E): SD(Y at each x) is constant (doesn't depend on x)
# How to check: Look at the residuals. Is the spread similar for all x values.
# This would be violated if the variability increases or decreases with x.
plot(residuals(linearModel)~Talley5K2013_clean$Age)

