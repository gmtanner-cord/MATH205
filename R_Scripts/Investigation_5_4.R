# Investigation 5.4 - ANOVA with DisabilityEmployment data

# Setup
DisabilityEmployment = read.delim("http://www.rossmanchance.com/iscam2/data/DisabilityEmployment.txt")
boxplot(DisabilityEmployment$score~DisabilityEmployment$disability) # the format for multiple boxplots is boxplot(response~treatments)

# tapply is very useful for applying a function to different groups
# Use ?tapply to read the help file on tapply
# in general, tapply(values,groups,function_to_apply)
tapply(DisabilityEmployment$score,DisabilityEmployment$disability,summary) 
disabilityMeans = tapply(DisabilityEmployment$score,DisabilityEmployment$disability,mean)
disabilityMeans
disabilitySD = tapply(DisabilityEmployment$score,DisabilityEmployment$disability,sd)
disabilitySD
disabilitySamples = tapply(DisabilityEmployment$score,DisabilityEmployment$disability,length)
disabilitySamples

# (j) overall mean
overallMean = mean(DisabilityEmployment$score)
overallMean

# (k) standard deviation among means
sd(disabilityMeans)
var(disabilityMeans)
sd(disabilityMeans)^2

# (m)
variationBetween = 14*var(disabilityMeans)
variationBetween

# (o) 
variationWithin = sum((disabilitySamples-1)*disabilitySD^2)/sum(disabilitySamples-1)
variationWithin

# (q) # ANOVA F statistic
F = variationBetween/variationWithin
F

# (w) ANovA in R, summary(aov(response~treatments))
summary(aov(DisabilityEmployment$score~DisabilityEmployment$disability))

# (x) Check Conditions
# For normal condition, let's look at histograms
# We can use tapply again for a quick and dirty way of generating all 5 histograms, but they don't look nice
# If we want nice histograms, we should create lists for each group and separate hist commands or use the lattice package with histogram 
tapply(DisabilityEmployment$score, DisabilityEmployment$disability, hist)
library(lattice)
histogram(~DisabilityEmployment$score|DisabilityEmployment$disability)

max(disabilitySD)/min(disabilitySD)

# (z) Ok, there is no z, but now that we found a significant difference what do we do?
# We can compare the different groups using TukeyHSD to get confidence intervals
TukeyHSD(aov(DisabilityEmployment$score~DisabilityEmployment$disability))

# Investigation 5.5
curve(df(x,2,390),from = 0, to = 5)
means = c(24.13,21.91,21.7)
samples = c(120,142,131)
sd = c(2.243,2.627,3.332)
n = 393
overallMean = sum(means*samples)/n
overallMean
between = sum(samples*(means-overallMean)^2)/2
between
within = sum((samples-1)*sd^2)/(393-3)
within
Fstat = between/within
Fstat
pf(Fstat,df1 = 3-1, df2 = 393-3,lower.tail = FALSE)
