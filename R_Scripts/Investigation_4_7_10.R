# MATH 205 - Investigations 4.7 and 4.10

# Investigation 4.7
# Download and import the CloudSeeding.txt
# (c)
library('lattice') # This library allows us a different way to make multiple graphs in the same plot
bwplot(~CloudSeeding$rainfall | CloudSeeding$treatment, layout=c(1,2))
histogram(~CloudSeeding$rainfall | CloudSeeding$treatment, nint = 20, layout=c(1,2))
unseededRainfall = CloudSeeding$rainfall[which(CloudSeeding$treatment == "unseeded")]
seededRainfall = CloudSeeding$rainfall[which(CloudSeeding$treatment == "seeded")]
summary(unseededRainfall)
summary(seededRainfall)

# (e)
t.test(seededRainfall,unseededRainfall,alternative = "greater")

# (g)
observed_diff = median(seededRainfall) - median(unseededRainfall)
I = 10000
diff = 0
for (i in 1:I) {
  randomsample = sample(CloudSeeding$rainfall)
  diff[i] = median(randomsample[1:26])-median(randomsample[27:52])
}
hist(diff); abline(v = observed_diff, col = 2)
sim_pvalue = sum(diff >= observed_diff)/I

# (i)
lnUnseededRainfall = log(unseededRainfall)
lnSeededRainfall = log(seededRainfall)
lnrainfall  = log(CloudSeeding$rainfall)

# (j)
bwplot(~lnrainfall | CloudSeeding$treatment, layout=c(1,2))
histogram(~lnrainfall | CloudSeeding$treatment, nint = 20, layout=c(1,2))

# (l)
t.test(lnSeededRainfall,lnUnseededRainfall,alternative = "greater")
ln_lb = t.test(lnSeededRainfall,lnUnseededRainfall,alternative = "two.sided")$conf.int[1]
ln_ub = t.test(lnSeededRainfall,lnUnseededRainfall,alternative = "two.sided")$conf.int[2]

# (n)
lb = exp(ln_lb)
ub = exp(ln_ub)

# Investigation 4.10
# Download and the shopping99.txt dataset.
# Before importing, note that there are 3 missing values in the file with * in place of values.
# To import, use "From Test (base)" and make sure to set na.string to *
par(mfrow = c(3,1))
hist(shopping99$Luckys, breaks = seq(0,7,0.5))
hist(shopping99$Scolaris, breaks = seq(0,7,0.5))
hist(shopping99$Luckys - shopping99$Scolaris, breaks = seq(-1.5,1,0.25))
# First, they note that the milk prices are for different sizes, so let's remove that row (row #25)
shopping2 = shopping99[-25,]
# Now, let's remove the rows with the missing data
shopping3 = na.omit(shopping2)
# Now let's look at the differences again
shopping3$diff = shopping3$Luckys - shopping3$Scolaris
par(mfrow = c(1,1))
hist(shopping3$diff, breaks = seq(-1,1,0.1))

# (g)
t.test(shopping3$Luckys,shopping3$Scolaris,paired = TRUE,alternative = "less")
t.test(shopping3$diff,alternative = "less")
