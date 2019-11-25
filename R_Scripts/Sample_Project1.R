# This is a sample script for project 1
# The dataset that I will be working with for this sample script is GymMembership.txt
# This dataset has a column for age and a column for gender
# You may modify this script to use your data

# Step 1 import data
# If your data is in Excel
# Use "Import Dataset" -> "From Excel..."
# If your data is in Google sheets, download it as Excel or CSV
# If CSV, use "Import Dataset" -> "From Text (base)" and choose comma as delimiter

# step 2 - Separate groups
maleAge = Gym_Membership$age[which(Gym_Membership$gender == "male")]
femaleAge = Gym_Membership$age[which(Gym_Membership$gender == "female")]
# verify that you get two lists of the appropriate length

# Step 3 - Visualize the data
hist(maleAge)
hist(femaleAge)
# It is a good idea to display these together, just choose a good x and y scale for both
# It is also good to make the graphs look pretty with labels and titles
par(mfrow = c(2,1))
hist(maleAge, breaks = seq(0,90,5), ylim = c(0,30), main = "Histograms of Male and Female Ages for New Gym Memberships", xlab = "Male Age (Years)")
hist(femaleAge, breaks = seq(0,90,5), ylim = c(0,30), main = NA, xlab = "Female Age (Years)")
# Boxplots are nice too 
par(mfrow = c(1,1))
boxplot(Gym_Membership$age ~ Gym_Membership$gender)
# We can make our boxplots pretty too (be careful that names are in alphabetical order)
boxplot(Gym_Membership$age ~ Gym_Membership$gender, horizontal = TRUE, main = "Boxplots of Male and Female Ages for New Gym Memberships", xlab = "Age (Years)", ylab = "Gender", names = c("Female","Male"))
# Numerical summaries are nice to be included in a table in the report
summary(maleAge)
summary(femaleAge)

# Test
# T-test
?t.test
t.test(maleAge,femaleAge,alternative = "two.sided")
# your options for alternative are "two.sided", "less", "greater"
# always use "two.sided" for confidence intervals
# if you are doing a paired test use paired = TRUE

# Randomization Test for Median (skewed data)
# Make sure to change the indices [1:126] and [127:252] in randomsample to match your sample sizes
observed_diff = median(maleAge) - median(femaleAge)
I = 10000
diff = 0
for (i in 1:I) {
  randomsample = sample(Gym_Membership$age)
  diff[i] = median(randomsample[1:126])-median(randomsample[127:252])
}
hist(diff); abline(v = observed_diff, col = 2)
sim_pvalue = sum(diff >= observed_diff)/I
