# Statistical Errors and Witch Hunts

# Section 1 - Randomly simulate jury lists of size 100 for 10 judges over 20 years ############
Year = rep(c(paste("Year 0",1:9,sep = ""),paste("Year",10:20)),each = 1000) # sets up repeating pattern of years
Judges = rep(rep(c(paste("Judge 0",1:9,sep=""),"Judge 10"),each = 100),20) #sets up repeating pattern of judges
Gender = ifelse(rbinom(20000,1,0.5),"Men","Women") # randomly assigns each juror to be a man or woman with pi = 0.5
venires = data.frame(Judges, Gender, Year)
venireTable = table(venires)
venireTable

# Section 2 - Let's do pairwise proportions tests to compare the judges within years #############
pvalues = array(0, dim = c(10,10,20)) # set up an empty array to put p-values into
disagreements = array(0, dim = c(10,10)) # set up an empty array to count disagreements between judges
suspicion = rep(FALSE,20) # set up an array for whether a year has a suspicious p-value
comparisons = 0 # counts to number of two-sample proportions tests
for (year in 1:20) {
  for (judge1 in 1:9) {
    for (judge2 in (judge1+1):10) {
      comparisons = comparisons + 1
      pvalues[judge1,judge2,year] = prop.test(venireTable[c(judge1,judge2),1,year],c(100,100),correct = FALSE)$p.value
      if (pvalues[judge1,judge2,year]<0.05) {
        suspicion[year] = TRUE
        disagreements[judge1,judge2] = disagreements[judge1,judge2] + 1
      }
    }
  }
}
round(pvalues,3)
suspicion # This list is TRUE for each year with at least one p-value < 0.05
sum(suspicion) # This is how many years had at least one p-value < 0.05
disagreements # This is how many small p-values there were for each pair of judges
sum(disagreements) # This is the number of tests that had a p-value < 0.05
comparisons # This is the number of tests that were performed
sum(disagreements)/comparisons #This is the proportion of tests that had p-value < 0.05

# Section 3 - Run Chi-squared Test ####
chisquaredResults = array(0,dim = 20)
for (year in 1:20) {
  chisquaredResults[year] = chisq.test(venireTable[,,year])$p.value
}
round(chisquaredResults,3) # These are the p-values for chi-squared tests for each year
rowSums(venireTable,dims=2) # These are each judges totals for the 20 years
chisq.test(rowSums(venireTable,dims=2)) # This is an overall p-value for the chi-squared test after summing over the years.
