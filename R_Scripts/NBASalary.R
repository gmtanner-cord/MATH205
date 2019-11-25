# First read in the NBASalaries data

# (c)
EastSalaries = NBASalaries2017$salary[which(NBASalaries2017$conference=="eastern")]
WestSalaries = NBASalaries2017$salary[which(NBASalaries2017$conference=="western")]
par(mfrow = c(2,1)) # Sets up double dotplots
hist(EastSalaries, breaks = seq(0,36,2), ylim = c(0,80))
hist(WestSalaries, breaks = seq(0,36,2), ylim = c(0,80))
mu_E = mean(EastSalaries)
mu_W = mean(WestSalaries)

# (d)
N_W = length(WestSalaries)
N_E = length(EastSalaries)
sigma_E = sd(EastSalaries)
sigma_W = sd(WestSalaries)

# (f)
westSample = sample(WestSalaries,20)
eastSample = sample(EastSalaries,20)

# (g) If you are having trouble with your plots, it can help to press the broom icon in the plot window to clear previous plots.
par(mfrow = c(2,1))
hist(eastSample, breaks = seq(0,35,5))
hist(westSample, breaks = seq(0,35,5))
m_E = mean(eastSample)
m_W = mean(westSample)
s_E = sd(eastSample)
s_W = sd(westSample)
m_E - m_W

# (l)
SE_E=sigma_E/sqrt(20)
SE_W=sigma_W/sqrt(20)

# (n) Highlight all of the code from I = 10000 to the closing } of the for loop, and press "Run"

I = 10000
westmean = 0; eastmean = 0; westsd = 0; eastsd = 0
for (i in 1:I) {
  westsample1 = sample(WestSalaries, 20)
  eastsample1 = sample(EastSalaries, 20)
  westmean[i]=mean(westsample1)
  eastmean[i]=mean(eastsample1)
  westsd[i] = sd(westsample1)
  eastsd[i] = sd(eastsample1)
}

par(mfrow = c(2,1))
hist(westmean,breaks=seq(0,16))
hist(eastmean,breaks=seq(0,16))
diffmean = eastmean-westmean
par(mfrow = c(1,1))
hist(diffmean)

# (q)
t_diff = ((eastmean-westmean)-(mu_E-mu_W))/sqrt(eastsd^2/20+westsd^2/20)
hist(t_diff,probability = TRUE)
curve(dt(x,df=19),add=TRUE)

# (r) 
t.test(eastSample,westSample)

