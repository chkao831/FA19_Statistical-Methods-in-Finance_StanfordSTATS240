##################################################
## STATS 240 HW1 Autumn19-20
## Date: Oct 21
## Author: Chih-Hsuan (Carolyn) Kao
##################################################

# 1.6(a) Time Series Plot

#plot formatting
par(mfrow=c(1,1))
d = read.table("Desktop/STATS240/w_logret_3stocks.txt",
               sep="\t",
               header = TRUE,
               fill=FALSE, 
               strip.white=TRUE)
d$Date <- as.Date(d$Date,"%m/%d/%y")

#plot time series data for 3 stocks
Citi = ts(d[,3],start = c(1982, 1), frequency = 52, end = c(2007, 1))
PFE = ts(d[,4],start = c(1982, 1),frequency = 52, end = c(2007, 1))
GM = ts(d[,5],start = c(1982, 1),frequency = 52, end = c(2007, 1))
ts.plot(Citi, PFE, GM, gpars = list(col = c("black", "red", "blue")))
legend(2004, -0.08, legend=c("Citi", "PFE", "GM"),
       col=c("black", "red", "blue"), lty=1:1, cex=0.6)
title('Weekly Log Returns of Pfizer Stock')

# Check iid assumption
Citi = ts(d[,3],start = c(1999, 1), frequency = 52, end = c(2000, 1))
PFE = ts(d[,4],start = c(1999, 1),frequency = 52, end = c(2000, 1))
GM = ts(d[,5],start = c(1999, 1),frequency = 52, end = c(2000, 1))
ts.plot(Citi, PFE, GM, gpars = list(col = c("black", "red", "blue")))
legend(2004, -0.08, legend=c("Citi", "PFE", "GM"),
       col=c("black", "red", "blue"), lty=1:1, cex=0.6)
title('Weekly Log Returns of Pfizer Stock (Year 1999)')
abline(v = 1999.3)

# 1.6(b) Fit Regression to PFE data
Citi = d[,3]
PFE = d[,4]
GM = d[,5]
PFE_mod1 <- lm(PFE ~ I(rownames(d) < 897)) #indicator t<t0 where t0=897
summary(PFE_mod1)
CI_lower <- summary(PFE_mod1)$coefficients[2,1] - 2* summary(PFE_mod1)$coefficients[2,2]
CI_upper <- summary(PFE_mod1)$coefficients[2,1] + 2* summary(PFE_mod1)$coefficients[2,2]
#95% confidence interval for miu1
cat(CI_lower,CI_upper)

PFE_mod2 <- lm(PFE ~ I(rownames(d) >= 897)) #indicator t>=t0
CI_lower <- summary(PFE_mod2)$coefficients[2,1] - 2* summary(PFE_mod2)$coefficients[2,2]
CI_upper <- summary(PFE_mod2)$coefficients[2,1] + 2* summary(PFE_mod2)$coefficients[2,2]
#95% confidence interval for miu2
cat(CI_lower,CI_upper)

# Similarly, for Citi and GM (optional)
citi_mod1 <- lm(Citi ~ I(rownames(d) < 897))
citi_mod2 <- lm(Citi ~ I(rownames(d) >= 897))
GM_mod1 <- lm(GM ~ I(rownames(d) < 897))
GM_mod2 <- lm(d[,5] ~ I(rownames(d) >= 897))


# 1.6(c) Test Null Hypothesis: miu1 = miu2 for Pfizer stock
miu1_estimate = summary(PFE_mod1)$coefficients[2,1]
miu2_estimate = summary(PFE_mod2)$coefficients[2,1]
#standard error
sigma = summary(PFE_mod1)$coefficients[2,2]
n = length(d)
#z score
z <- (miu1_estimate-miu2_estimate)/(sigma/sqrt(n))
alpha = .05 
z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)
cat(z)

