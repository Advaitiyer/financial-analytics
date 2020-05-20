#Case 1: CAPM (Chap9)
#Author: Advait Ramesh Iyer

#Step 0.1 : Prepare (data download, R script, WD)
getwd() # "\\\\hd.ad.syr.edu/03/75b07e/Documents"
setwd("\\\\hd.ad.syr.edu/03/75b07e/Documents/Desktop/FIN 654/Class 4")
rm(list=ls())
# remove all variables in the workspace
#Step 0.2 : Load the data
CAPM <- read.csv("Case1CAPM.csv", header = TRUE, sep = ",")
#capm <- read.csv("Case1CAPM.csv", header = TRUE, sep = ",")
#Step 0.3 : Dimension and Name of the Variable
dim(CAPM)
names(CAPM)
# 5540 rows(days), 4 variables: Time series data
#Step 0.4 : Read data descriptions 2.3 + view Data
View(CAPM)
#Step 0.5: Change the class of variable DATE to "Date"
class(CAPM$DATE)
DATE <- as.Date(as.character(CAPM$DATE), "%Y%m%d")

#Step 1.1: Create the excess returns of IBM
ibmRET <- CAPM$IBMRET
marketEXRET <- CAPM$MarketEXRET
RF <- CAPM$RF
IBMEXRET <- ibmRET-RF

#Step 1.2 Create yearly excess returns
lg <- length(ibmRET)
IBMEXRET_Annualized <- rep(NA,lg)
marketEXRET_Annualized <- rep(NA,lg)

for (i in 252:lg){
  IBMEXRET_Annualized[i] <- (prod(IBMEXRET[(i-252+1):(i)]/100+1)-1)*100 # Daily excess IBM returns
  marketEXRET_Annualized[i] <- (prod(marketEXRET[(i-252+1):(i)]/100+1)-1)*100 # Daily excess Market Returns
}

IBMEXRET_Annualized[500] #[1] 22.17579

#Step 1.3: Time-Series plot of Yearly returns
jpeg(filename = "Case1_marketEXRET_Annualized.jpeg")
plot(DATE[252:lg], marketEXRET_Annualized[252:lg], type = "l"
     , col="blue", xlab="", ylab=""
     , main = "Daily Market Excess Return (annualized percentage)"
     , ylim = c(-60,160))
dev.off()

#find the global maximum
maximum <- max(marketEXRET_Annualized, na.rm = T)
maxvalue <- grepl(maximum, marketEXRET_Annualized)
findmax <- which(maxvalue)
DATE[findmax] # Date when Market Excess returns was maximum
marketEXRET_Annualized[findmax] # Maximum value of daily Market Excess returns

# Step 1.4: Five-year investment
IBMEXRET_5Year <- rep(NA,lg)
marketEXRET_5Year <- rep(NA,lg)

for(i in (252*5):lg){
  IBMEXRET_5Year[i] <- (prod(IBMEXRET[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100 # Five-year IBM excess returns
  marketEXRET_5Year[i] <- (prod(marketEXRET[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100 # Five-year Market excess returns
}

# Step 1.5: Time-series plot of Five-year annualized returns
jpeg(filename = "Case1_marketEXRET_5Year.jpeg")
plot(DATE[(252*5):lg], marketEXRET_5Year[(252*5):lg], type = "l", col = "blue"
     , xlab = "", ylab = "", main = "Daily market excess returns (annualized percentage)"
     , ylim = c(-10,60))
dev.off()

# Step 1.6: Check your work
mean(IBMEXRET_Annualized[252:lg]) #[1] 18.01655
mean(IBMEXRET_5Year[(252*5):lg]) # [1] 15.07887
mean(marketEXRET_Annualized[252:lg]) # [1] 8.466417
mean(marketEXRET_5Year[(252*5):lg]) # [1] 5.010191

# Step 2: Moments

# Step 2.1: Fig plot: Box plot of daily Market and IBM excess returns
boxplot(marketEXRET, main = "Daily Market Excess returns (Percentage)"
        , ylim = c(-15,15))
boxplot(IBMEXRET, main = "Daily IBM Excess returns (Percentage)"
        , ylim = c(-15,15))

# Step 2.2: Scatter plot of daily market excess return to daily IBM excess returns
plot(x=marketEXRET,y=IBMEXRET, main = "Scatter Plots of Stock Returns"
     , xlab = "Daily Market Excess return (Percentage)"
     , ylab = "Daily IBM Excess return (Percentage)")

# Step 2.3: Numerical moments
install.packages("e1071")
library(e1071)
# Install statistics functions skewness and kurtosis

## Compute descriptive statistics for market excess return in daily percentage.
MKTmean <- mean(marketEXRET)*252 # Mean: [1] 7.892513
MKTsd <- sd(marketEXRET)*sqrt(252) # Standard deviation: [1] 18.83606
MKTskew <- skewness(marketEXRET) # Skewness: [1] -0.1168607
MKTkurto <- kurtosis(marketEXRET) # Kurtosis: [1] 7.639527
MKTmin <- min(marketEXRET) # Minimum: [1] -8.95
MKTmax <- max(marketEXRET) # Maximum: [1] 11.35

# Sharpe ratio
MKTsr <- MKTmean/MKTsd # Sharpe Ratio: [1] 0.4190108

# Value at risk
MKTVaR <- quantile(marketEXRET, probs = c(0.05))# Value at risk: 5% -1.8305

# Expected shortfall
numES <- lg*0.05 # [1] 277
numESInteger <- floor(numES)
numESDecimal <- numES - numESInteger # [1] 277
datasort <- sort(marketEXRET, decreasing = FALSE)
MKTES <- sum(datasort[1:numESInteger]+datasort[numESInteger+1]*numESDecimal)/numES # Expected shortfall: [1] -2.809458

## Computes descriptive statistics for IBM excess return in daily percentage
IBMmean <- mean(IBMEXRET)*252 # Mean: [1] 17.38198
IBMsd <- sd(IBMEXRET)*sqrt(252) # Standard deviation: [1] 28.8362
IBMskew <- skewness(IBMEXRET) # Skewness:[1] 0.580013
IBMkurto <- kurtosis(IBMEXRET) # Kurtosis: [1] 8.060213
IBMmin <- min(IBMEXRET) # Minimum: [1] -14.41947
IBMmax <- max(IBMEXRET) # Maximum: [1] 14.05134

# Sharpe ratio
IBMsr <- IBMmean/IBMsd # Sharpe ratio: [1] 0.6027832

# Value at risk
IBMVaR <- quantile(IBMEXRET, probs = c(0.05)) # Value at Risk: 5%, -2.633047

# Expected shortfall
numES <- lg*0.05
numESInteger <- floor(numES)
numESDecimal <- numES - numESInteger
datasort1 <- sort(IBMEXRET, decreasing = FALSE)
IBMES <- sum(datasort1[1:numESInteger]+datasort1[numESInteger+1]*numESDecimal)/numES # Expected shortfall: [1] -3.980314

# Compute the correlation
IBMcMarket <- cor(IBMEXRET, marketEXRET) # Correlation: [1] 0.5955791

## Construct each column of our table
Name <- c("Mean:", "Std:", "Skewness:","Kurtosis:"
          ,"Sharpe Ratio","Value at Risk","Expected Shortfall","Correlation:")
IBM<-c(IBMmean, IBMsd, IBMskew, IBMkurto, IBMsr, IBMVaR, IBMES, IBMcMarket)
Market<-c(MKTmean, MKTsd, MKTskew, MKTkurto, MKTsr,MKTVaR, MKTES, NA)
## Construct table
data.frame(round(IBM,4), round(Market,4),row.names =Name,check.names = TRUE)

#                        round.IBM..4. round.Market..4.
#Mean:                    17.3820           7.8925
#Std:                     28.8362          18.8361
#Skewness:                 0.5800          -0.1169
#Kurtosis:                 8.0602           7.6395
#Sharpe Ratio              0.6028           0.4190
#Value at Risk            -2.6330          -1.8305
#Expected Shortfall       -3.9803          -2.8095
#Correlation:              0.5956               NA

# Step 3.1
# Histogram of Market excess returns
jpeg(filename = "Case1_histmarketEXERT.jpeg")
hist(marketEXRET, main = "Daily Market Excess Returns (Percentage)"
                         , breaks = 50, prob=TRUE)
curve(dnorm (x, mean = mean (marketEXRET), sd= sd (marketEXRET)), add=TRUE)
dev.off()

# Histogram for IBM excess returns
jpeg(filename = "Case1_histIBMEXRET.jpeg")
hist(IBMEXRET, main = "Daily IBM Excess returns (percentage)", prob=TRUE, ylim= c(0, 0.25), breaks = 50)
curve(dnorm (x, mean = mean (IBMEXRET), sd= sd (IBMEXRET)), add=TRUE)
dev.off()

#Step 3.2
# QQ plot for market excess returns
jpeg(filename = "Case1_QQmarketEXRET.jpeg") #Market QQ plot
qqnorm(marketEXRET, main = "Q-Q plot of Market returns")
qqline(marketEXRET)
dev.off()

# QQ plot for IBM excess returns
jpeg(filename = "Case1_QQIBMEXERT.jpeg") #IBM QQ plot
qqnorm(IBMEXRET, main = "Q-Q plot of IBM returns")
qqline(IBMEXRET)
dev.off()

#Step 3.3: The Jarque-Bera Test
install.packages("tseries")
library(tseries)
jarque.bera.test(IBMEXRET) # X-squared = 15322, df = 2, p-value < 2.2e-16
jarque.bera.test(marketEXRET) # X-squared = 13498, df = 2, p-value < 2.2e-16

#Step 3.4: The Lilliefors Test
install.packages("nortest")
library(nortest)
lillie.test(IBMEXRET) # D = 0.081205, p-value < 2.2e-16
lillie.test(marketEXRET) # D = 0.084228, p-value < 2.2e-16

#Step 5: Model Estimation

Model <- lm(IBMEXRET~marketEXRET)
summary(Model)

#shows the intercept and slope
Model$coefficients
# (Intercept) marketEXRET 
# 0.04041978  0.91177448 

#names of the output
names(summary(Model))
# [1] "call"          "terms"         "residuals"     "coefficients"  "aliased"      
# [6] "sigma"         "df"            "r.squared"     "adj.r.squared" "fstatistic"   
# [11] "cov.unscaled" 

summary(Model[["coefficients"]])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.04042 0.25826 0.47610 0.47610 0.69394 0.91177

#Linear model
summary(Model)[["call"]] # lm(formula = IBMEXRET ~ marketEXRET)

summary(Model)[["sigma"]] # [1] 1.459328

summary(Model)[["df"]]

# R squared value
summary(Model)[["r.squared"]] # [1] 0.3547145

# F-statistic
summary(Model)[["fstatistic"]]
# value    numdf    dendf 
# 3044.248    1.000 5538.000 

mdlsum <- summary(Model)
mdlpred <- predict(Model)
mdlresid <- resid(Model)

class(mdlsum) # [1] "summary.lm"
names(mdlsum)
# [1] "call"          "terms"         "residuals"     "coefficients"  "aliased"      
# [6] "sigma"         "df"            "r.squared"     "adj.r.squared" "fstatistic"   
# [11] "cov.unscaled" 

#Step 8: Plot the OLS line
jpeg(filename = "Case1_OLSLINE.jpeg")
plot(marketEXRET, IBMEXRET
     , main = "Scatter Plot of IBM Excess returns Vs. Market Excess returns"
     , xlab = "Market Excess returns"
     , ylab = "IBM Excess returns")
abline(lm(IBMEXRET~marketEXRET), col = "blue")
dev.off()

#Step 9.1: Is the risk adjusted returns zero?
#s1: According to the null
testValue<-1
Model<-lm(IBMEXRET~marketEXRET)
#s2: compute test statistics
estimatedcoeff<-Model$coefficients[2]
estimatedstd<-summary(Model)[["coefficients"]][2,2]
tstats<-(estimatedcoeff-testValue)/estimatedstd
#s3: decision rule for two sided test
decisionRule<-tstats>qt(0.95, length(marketEXRET)-1-1)
#s4: conclusion
Result<-ifelse(decisionRule, "Reject", "Can't Reject")

Model2<-lm(IBMEXRET~marketEXRET)
tstats2<-(Model2$coefficients[2]-testValue)/summary(Model2)[["coefficients"]][2,2]
Result<-ifelse(tstats2>qt(0.95, length(marketEXRET)),"Reject", "Can't Reject")
# marketEXRET 
# "Can't Reject"



