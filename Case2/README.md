## Portfolio Sorting Strategy for 5000+ companies

In this project, we investigated portfolio sorting strategy. We then listed one or more hypothesis to test the additional contribution of a factor.

**Step 1:** Prepare data

The dataset consists of 3 datasets:

1. Case3Market.csv:
  - DATEyyyymmdd : Date formed as year month and day. For example, 3July2017 is entered as 20170703. Our sample spans from 19960104 to 20151231.
  - MarketEXRET: Daily market excess returns in percentage unit defined as Rm,t+1 − Rf,t. The excess returns on the market, value- weight returns of all CRSP firms incorporated in the US and listed on the NYSE, AMEX, or NASDAQ that have a CRSP share code of 10 or 11.
  - RF : The Tbill return is the simple daily rate (in percentage) that over the number of trading days in the month compounds to 1-month TBill rate from Ibbotson and Associates Inc.
  - VIX2 : VIX index squared in monthly percentage squared unit
  - RV: realized variance in monthly percentage squared unit
  - VRP : variance risk premium in monthly percentage squared unit
  - SKEW: risk neutral skewness 

2. ”Case3Port.csv”:
  - FIRMRET: Daily returns in percentage unit defined as Ri,t+1. Different columns show for different firms.

3. ”Case3FirmSECID.csv”:
  - SECID: Security ID assigned by Optionmetrics to each security or in-
dex.
  - PERMNO: PERMNO is a unique permanent security identification number assigned by CRSP to each security.
  - TICKER : Ticker Symbol is an alphabetic symbol assigned to each security by an exchange. Tickers can be reused over time.
  - COMNAM : CRSP allocates a 32 character name description field for all securities. Preference is given to the spellings and abbreviations provided in Standard & Poor’s CUSIP Directory. In cases where all name sources provide descriptions in excess of 32 characters, CRSP furnishes its own abbreviations.

*Data Source: CRSP, Fama French Data Library and CBOE website.*

```javascript
Market<-read.csv("Case3Market.csv", header = FALSE, sep=",")
Port<-read.csv("Case3Port.csv", header = FALSE, sep=",")
FirmSECID<-read.csv("Case3FirmSECID.csv", header = TRUE, sep=",")
```
**Step 2:** Organize and merge data

We put the data into a panel. Made sure that different rows represent different days and different columns are listing returns for different firms. We also wanted to make sure that the factors provided here share the same number of rows in the cross-sectional return panel. Lastly, for all the data about to be analyzed, made sure we know how they handle missing values. In our case, we used NaN.

```javascript
## Create a variable DATE of class "Date" 
DATE<-as.Date(as.character(Market$V1), "%Y%m%d")
## Measure the dimension of "Case3Port"
dim(Market)
dim(Port)
dim(FirmSECID)
## columns and rows of the Port data file 
ncol_Port<-ncol(Port) nrow_Port<-nrow(Port)
## Risk free rate and market excess return 
Market_EXERT<-Market$V2
Rf<-Market$V3
## identify companies 
SECID<-FirmSECID$secid 
PERMNO<-FirmSECID$permno 
TICKER<-as.character(FirmSECID$Ticker) 
COMNAM<-FirmSECID$Name
```

**Step 3:** Dropped firms with no observations

Deleted firms with no observations in the whole sample period.

```javascript
## calculate the number of Nan for each firm 
num_Nan<-rep(0, ncol_Port)
for(i in 1:ncol_Port){
  num_Nan[i]<-sum (ifelse(is.nan(Port[, i]), 1, 0))
  }

## Find the firms that have no observations in the whole sample period 
vec_DELETE<-which(num_Nan == nrow_Port)
## Delete the firms that have no observations in the whole sample period 
## TICKER[-c(i)] can return vector TICKER without ith element of TICKER,
## given i is a positive integer and SECID_Clean<-SECID[-c(vec_DELETE)] 
PERMNO_Clean<-PERMNO[-c(vec_DELETE)] 
TICKER_Clean<-TICKER[-c(vec_DELETE)] 
COMNAM_Clean<-COMNAM[-c(vec_DELETE)]

## By assigning the value of NULL, we delete the corresponding variable in the data.frame. 
Port[, vec_DELETE]<-NULL
## output the beginning and the ending date in our sample 
lg<-length(DATE)
DATE[1]
DATE[lg]

## The number of firms that are kept and dropped 
ncol_Port_Clean<-ncol(Port) 
num_Dropped<-ncol_Port-ncol_Port_Clean
## Calculate Firm excess returns 
Firm_RET<-Port-Rf
```
**Step 4:** Calculated numerical moments

For each firm, summarized the excess returns by computing the time-series mean, standard deviation, skewness, kurtosis, minimum, maximum and annualized sharpe ratio. Put the quantile ( 5, 25, 50, 75, 95) of these statistics in one table. Also included IBM in an additional column.

```javascript
library(e1071)
## descriptive statistics 
Firm_Mean<-apply(Firm_RET, 2, mean, na.rm=TRUE) 
Firm_Std<-apply(Firm_RET, 2, sd, na.rm=TRUE) 
Firm_Skew<-apply(Firm_RET, 2, skewness, na.rm=TRUE) 
Firm_Kurt<-apply(Firm_RET, 2, kurtosis, na.rm=TRUE) 
Firm_Min<-apply(Firm_RET, 2, min, na.rm=TRUE) 
Firm_Max<-apply(Firm_RET, 2, max, na.rm=TRUE) 
Firm_Sharpe<-Firm_Mean/Firm_Std*sqrt(252)

## Calculate descriptive statistics, such as mean, standard deviation, and Sharpe-ratio for IBM 
IBM<-Firm_RET[, which(SECID_Clean==106276)] 
IBM_Mean<-mean(IBM, na.rm = TRUE)
IBM_Std<-sd(IBM, na.rm = TRUE) 
IBM_Skew<-skewness(IBM, na.rm = TRUE) 
IBM_Kurt<-kurtosis(IBM, na.rm = TRUE) 
IBM_Min<-min(IBM, na.rm = TRUE) 
IBM_Max<-max(IBM, na.rm = TRUE) 
IBM_Sharpe<-IBM_Mean/IBM_Std*sqrt(252)

## Calculate Quantile for each descriptive statistics 
Quantile_Percent<-c(0.05, 0.25, 0.5, 0.75, 0.95) 
Mean_Quantile<-quantile(Firm_Mean, Quantile_Percent, na.rm=TRUE) 
Std_Quantile<-quantile(Firm_Std, Quantile_Percent, na.rm=TRUE) 
Skew_Quantile<-quantile(Firm_Skew, Quantile_Percent, na.rm=TRUE) 
Kurt_Quantile<-quantile(Firm_Kurt, Quantile_Percent, na.rm=TRUE) 
Min_Quantile<-quantile(Firm_Min, Quantile_Percent, na.rm=TRUE) 
Max_Quantile<-quantile(Firm_Max, Quantile_Percent, na.rm=TRUE) 
Sharpe_Quantile<-quantile(Firm_Sharpe, Quantile_Percent, na.rm=TRUE)

## Construct a table to present the results
Table_2_1<-matrix(data=NA,nrow = 7, ncol = 6)
Table_2_1[1,]<-c(IBM_Mean, Mean_Quantile)
Table_2_1[2,]<-c(IBM_Std,Std_Quantile)
Table_2_1[3,]<-c(IBM_Skew,Skew_Quantile)
Table_2_1[4,]<-c(IBM_Kurt,Kurt_Quantile)
Table_2_1[5,]<-c(IBM_Min,Min_Quantile)
Table_2_1[6,]<-c(IBM_Max,Max_Quantile) Table_2_1[7,]<-c(IBM_Sharpe,Sharpe_Quantile)
rownames(Table_2_1)<-c("Mean", "Std", "Skew", "Kurt", "Min", "Max", "Sharpe-Ratio" colnames(Table_2_1)<-c("IBM","Q5","Q25","Q50","Q75","Q95") as.table(round(Table_2_1,2))
```

Results:

| Moments | IBM | Q5 | Q25 | Q50 | Q75 | Q95 |
| ------- | ------- | ------- | ------- | ------- | ------- | ------- |
| Mean | 0.06 | -0.02 | 0.08 | 0.13 | 0.22 | 0.45 |
| Std | 1.83 | 1.73 | 2.63 | 3.69 | 5.23 | 9.54 |
| Skew | 0.51 | -0.10 | 0.59 | 1.23 | 2.75 | 12.90 |
| Kurt | 7.89 | 3.01 | 7.60 | 13.86 | 33.93 | 326.88 |
| Min | -14.42 | -46.53 | -31.98 | -23.03 | -15.56 | -7.62 |
| Max | 14.05 | 10.87 | 22.31 | 36.67 | 67.06 | 213.45 |
| Sharpe-Ratio | 0.56 | -0.10 | 0.43 | 0.59 | 0.76 | 1.26 |

**Step 5:** Identified the firms

Identified firms whose Sharpe ratio is at the min, 50 and max quantile, re- spectively. Created a table to summarize these Sharpe ratios. In addition, for each of these firms, summarized the sample length, the starting date and the ending date.

```javascript
## Firm identification
TICK_vec<-rep(NaN,4)
TICK_vec[1]<-which(SECID_Clean==106276) 
# IBM 
TICK_vec[2]<-which(Firm_Sharpe==min(Firm_Sharpe, na.rm = TRUE)) 
TICK_vec[3]<-which((Firm_Sharpe==quantile(Firm_Sharpe, c(0.50), na.rm = TRUE)))
TICK_vec[4]<-which(Firm_Sharpe==max(Firm_Sharpe, na.rm = TRUE)) 
 ## Study these firms
Q_Index<-list() # row numbers that have observable return values 
QLength<-integer() # number of days that have observable return values 
QStarting<-character() # starting date
QEnding<-character() # ending date

for (i in 1:length(TICK_vec)){ 
  Q_Index[[i]]<-which(is.nan(Firm_RET[,TICK_vec[i]])== "FALSE") 
  QLength[i]<-length(Q_Index[[i]]) 
  QStarting[i]<-Market$V1[Q_Index[[i]][1]] 
  QEnding[i]<-Market$V1[Q_Index[[i]][QLength[i]]]
}

## Construct table to present previous results 
Table_2_2<-matrix(data=NA,nrow =4, ncol = 4) 
Table_2_2[1,]<-round(Firm_Sharpe[TICK_vec],4) 
Table_2_2[2,]<-QLength
Table_2_2[3,]<-QStarting
Table_2_2[4,]<-QEnding
rownames(Table_2_2)<-c("Sharpe-Ratio", "Length", "Start", "End") 
colnames(Table_2_2)<-c("IBM","Min","Q50","Max") 
as.table(Table_2_2)
```

Results:

| Metrics | IBM | Min | Q50 | Max |
| ----- | ----- | ----- | ----- | ----- |
| Sharpe-Ratio | 0.5595 | -4.0181 | 0.5944 | 5.1617 |
| Length | 5025 | 45 | 5025 | 23 |     
| Start date | 1996-01-04 | 2005-08-12 | 1996-01-04 | 1996-01-04 |
| End date | 2015-12-31 | 2005-10-14 | 2015-12-31 | 1996-02-05 | 

**Step 6:** Visualizations

Daily Excess Returns at 50th Percentile Sharpe Ratio (Percentages):

<img src="https://github.com/Advaitiyer/advaitiyer.github.io/blob/master/assets/images/financial-analytics/Case2_Sharpe50.jpeg?raw=true"/>

Q-Q plot of Daily Excess Returns at 50th Percentile Sharpe Ratio:

<img src="https://github.com/Advaitiyer/advaitiyer.github.io/blob/master/assets/images/financial-analytics/Case2_QQSharpe_50.jpeg?raw=true"/>

**Step 7:** Beta estimations

In our sample, we have about 6000 firms and not all of them share the same length. For simplicity reason, we only use firms whose sample length is 5025 (from 1996-01-04 to 2015-12-31).

```javascript
## calculate the number of Nan for each firm 
for(i in 1:ncol(Firm_RET)){
  Firm_num_Nan[i]<-sum (ifelse(is.nan(Firm_RET[, i]), 1, 0)) 
  }
## Find firms that have incomplete sample length 
Firm_DELETE<-which(Firm_num_Nan>0)
## Drop the firms with incomplete information during the sample period 
SECID_DClean<-SECID_Clean[-c(Firm_DELETE)] 
PERMNO_DClean<-PERMNO_Clean[-c(Firm_DELETE)] 
TICKER_DClean<-TICKER_Clean[-c(Firm_DELETE)] 
COMNAM_DClean<-COMNAM_Clean[-c(Firm_DELETE)]
Firm_DRET<-Firm_RET Firm_DRET[, Firm_DELETE]<-NULL
## Calculate the number of firms after delete firms with incompelete observations 
Num_Firms<-ncol(Firm_DRET)
## The number of firms that are dropped 
Num_Delete<-ncol_Port_Clean-Num_Firms
## The length, beginning date, and ending date nrow(Firm_DRET)
DATE[1]
DATE[nrow(Firm_DRET)]

## Find the the specific dates for the end of each month in the sample period. 
End_Month<-tapply(as.character(DATE), substr(DATE, 1, 7), max)
## Create a variable to store the index for the end of each month 
End_Month_Index<-rep(NA, length(End_Month))
## Find the row number for the end of each month 
for( i in 1:length(End_Month)){
  End_Month_Index[i]<-which(DATE==End_Month[i]) 
  }
```

**Step 8:** OLS Regression

Our goal was to test whether stocks with different sensitivities to the market excess returns have different average returns. This is a one factor model with the market excess returns as the only factor. The empirical model we examined is,

<img src="https://latex.codecogs.com/gif.latex?R_{i,t+1}-R_{f,t}={\alpha_{i}}+{\beta_{i}(R_{m,t+1}-R_{f,t})+{\epsilon_{i,t+1}}}"/>

We chose to use daily data with a three-month moving window (sixty days). In a setting in which coefficients potentially vary over time, a three-month window with daily data is a natural compromise between estimating coef- ficients with a reasonable degree of precision and pinning down conditional coefficients in an environment with time-varying factor loadings. We got hold of a panel of βi where i stands for different firms at the end of each month. βi could be time-varying.

```javascript
## OLS regression firm by firm 
Num_Month<-length(End_Month_Index)
Win<-60
Starting_Month_Index<-3 
Beta_Estimation_3_2<-matrix(NA, nrow = 240, ncol=1111)
## Firm Beta Estimation 
for (i in 1:Num_Firms){
  for(j in Starting_Month_Index:Num_Month){ 
    y<-Firm_DRET[(End_Month_Index[j]-Win+1):End_Month_Index[j], i] 
    x<-Market_EXERT[(End_Month_Index[j]-Win+1):End_Month_Index[j]]
    Model<-lm(y~x)
    Beta_Estimation_3_2[j,i]<-Model$coefficients[2] 
  }
}
```
Result:

March 1996
SECID=100861, Ticker=’AAON’, Permno=76868, COMNAM=’AAON INC’,

| Coefficients | Estimate Std. | Error | t value | Pr > t |
| ----- | ----- | ----- | ----- | ----- |
| (Intercept) | 0.1762 | 0.6942 | 0.254 | 0.8006 |
| x1 | -1.6198 | 0.9099 | -1.780 | 0.0803 |

Residual standard error: 5.348 on 58 degrees of freedom 
Multiple R-squared: 0.05181, Adjusted R-squared: 0.03546 
F-statistic: 3.169 on 1 and 58 DF, p-value: 0.08027

**Step 9:** Portfolio sorting

We have sorted stocks into quantiles (five portfolios with equal number of stocks in each portfolio), based on the value of βi,t estimates over the past three months. Firms in quantile 1 have the lowest βi,t coefficients, while firms in quantile 5 have the highest βi,t loadings. Within each quantile portfolio, we have given each firm an equal weight. We then formed series of post-ranking monthly returns for each quantile portfolio. Now, we have a panel of portfolio returns with different rows representing different month and each column referring to different quantiles. Be aware that since the portfolio returns are rebalanced every month, one particular stock can be grouped into different quintiles at different point of time.

```javascript
## Construct Portfolios at the end of each month 
Month_RET_4<-matrix(NA, nrow =Num_Month, ncol =ncol(Firm_DRET)) 
Port_RET_Q<-matrix(NA, nrow =Num_Month, ncol =6)
for (j in Starting_Month_Index:(Num_Month-1)){
  for (i in 1:Num_Firms){ Month_RET_4[j,i]<-sum(Firm_DRET[(End_Month_Index[j]+1):End_Month_Index[(j+1)], i, na.rm=TRUE)}
  
  ## use beta quantiles as cutoff points
  cutoff<-quantile(Beta_Estimation_3_2[j,], c(0, 0.2, 0.4, 0.6,0.8,1), na.rm=TRUE)

  ## form portfolios at the end of each month 
  Port_RET_Q[j,1]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>=cutoff[1]) & (Beta_Estimation_3_2[j,]<=cutoff[2]))])
  Port_RET_Q[j,2]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[2]) & (Beta_Estimation_3_2[j,]<=cutoff[3]))])
  Port_RET_Q[j,3]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[3]) & (Beta_Estimation_3_2[j,]<=cutoff[4]))])
  Port_RET_Q[j,4]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[4]) & (Beta_Estimation_3_2[j,]<=cutoff[5]))])
  Port_RET_Q[j,5]<-mean(Month_RET_4[j, which((Beta_Estimation_3_2[j,]>cutoff[5]) & (Beta_Estimation_3_2[j,]<=cutoff[6]))])

  ## Return difference between highest quantile and lowest quantile
  Port_RET_Q[j,6]<-Port_RET_Q[j,5]-Port_RET_Q[j,1] 
}
```

For each portfolio returns, computed the time-series average, time-series standard deviation, p-value for the mean and the Sharpe ratio. Created a Barplot for these five time-series averages.

<img src="https://github.com/Advaitiyer/advaitiyer.github.io/blob/master/assets/images/financial-analytics/Case2_Portfolio_Means.jpeg?raw=true"/>

```javascript
## compute the p-value for time-series mean 
x_4<-rep(1,Num_Month)
lmSUM<-summary(lm(Port_RET_Q~0+x_4)) 
Port_RET_Q_pvalue<-rep(NA,6) 
Port_RET_Q_pvalue[1]<-lmSUM[["Response Y1"]]$coefficients[1,4] 
Port_RET_Q_pvalue[2]<-lmSUM[["Response Y2"]]$coefficients[1,4] 
Port_RET_Q_pvalue[3]<-lmSUM[["Response Y3"]]$coefficients[1,4] 
Port_RET_Q_pvalue[4]<-lmSUM[["Response Y4"]]$coefficients[1,4] 
Port_RET_Q_pvalue[5]<-lmSUM[["Response Y5"]]$coefficients[1,4] 
Port_RET_Q_pvalue[6]<-lmSUM[["Response Y6"]]$coefficients[1,4]

## Table output
Table_4_2<-matrix(data=NA,nrow =4, ncol = 6) 
Table_4_2[1,]<-Port_RET_QMean
Table_4_2[2,]<-Port_RET_Qsd
Table_4_2[3,]<-Port_RET_Q_pvalue 
Table_4_2[4,]<-Port_RET_QSharpe 
rownames(Table_4_2)<-c("Mean","Std","p-value","Sharpe-Ratio") 
colnames(Table_4_2)<-c("Q1","Q2","Q3","Q4","Q5","Q5-Q1") 
as.table(round(Table_4_2,3))
```
Results:

| Metrics | Q1 | Q2 | Q3 | Q4 | Q5 | Q5-Q1 |
| ----- | ----- | ----- | ----- | ----- | ----- | ----- |
| Mean | 3.282 | 2.091 | 1.897 | 1.930 | 2.988 | -0.294 |
| Std | 6.529 | 4.939 | 4.769 | 4.895 | 6.102 | 3.033 |
| p-value | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.137 |
| Sharpe-Ratio | 1.741 | 1.467 | 1.378 | 1.366 | 1.696 | -0.336 |

**Step 10:** Return Differentials

At the end of each month, computed the return differential defined as the 5th quantile return minus the 1st quantile return. In other words, we created the 5-1 returns between the quantile portfolios with the highest and lowest βi,t coefficients. According to CAPM, this spread should be significantly positive. Setup a hypothesis to test for this implication.
H0 : μ5−1 ≤ 0. The return differential’s mean is zero or negative 
HA : μ5−1 > 0. The return differential’s mean is positive

```javascript
# step1: set up null and alternative
# step2: compute t-stats
Test_Value_4_3<-lmSUM[["Response Y6"]]$coefficients[1,3]

# step3: 
decisionRule<-Test_Value_4_3>qt(0.95,(Num_Month-Starting_Month_Index-1)) 

# Step4:
Test_Result_4_3<-ifelse(decisionRule, "reject", "can’t reject")

Result: The null hypothesis could not be rejected!
```

**Step 11:** Market risk premium per month

First, we compute the post-ranking monthly returns for each stock in our sample by taking the simple sum of the daily returns. We then perform a cross sectional regression at the end of each month,

Ri,t+1(k) − Rf,t(k) = λ0,t + λm,tβi,t + εi,t+1(k), where k refers to the number of the days within month t + 1.

```javascript
## Step 5.1 Estimate the Market Risk Permium month by month 
Market_Risk_Slope_5<-rep(NA,(Num_Month)) 
Market_Risk_Intercept_5<-rep(NA,(Num_Month))
for (j in Starting_Month_Index:(Num_Month-1)){
  Model<-lm(Month_RET_4[j,]~Beta_Estimation_3_2[j,])
  Market_Risk_Intercept_5[j]<-Model$coefficients[1]
  Market_Risk_Slope_5[j]<-Model$coefficients[2] 
  }
```

**Step 12:** Is the market positively priced?

We then computed the average price of the market risk μm = E(λm,t). To test whether the market risk is being positively priced, we setup the null and the alternative as following,
H0 : μm ≤ 0. the market risk is not positively priced 
HA : μm > 0. the market risk is positively priced

```javascript
## Step 5.2 Is the market risk postively priced
#step1: set up null and alternative
#Step2: Compute test-statistics 
Test_Value_5_2<-summary(lm(Market_Risk_Slope_5~0+x_4))$coefficients[,3] 

#Step3: decision ruls
decisionRule<-Test_Value_5_2>qt(0.95, (Num_Month-Starting_Month_Index-1)) 
#step4 Conclusion
Test_Result_5_2<-ifelse(decisionRule, "reject", "can’t reject")

Result: The null hypothesis could not be rejected!
```

**Step 13:** Are there any other factors?

Finally, we wanted to test another CAPM implication that whether the inter- cept is zero. We again started by computing the average μ0 = E(λ0,t). To test whether the intercept is zero, we setup the null and the alternative as following,
H0 : μ0 = 0. CAPM holds and no abnormal idiosyncratic returns or other factors matter. 
HA : μ0 ̸= 0. CAPM doesn’t hold. Other factors matter.

```javascript
## Step 5.3 Are there other factors
#step1: Set up null and alternative
#step2: Compute p value 
Test_p_value_5_3<-summary(lm(Market_Risk_Intercept_5~0+x_4))$coefficients[,4] 

#step3: Decision rule
DecisionRule<-Test_p_value_5_3<0.05
#step4: Conclusion
Test_Result_5_3<-ifelse(DecisionRule, "reject", "can’t reject")

Result: The null hypothesis could be rejected!
```
