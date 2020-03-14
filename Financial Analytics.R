#Kshitij Sankesara
#kssankes@syr.edu 
#913789324
#Financial Analytics - Case 1


#Setting the directory using Sessions - Set Working Directory - Choose Directory
#Reading the csv file with header as True and seperated by ,
CAPM <- read.csv("Case1CAPM.csv", header = TRUE, sep = ",")


#Viewing the CAPM file
View(CAPM)


#Dimension of the data file: 5540 instances and 4 variables
dim(CAPM)


#Names of the variables of the data
names(CAPM)


#Class of the Date Variable
class(CAPM$DATE)


#Changing the class of the DATE variable from integer to Date
DATE <- as.Date(as.character(CAPM$DATE), "%Y%m%d")
class(DATE)


#Excess return
ibmRET <- CAPM$IBMRET
marketEXERT <- CAPM$MarketEXRET
RF <- CAPM$RF
IBMEXERT <- ibmRET - RF         #Subtracting ibmRET with RF


#Yearly Excess Return
lg <- length(ibmRET)  
IBMEXERT_Annualized <- rep(NA, lg)
marketEXERT_Annualized <- rep(NA, lg)

#For Loop
for (i in 252:lg){
  IBMEXERT_Annualized[i]<-(prod(IBMEXERT[(i-252+1):(i)]/100+1)-1)*100
  marketEXERT_Annualized[i] <- (prod(marketEXERT[(i-252+1):(i)]/100+1)-1)*100
}


length(IBMEXERT_Annualized)      #To know the number of instances
IBMEXERT_Annualized[1000]        

length(marketEXERT_Annualized)  #To know the number of instances
marketEXERT_Annualized[1000]

mean(IBMEXERT_Annualized[252:lg])    #Mean
mean(marketEXERT_Annualized[252:lg])


#Time-series plot of yearly returns (Market)
#Saving the jpeg file
#Giving Title, name of x and y axis
jpeg(filename = "Case1_marketEXERT_Annualized.jpeg")
plot(DATE[252:lg], marketEXERT_Annualized[252:lg], type = "l",
     col = "blue", xlab="Year", ylab="Percentage",
     main = "Daily Market Excess Return (annualized percentage)", ylim = c(-60, 160))
dev.off()

maximum<-max(marketEXERT_Annualized, na.rm = T)    #Maximum value
maxvalue<-grepl(maximum, marketEXERT_Annualized)
findmax<-which(maxvalue)                    #Gives the index 
DATE[findmax]
marketEXERT_Annualized[findmax]


#Time series plot of yearly return (IBM)
jpeg(filename = "Case1_IBMEXERT_Annualized.jpeg")
plot(DATE[252:lg], IBMEXERT_Annualized[252:lg], type = "l",
     col = "blue", xlab="Year", ylab="Percentage",
     main = "Daily IBM Excess Return (annualized percentage)", ylim = c(-60, 160))
dev.off()

maximum<-max(IBMEXERT_Annualized, na.rm = T)      #Maximum Value
maxvalue<-grepl(maximum, IBMEXERT_Annualized)
findmax<-which(maxvalue)                        #Gives the index 
DATE[findmax]
IBMEXERT_Annualized[findmax]


#Five Year Investment
IBMEXERT_5Year<-rep(NA,lg)
marketEXERT_5Year<-rep(NA,lg)
for (i in (252*5):lg){
  IBMEXERT_5Year[i]<-(prod(IBMEXERT[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100
  marketEXERT_5Year[i]<-(prod(marketEXERT[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100
}


#Time series plot of 5 year Annualized Returns (Market)
jpeg(filename = "Case1_marketEXERT_5Year.jpeg")
plot(DATE[(252*5):lg], marketEXERT_5Year[(252*5):lg], type="l", col="red",
     xlab="Year", ylab="Percentage", main="Daily Market Excess returns (annualized percentage)",
ylim=c(-10,60))
dev.off()


#Time series plot of 5 year Annualized Returns (IBM)
jpeg(filename = "Case1_IBMEXERT_5Year.jpeg")
plot(DATE[(252*5):lg], IBMEXERT_5Year[(252*5):lg], type="l", col="red",
     xlab="Year", ylab="Percentage", main="Daily IBM Excess returns (annualized percentage)",
     ylim=c(-10,60))
dev.off()

#Mean
mean(IBMEXERT_5Year[(252*5):lg])
mean(marketEXERT_5Year[(252*5):lg])


#Boxplot of marketExert and IBMexert
boxplot(marketEXERT, data=CAPM, main="Daily Market Excess Returns (percentage)")
boxplot(IBMEXERT, data=CAPM, main="Daily IBM Excess Returns (percentage)")


#Scatter Plot
#x axis - marketEXERT; y axis- IBMEXERT
#main - Assigns title; xlab - Assigns name to the x axis; ylab - Assigns name to the y axis
plot(marketEXERT, IBMEXERT, main="Scatter Plots of Stock Returns", 
  xlab="Daily Market Excess Return (percentage)", ylab="Daily IBM Excess return (percentage)")


#Installing and Loading e1071 Library
install.packages("e1071")
library(e1071)


#Descriptive Statistics of (marketExert)
MKTmean <- mean(marketEXERT)*252        #Mean
MKTmean
MKTsd <- sd(marketEXERT)*sqrt(252)      #Standard Deviation
MKTsd
MKTskew <- skewness(marketEXERT)        #Skewness
MKTskew
MKTkurto <- kurtosis(marketEXERT)       #Kurtosis
MKTkurto
MKTmin <- min(marketEXERT)              #Minimum value
MKTmin
MKTmax <- max(marketEXERT)              #Maximum value
MKTmax

# Sharpe Ratio
MKTsr<-MKTmean/MKTsd                   #Dividing MKTmean with MKTsd
MKTsr


#Descriptive Statistics (IBMExert)
IBMmean <- mean(IBMEXERT)*252
IBMmean
IBMsd <- sd(IBMEXERT)*sqrt(252)
IBMsd
IBMskew <- skewness(IBMEXERT)
IBMskew
IBMkurto <- kurtosis(IBMEXERT)
IBMkurto
IBMmin <- min(IBMEXERT)
IBMmin
IBMmax <- max(IBMEXERT)
IBMmax

# Sharpe Ratio
IBMsr<-IBMmean/IBMsd
IBMsr


# Value at risk - Lowest 5 Percentile (MarketEXERT)
MKTVaR <- quantile(marketEXERT, probs = c(0.05))

#Expected Shortfall
numES <- lg*0.05

#To get only the Integer of numES
numESInteger <- floor(numES)

#To get the decimal of numES
numESDecimal <- numES-numESInteger

#To sort the data from smallest to largest
datasortMKT <- sort(marketEXERT, decreasing = FALSE)
MKTES <- sum(datasortMKT[1:numESInteger]+datasortMKT[numESInteger+1]*numESDecimal)/numES

# Value at risk - To get the lowest 5 Percentile (IBMMarket)
IBMVaR <- quantile(IBMEXERT, probs = c(0.05))

#Sorting the data from smallest to largest
datasortIBM <- sort(IBMEXERT, decreasing = FALSE)
IBMES <- sum(datasortIBM[1:numESInteger]+datasortIBM[numESInteger+1]*numESDecimal)/numES


#Correlation of IBMEXERT and marketEXERT
IBMcMarket <- cor(IBMEXERT, marketEXERT)
IBMcMarket


#Constructing each column of our table
#Creating a data frame with IBM and Market as the variables and Name as the rows
Name <- c("Mean:", "Std:", "Skewness:", "Kurtosis:","Sharpe Ratio","Value at Risk","Expected Shortfall","Correlation:" )
IBM <- c(IBMmean, IBMsd, IBMskew, IBMkurto, IBMsr, IBMVaR, IBMES, IBMcMarket)
Market <- c(MKTmean, MKTsd, MKTskew, MKTkurto, MKTsr,MKTVaR, MKTES, NA)
data.frame(round(IBM,4), round(Market,4), row.names = Name, check.names = TRUE)


#Histogram - MarketEXERT
jpeg(filename = "Case1_histMarketEXERT.jpeg")
hist(marketEXERT, main = "Daily Market Excess returns (percentage)", prob=TRUE, ylim= c(0, 0.5), breaks = 50)
curve(dnorm (x, mean = mean (marketEXERT), sd= sd (marketEXERT)), add=TRUE)
dev.off()


#Histogram - IBMEXERT
jpeg(filename = "Case1_histIBMEXERT.jpeg")
hist(IBMEXERT, main = "Daily IBM Excess returns (percentage)", prob=TRUE, ylim= c(0, 0.5), breaks = 50)
curve(dnorm (x, mean = mean (IBMEXERT), sd= sd (IBMEXERT)), add=TRUE)
dev.off()


#Q-Q Plot - MarketExert
jpeg(filename = "Case1_QQmarketEXERT.jpeg")
qqnorm(marketEXERT, main="Q-Q plot of Market returns")
qqline(marketEXERT)
dev.off()


#Q-Q Plot - IBMExert
jpeg(filename = "Case1_QQIBMEXERT.jpeg")
qqnorm(IBMEXERT, main="Q-Q plot of IBM returns")
qqline(IBMEXERT)
dev.off()


#Installing and Loading tseries package
install.packages("tseries")
library(tseries)
jarque.bera.test(IBMEXERT)
jarque.bera.test(marketEXERT)


#Installing and loading nortest package
install.packages("nortest")
library(nortest)
lillie.test(IBMEXERT)
lillie.test(marketEXERT)


#Linear Model 
Model<-lm(IBMEXERT~marketEXERT)
summary(Model)

#To get the summary
Model$coefficients
names(summary(Model))
summary(Model)[["coefficients"]]
summary(Model)[["call"]]
summary(Model)[["sigma"]]
summary(Model)[["df"]]
summary(Model)[["r.squared"]]
summary(Model)[["fstatistic"]]


#Scatter Plot
jpeg(filename = "Case1_OLSLINE.jpeg")
plot(marketEXERT, IBMEXERT,
     main="Scatter Plot of IBM Excess returns Vs. Market Excess returns",xlab="Market Excess returns", ylab="IBM Excess returns")
abline(lm(IBMEXERT~marketEXERT), col="blue")
dev.off()



## Step 9.1: Is the adjusted returns zero
#s1: According to the null
testValue<-0
Model<-lm(IBMEXERT~marketEXERT)
#s2: compute test statistics
estimatedcoeff<-Model$coefficients[1]
estimatedstd<-summary(Model)[["coefficients"]][1,2]
tstats<-(estimatedcoeff-testValue)/estimatedstd
#s3: decision rule for two sided test
decisionRule<-abs(tstats)>qt(0.975, length(marketEXERT)-1-1)
#s4: conclusion
Result<-ifelse(decisionRule, "Reject", "Can???t Reject")



## Step 9.2: Is the risk exposure higher than one?
#s1: According to the null
testValue<-1
Model<-lm(IBMEXERT~marketEXERT)
#s2: compute test statistics
estimatedcoeff<-Model$coefficients[2]
estimatedstd<-summary(Model)[["coefficients"]][2,2]
tstats<-(estimatedcoeff-testValue)/estimatedstd
#s3: decision rule for two sided test
decisionRule<-tstats>qt(0.95, length(marketEXERT)-1-1)
#s4: conclusion
Result<-ifelse(decisionRule, "Reject", "Can???t Reject")
