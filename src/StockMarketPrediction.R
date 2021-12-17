#######################################################################################################################
# Script name: StockMarketPrediction.R
# Porpouse   : This script is developed to analyse the stock market for certain security and 
#              provide prediction based on stock price (using time series method)
# Data source: Data source could be off-line (marketPriceHistory.csv) or online SP&500
# R Package usagae:
# quantmod
# binhf
#######################################################################################################################
#Developer         Date              Version                    Reason
#Saeid Rezaei     2021-10-20           0                      Initial Version
#Saeid Rezaei     2021-11-30           1                   Added binhf Module to script
#Saeid Rezaei     2021-12-10           2                   Added ARIMA method to script
#######################################################################################################################

# Start program
print ("Start program - Forcaste Stock Marekt")

print ("STEP 1: Merging data into one file and value missing records")
# If you are using off line market price you would need to execute 
# DataClening.pl (Perl) script to merge files and value the secirities 
# with missing price, The method is to value the missing price by looking into
# Previous price, if this is first row price would be Zero (0)
# Note: I'm running from my local drive. You would need to specify the path 
# if you are running from other location
# Recomandation setup:
# Create subfolder in your local (C) drive call it CHM136
# Create another sub-directory under CHM136 call id StockPriceHist
# Copy all downloaded price .csv files there

system("perl C:/CHM136/DataCleaning.pl")

print ("STEP 2: Analyse data and train data")
print ("STEP 2.1: Install and Load R Packages")
install.packages('quantmod')
install.packages('binhf')
install.packages('tseries')
install.packages('timeSeries')
install.packages('forecast')
install.packages('xts')


library(quantmod)

# Load data into Var.
# Load data from local .csv file into var.
#marketPriceHisotry <- read.csv( "C:/CHM136/StockPriceHist/output/secPriceHistory.csv")
#attach(marketPriceHisotry)

# Since Downloading data is not up-t-date, I used R PACKAGE CALLED quantmod to get realtime stock price
# I'll use that source in my project going forward

print ("STEP 2.2: Get stock price from Yahoo and analyse data")
getSymbols('SPY', src='yahoo')
getSymbols('^GSPC', src='yahoo')
getSymbols('^IBEX', src='yahoo')
getSymbols(c('QQQ'), src='google')
head(GSPC)
tail(GSPC)
head(SPY)
tail(SPY)

# Remove the null values 
QQQ <- QQQ[!(rowSums(is.na(QQQ))),]
SPY <- SPY[!(rowSums(is.na(SPY))),]

GSPC <- GSPC[!(rowSums(is.na(GSPC))),]
IBEX <- IBEX[!(rowSums(is.na(IBEX))),]



# GSPC and SPY are Time sereies data, Let's find the class
class(GSPC)

# Create a vector and put more than one symbol into that
# This VAR will being used to compare more than one symbol
# and analyse the market
basketSymbols <-(c('YELP','AAPL','AMZN'))
getSymbols(basketSymbols, src='yahoo')

# Analyse the Data
summary(YELP)
summary(AAPL)
summary(AMZN)

# Merge all there symbol data into one data frame
basket <- data.frame(as.xts(merge(YELP,AAPL,AMZN)))
# N/A respresents when Symbol does not have have price
head(basket)
tail(basket)

# Draw few charts to do basid analyses
print ("STEP 2.3: Draw few charts and analyse them")

lineChart(GSPC,line.type = 'h',theme = 'white',TA=NULL)

# put the volumn 
lineChart(GSPC,line.type = 'h',theme = 'white')
barChart(GSPC,bar.type = 'hcl',TA=NULL)
candleChart(GSPC,TA=NULL,subset = '2015')
# Fucase on Jan 2017
candleChart(GSPC,TA=NULL,subset = '2017-01')
# Review the price changes from Feb 2017 and backward to 1st day
candleChart(GSPC,TA=NULL,subset = '::2017-02')



candleChart(GSPC,theme = chartTheme('white',up.col='yellow',dn.col='darkred'),
            TA=NULL,subset = '2016-01')

chartSeries(GSPC,type =c("candlesticks"),TA=NULL,subset = '2016-01')


chartSeries(SPY, theme='white')

# Let's find the Symple moving avarage for period of 200
#{{\mathit  {momentum}} \over N+1}={\mathit  {SMA}}_{{\mathit  {today}}}-{\mathit  {SMA}}_{{\mathit  {yesterday}}}


addSMA(n=200)
#Find the 10 period days of rate of change

addROC(n=200)



chartSeries(SPY, theme="white",
            TA="addVo();addBBands();addCCI()", subset='2015')

chartSeries(SPY, theme="white", subset='2015')



chartSeries(SPY, theme=chartTheme('white'), up.col="black",
            dn.col="black")

SPY.EMA.20<- EMA(SPY$SPY.Close, n=20)
SPY.EMA.100<- EMA(SPY$SPY.Close, n=100)
addTA(SPY.EMA.20, on=1, col = "red")

addTA(SPY.EMA.100, on=1, col = "blue")

addTA(SPY.EMA.20 - SPY.EMA.100,col='blue', type='h',legend="20-100 MA")


# get more inside about Moving Average price
# In the below lines I'm going to explain the SMA 
# function that I have used above 
print ("STEP 2.4:Creating Moving Average")
#getSymbols(c('QQQ'), src='google')

#I’ll focus on the Close of the bar (where it closed for the day). Let’s take a quick peek at what we have:
plot(QQQ$QQQ.Close)

#I’ll create a simple function to break down the data and average every price point by x amount of points prior to it. 
#In this case I’ll use a 100 day smoothing period.

period <- 100
price_vector <- QQQ$QQQ.Close
moving_average_vector <- c()
for (ind in seq((period+1),(length(price_vector))) ){
       moving_average_vector <- c(moving_average_vector, mean(price_vector[(ind-period):ind]))
}

par(mfrow=c(2,1))
plot(QQQ$QQQ.Close)
plot(moving_average_vector, type='l', col='red', lwd=3, main = paste('SMA', period))

#The first plot is the raw QQQ daily closing prices and the second plot, is our smoothed version. Keep in mind that the first 100 days of price data 
#can’t be used as that is the minimum data we need to create a 100 period average.
#The issue we have is our new SMA vector contains 2065 entries, while our the QQQ market download, has 2165 entries. 
#This should be easy to understand as it takes 100 entries to calculate an SMA. 
#This is going to make it difficult to overlay our SMA onto the raw market data. 
#One way around this is to buffer our SMA with 100 NA’s.

period <- 100
price_vector <- QQQ$QQQ.Close
moving_average_vector <- c(rep(NA, period))
# moving_average_vector <- c(rep(as.numeric(QQQ$QQQ.Close[period]), period))
for (ind in seq((period+1),(length(price_vector))) ){
       moving_average_vector <- c(moving_average_vector, mean(price_vector[(ind-period):ind]))
}

# pass it back to our time series object
QQQ$QQQ.Close.SMA <- moving_average_vector

plot(QQQ$QQQ.Close)
lines(QQQ$QQQ.Close.SMA, type='l', col='red', lwd=3)

# All above action could be simplified by using TTA package same as below:
chartSeries(QQQ$QQQ.Close, theme="white", TA="addSMA(100)")


# Following the trend with multiple moving avarge
# Looking at multiple moving averages, the 10, 50 & 200 MAs * Detrending market action
getSymbols(c('EWP', 'SPY'), src='google')


#Let’s chart the data using a 50 and 200-period moving average. 
#These are common periods often used as benchmarks to indicate a strengthening or weakening stock.
chartSeries(EWP$EWP.Close, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

chartSeries(SPY, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

#Having two moving averages of different periods removes a lot of the noise. 
#When the fast moving average is above the slow one, the market is moving upwards, 
#and when the fast is below the slow, it is going down. Some traders will look at the 
#crossing of these moving averages to take a directional position


SPY.EMA.50<- EMA(SPY$SPY.Close, n=50, ) 
SPY.EMA.200<- EMA(SPY$SPY.Close, n=200, )  
#SPY.EMA.50 fast change
#SPY.EMA.200 slow change 
addTA(SPY.EMA.50 - SPY.EMA.200,col='blue', type='h',legend="50-200 MA")

chartSeries(SPY$SPY.Close, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")


EWP.EMA.50 <- EMA(EWP$EWP.Close, n=50, ) 
EWP.EMA.200 <- EMA(EWP$EWP.Close, n=200, ) 
addTA(EWP.EMA.50 - EWP.EMA.200, col='blue', type='h',legend="50-200 MA")
chartSeries(EWP$EWP.Close, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")
# everyting below Zero - You should not be long - and keep the Index , Holding
# everything above Zero - You should not be short - and sell the Index , Holding
# Let's look into three avarage moving , I'm adding 10 period

EWP.EMA.10 <- EMA(EWP$EWP.Close, n=10, ) 
EWP.EMA.50 <- EMA(EWP$EWP.Close, n=50, ) 
EWP.EMA.200 <- EMA(EWP$EWP.Close, n=200, ) 
Fast.Diff <- EWP.EMA.10 - EWP.EMA.50
Slow.Diff <- EWP.EMA.50 - EWP.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA")

addTA(Slow.Diff, col='red', type='h',legend="50-200 MA")

chartSeries(SPY, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")


SPY.EMA.10 <- EMA(SPY$SPY.Close, n=10, ) 
SPY.EMA.50 <- EMA(SPY$SPY.Close, n=50, ) 
SPY.EMA.200 <- EMA(SPY$SPY.Close, n=200, ) 
Fast.Diff <- SPY.EMA.10 - SPY.EMA.50
Slow.Diff <- SPY.EMA.50 - SPY.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA")

addTA(Slow.Diff, col='red', type='h',legend="50-200 MA")


#Trading With The Trend

#You can only enter in the direction of the red Slow.Diff indicator, 
#if its above zero you can take long signals, if its below zero, 
#you can take short signals. The Fast.Diff indicator dictates the entries. 
#When the blue line goes from negative to positive, its a long trade (and the slower red Slow.Diff indicator is above zero). 
#Same thing for shorts. This is also referred to as a moving average crossover trading system.

#To run this system, we need to build rules to hunt them down.

#The rules are:

#    if no position: red > 0 and blue-1 < 0 and blue > 0 go long
#    if long: blue < 0 exit long
    
#    if no position: red < 0 and blue-1 > 0 and blue < 0 go short
#    if short: blue > 0 exit short
# New chalange would to find the blue -1 means, meaning lag of blue, Pre. price .
print ("STEP 2.5:Trading With The Trend")

library(binhf)
tail(as.numeric(Fast.Diff))
# return prev. data
tail(shift(v=as.numeric(Fast.Diff), places=1, dir="right")) 

#This allows us to compare the values of two different rows on the same row. 
#We still have our indicator value of today, but we now can compare it with yesterday’s value on the same row. 
#Sure, we could have just easily created a loop and run through each value but by doing it this way we stick to vector comparison in its simplest form.

#Now, let’s translate our trend trading system pseudo code into R code:
#Note: Closing price won't give us best price since compay pays dividend / interest and this price is not accure ah the end of the
# month, Hence I have used Adjusted price.

GSPC.SMA.10 <- SMA(GSPC$GSPC.Adjusted, n=10, ) 
GSPC.SMA.50 <- SMA(GSPC$GSPC.Adjusted, n=50, ) 
GSPC.SMA.200 <- SMA(GSPC$GSPC.Adjusted, n=200, ) 
Fast.Diff <- GSPC.SMA.10 - GSPC.SMA.50
Slow.Diff <- GSPC.SMA.50 - GSPC.SMA.200

# look for long entries
Long_Trades <- ifelse(
Slow.Diff  > 0 &
Fast.Diff  > 0 &
shift(v=as.numeric(Fast.Diff), places=1, dir="right") < 0, GSPC$GSPC.Adjusted, NA)

# look for long exits (same thing but inverse signts)
Short_Trades <- ifelse(
Slow.Diff  < 0 &
Fast.Diff  < 0 &
shift(v=as.numeric(Fast.Diff), places=1, dir="right") > 0, GSPC$GSPC.Adjusted, NA)
plot(GSPC$GSPC.Adjusted)

## Warning in plot.xts(EWP): only the univariate series will be plotted
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)

#Mixture of entry points and that is usually how it works on a trading, bouncing trend. 
#Though we aren’t going to design full trending systems here, a stop-loss exit order is key to any directional 
#trading so you don’t lose everything! Let’s see what it does on trending market:



IBEX.EMA.10 <- EMA(IBEX$IBEX.Adjusted, n=10 ) 
IBEX.EMA.50 <- EMA(IBEX$IBEX.Adjusted, n=50, ) 
IBEX.EMA.200 <- EMA(IBEX$IBEX.Adjusted, n=200, ) 
Fast.Diff <- IBEX.EMA.10 - IBEX.EMA.50
Slow.Diff <- IBEX.EMA.50 - IBEX.EMA.200

# look for long entries
Long_Trades <- ifelse(
  Slow.Diff  > 0 &
    Fast.Diff  > 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") < 0, IBEX$IBEX.Adjusted, NA)

# look for long exits (same thing but inverse signts)
Short_Trades <- ifelse(
  Slow.Diff  < 0 &
    Fast.Diff  < 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") > 0, IBEX$IBEX.Adjusted, NA)

plot(IBEX$IBEX.Adjusted)
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)

print ("STEP 2.6:Volume-based indicators")

library(quantmod)
getSymbols(c('QQQ', 'SPY'), src='google')

# remove any NAs 
QQQ <- QQQ[!(rowSums(is.na(QQQ))),]
SPY <- SPY[!(rowSums(is.na(SPY))),]

library(TTR)

#The ADX is Welles Wilder’s Directional Movement Indicator. It is used by lots of people to determine if the market is trending or range bound.
# Refrence: https://en.wikipedia.org/wiki/Average_directional_movement_index
chartSeries(QQQ, theme="white", TA="addSMA(50, col='black');addSMA(200, col='blue');addADX(n = 14, maType='EMA', wilder=TRUE)", subset='2013::')

# Look into price as of 2013 and onward
chartSeries(SPY, theme="white", TA="addSMA(50, col='black');addSMA(200, col='blue');addADX(n = 14, maType='EMA', wilder=TRUE)", subset='2013::')


#In a nutshell, Welles recommends using the ADX with a 14-day period. When the main blue line is above 20, it is considered a strong, 
#trending market, when it is below, it is considered a weak one.
#Volume

#As this is an introductory course, we’re mostly using the closing price but it is important to note that there are a lot of other market variables available. 
#You can design systems with the open price, the high or low, the difference between the open and close, etc. And there is also the volume.

#This an important indicator. A falling stack on rising volume or a rising stock on falling volume may mean the move is about to 
#reverse. Whatever the reason for abnormal volume, it should be a warning to keep a vigilant eye on the stock.

#There are plenty of indicators that include the volume price such as the Volume-weighted average price (VWAP). 
#The VWAP is a guide more than a trading indicator as to where the market is trading compared to the volume adjusted price. 
#It divides dollars traded by volume (see above link for more details).

VWAP.Slow <- VWAP(price=SPY$SPY.Close, volume=SPY$SPY.Volume, n=100)
VWAP.Fast <- VWAP(price=SPY$SPY.Close, volume=SPY$SPY.Volume, n=20)
VWAP.Diff <- VWAP.Fast- VWAP.Slow

chartSeries(SPY, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue')")
# QQQ
VWAP.Slow <- VWAP(price=QQQ$QQQ.Close, volume=QQQ$QQQ.Volume, n=100)
VWAP.Fast <- VWAP(price=QQQ$QQQ.Close, volume=QQQ$QQQ.Volume, n=20)
VWAP.Diff <- VWAP.Fast- VWAP.Slow

chartSeries(QQQ, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue')")

ADX.20 <- ADX(QQQ,n=14)

# look for long entries
Long_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff> 0, QQQ$QQQ.Close, NA)

# look for long entries
Short_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff < 0, QQQ$QQQ.Close, NA)

plot(QQQ$QQQ.Close)

points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)



chartSeries(SPY, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue');
            addADX(n = 14, maType='EMA', wilder=TRUE)")
            
ADX.20 <- ADX(SPY,n=14)

# look for long entries
Long_Trades <- ifelse(
        ADX.20$ADX > 20 &
        VWAP.Diff> 0, SPY$SPY.Close, NA)

# look for long entries
Short_Trades <- ifelse(
        ADX.20$ADX > 20 &
        VWAP.Diff < 0, SPY$SPY.Close, NA)

plot(SPY$SPY.Close)
## Warning in plot.xts(SPY): only the univariate series will be plotted
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)

print ("STEP 2.7: Counter-Trend Systems including * Momentum Indicators * Volatility Indicator * Counter-Trend Systems")

#Counter-trend systems are tricky. You trade raw counter trends when you’re sure you’re in a range-bound market 
#and are trading at the extremes otherwise you use added indicators to stay aligned with longer-term trends. 
#Raw counter-trend trading feels like picking tops and bottoms, and those rarely work out. 
#Here we’ll focus on trading the short-term counter trend, while following the long-term trend.


library(binhf)
library(quantmod)
getSymbols(c('EWP', 'SPY'), src='google')

# remove any NAs 
EWP <- EWP[!(rowSums(is.na(EWP))),]
SPY <- SPY[!(rowSums(is.na(SPY))),]



#Momentum Indicators

#We’re going to look at 3 interesting momentum indicators that capture short-term cycles:

#Relative Strength Index (RSI), is an momentum indicator that measures movement. Its author, J. Welles Wilder, recommends using a period of 14 and when it is over 70, it is strongly bought (or overbought) and under 30, it is strongly sold (or oversold).
#REF: https://en.wikipedia.org/wiki/Relative_strength_index
#Commodity Channel Index (CCI) by Donald Lambert, is a price-derived indicator revolving around 0, where 100 is usually considered overbought and -100, oversold.
#REF:https://en.wikipedia.org/wiki/Commodity_channel_index
#Rate of Change (ROC), also a momentum indicator, looks at accelerating and decelerating market moves.
#REF:https://en.wikipedia.org/wiki/Momentum_(technical_analysis)


#Let’s look at all 3 of them with a 20-period setting:

chartSeries(EWP, theme="white", TA="addRSI(n=100);addCCI(n=100);addROC(n=100)", subset='2015')

chartSeries(SPY, theme="white", TA="addRSI(n=100);addCCI(n=100);addROC(n=100)")

#Counter-Trend Systems

#For our counter-trend system, we will counter a faster cycle but stay in the direction of the slower one. In essence, we’re trading with the slow trend but against the fast one. While in the previous systems, we only took a trade while both directions aligned in the direction of the long-term trend.

#The key is to use one of the derived indicators that best signals overbought/oversold signals.

#We’ll try each one of them with a long-term EMA.

chartSeries(EWP, theme="white", TA="addCCI(n=100);addEMA(n=50,col='blue');addEMA(n=200,col='red')")

# create a slow ema difference
EWP.EMA.50 <- EMA(EWP$EWP.Close, n=50) 
EWP.EMA.200 <- EMA(EWP$EWP.Close, n=200) 
Slow.Diff <- EWP.EMA.50 - EWP.EMA.200
CCI.IND <- CCI(HLC=EWP[,c("EWP.High","EWP.Low","EWP.Close")],n=100)

# look for long entries
Long_Trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places=1, dir="right") > CCI.IND &
    CCI.IND < 100 & 
    Slow.Diff > 0, EWP$EWP.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  shift(v=as.numeric(CCI.IND), places=1, dir="right") < CCI.IND &
    CCI.IND > -100 & 
    Slow.Diff < 0, EWP$EWP.Close, NA)

plot(EWP$EWP.Close)
## Warning in plot.xts(EWP): only the univariate series will be plotted
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)


#Volatility indicator

#Chaikin Volatility, uses the high, low, close for its accumulation/distribution and subtracts two moving averages of different 
#periods of the AD.

chartSeries(EWP, theme="white", TA="addChVol(n=100);")

chartSeries(EWP, theme="white", TA="addCCI(n=100);addEMA(n=50,col='blue');addEMA(n=200,col='red');addChVol(n=100);")



# create a slow ema difference
EWP.EMA.50 <- EMA(EWP$EWP.Close, n=50) 
EWP.EMA.200 <- EMA(EWP$EWP.Close, n=200) 
Slow.Diff <- EWP.EMA.50 - EWP.EMA.200
CCI.IND <- CCI(HLC=EWP[,c("EWP.High","EWP.Low","EWP.Close")],n=100)
CV.IND <- chaikinVolatility(HL=EWP[,c("EWP.High","EWP.Low")], n=100)

# look for long entries
Long_Trades <- ifelse(
     shift(v=as.numeric(CCI.IND), places=1, dir="right") > CCI.IND &
        CCI.IND < 100 & 
        CV.IND < 0 & 
        Slow.Diff > 0, EWP$EWP.Close, NA)

# look for short entries
Short_Trades <- ifelse(
       shift(v=as.numeric(CCI.IND), places=1, dir="right") < CCI.IND &
        CCI.IND > -100 & 
        CV.IND < 0 & 
        Slow.Diff < 0, EWP$EWP.Close, NA)

plot(EWP$EWP.Close)
## Warning in plot.xts(EWP): only the univariate series will be plotted
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)


#What about shifting further back on the CCI, this ensures that it is a retracement and not a random bump…

chartSeries(EWP, theme="white", TA="addRSI(n=100);", subset='2015')



chartSeries(EWP, theme="white", TA=NULL, subset='2015')



RSI.Fast <- RSI(price=EWP$EWP.Close,n=10)
RSI.Slow <- RSI(price=EWP$EWP.Close,n=30)
RSI.Diff <- RSI.Fast-RSI.Slow
addTA(RSI.Diff, col='blue', type='h',legend="RSI Diff")




# create a slow ema difference
EWP.EMA.50 <- EMA(EWP$EWP.Close, n=50) 
EWP.EMA.200 <- EMA(EWP$EWP.Close, n=200) 
Slow.Diff <- EWP.EMA.50 - EWP.EMA.200

RSI.IND <- RSI(price=EWP$EWP.Close,n=30)

# look for long entries
Long_Trades <- ifelse(
  RSI.Diff  < 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") > 0  &
    Slow.Diff > 0, EWP$EWP.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  RSI.Diff  > 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") < 0  &
    Slow.Diff < 0, EWP$EWP.Close, NA)

plot(EWP$EWP.Close, main='RSI')
## Warning in plot.xts(EWP, main = "RSI"): only the univariate series will be
## plotted
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)

#Lets see if we can improve this by adding the Chaikin Volatility to the RSI like we did earlier with the CCI counter-trading system.

chartSeries(EWP, theme="white", TA="addRSI(n=100);addChVol(n=100);", subset='2015')

chartSeries(EWP, theme="white", TA=NULL, subset='2015')


RSI.Fast <- RSI(price=EWP$EWP.Close,n=10)
RSI.Slow <- RSI(price=EWP$EWP.Close,n=30)
RSI.Diff <- RSI.Fast-RSI.Slow
addTA(RSI.Diff, col='blue', type='h',legend="RSI Diff")



# create a slow ema difference
EWP.EMA.50 <- EMA(EWP$EWP.Close, n=50) 
EWP.EMA.200 <- EMA(EWP$EWP.Close, n=200) 
Slow.Diff <- EWP.EMA.50 - EWP.EMA.200
CV.IND <- chaikinVolatility(HL=EWP, n=100)
RSI.IND <- RSI(price=EWP$EWP.Close,n=30)

# look for long entries
Long_Trades <- ifelse(
  RSI.Diff  < 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") > 0  &
    CV.IND < -0.1 &
    Slow.Diff > 0, EWP$EWP.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  RSI.Diff  > 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") < 0  &
    CV.IND < -0.1 &
    Slow.Diff < 0, EWP$EWP.Close, NA)

plot(EWP$EWP.Close, main='RSI')
## Warning in plot.xts(EWP, main = "RSI"): only the univariate series will be
## plotted
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)


#Let’s try this final system on the S&P 500
chartSeries(SPY, theme="white", TA="addRSI(n=100);addChVol(n=100);")


# create a slow ema difference
SPY.EMA.50 <- EMA(SPY$SPY.Close, n=50) 
SPY.EMA.200 <- EMA(SPY$SPY.Close, n=200) 
Slow.Diff <- SPY.EMA.50 - SPY.EMA.200

RSI.Fast <- RSI(price=SPY$SPY.Close,n=10)
RSI.Slow <- RSI(price=SPY$SPY.Close,n=30)
RSI.Diff <- RSI.Fast-RSI.Slow

CV.IND <- chaikinVolatility(HL=SPY, n=100)

# look for long entries
Long_Trades <- ifelse(
  CV.IND < -0.1 &
    RSI.Diff  < 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") > 0  &
    shift(v=as.numeric(RSI.Diff ), places=2, dir="right") < 0  &
    Slow.Diff > 0, SPY$SPY.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  CV.IND < -0.1 &
    RSI.Diff  > 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") < 0  &
    shift(v=as.numeric(RSI.Diff ), places=2, dir="right") > 0  &
    Slow.Diff < 0, SPY$SPY.Close, NA)

plot(SPY$SPY.Close, main='RSI')
## Warning in plot.xts(SPY, main = "RSI"): only the univariate series will be
## plotted
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)


# Basket Analysis
#Basket of stocks related to the QQQ

#We’ll use a few member stocks of the QQQ Index. This makes things easy for us, but the concepts discussed here can be 
#applied to any other financial product and index as long they are related in some way.

#We’ll focus on the following tech stocks:

#CSCO, INTC, MSFT, YHOO, TXN. They’re fairly related, of similar size, and we can donwload 10+ years of data for each.

print ("STEP 2.8: Basket of stocks related to the QQQ Index")

library(quantmod)
basket_symbols <- c('MSFT', 'INTC', 'YHOO', 'CSCO', 'TXN', 'QQQ')
getSymbols(basket_symbols, src='google')

#We need to merge all the stocks into one data.frame. We’ll use as.xts that converts objects to xts class, 
#this will merge by time all our columns into one data frame:
basket <- data.frame(as.xts(merge(MSFT, INTC, YHOO, CSCO, TXN, QQQ)))
head(basket,2)

#To keep things simple, we’ll only keep the Close column for all symbols:
basket <- basket[,names(basket)[grepl(x=names(basket), pattern='Close')]]
head(basket)

#Let’s pair every stock with the QQQ in a chart. We’ll overlay them together, and, even though they won’t share the same price scale, 
#it should still give us an idea of how they both move:
plot(as.Date(row.names(basket)), basket$CSCO.Close, col="blue", type='l', ylab="", xlab="")
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$QQQ.Close, col='green', type='l', 
     xaxt="n", yaxt='n', xlab="",ylab="")
axis(4)
legend("topleft",col=c("blue","green"),lty=1,legend=c("CSCO","QQQ"))

plot(as.Date(row.names(basket)), basket$INTC.Close, col="blue", type='l', ylab="", xlab="")
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$QQQ.Close, col='green', type='l', 
     xaxt="n", yaxt='n', xlab="",ylab="")
axis(4)
legend("topleft",col=c("blue","green"),lty=1,legend=c("INTC","QQQ"))

plot(as.Date(row.names(basket)), basket$MSFT.Close, col="blue", type='l', ylab="", xlab="")
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$QQQ.Close, col='green', type='l', 
     xaxt="n", yaxt='n', xlab="",ylab="")
axis(4)
legend("topleft",col=c("blue","green"),lty=1,legend=c("MSFT","QQQ"))

plot(as.Date(row.names(basket)), basket$TXN.Close, col="blue", type='l', ylab="", xlab="")
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$QQQ.Close, col='green', type='l', 
     xaxt="n", yaxt='n', xlab="",ylab="")
axis(4)
legend("topleft",col=c("blue","green"),lty=1,legend=c("TXN","QQQ"))

plot(as.Date(row.names(basket)), basket$YHOO.Close, col='blue', type='l', ylab="", xlab="")
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$QQQ.Close, col='green', type='l', 
     xaxt="n", yaxt='n', xlab="",ylab="")
axis(4)
legend("topleft",col=c("blue","green"),lty=1,legend=c("YHOO","QQQ"))

#All the stocks in our basket have followed the QQQ relatively well with the exception of CISCO. 
#The point here, is that there may be arbitrage opportunities with stocks that deviate from their group or index but 
#it’s important to be cautious. Stocks deviate from their peers for a reason and may want to investigate before jumping in - 
#whether its just a perception or a serious change.

#Looking at direction

#There is a handy function in quantmod called OHLC.Transformations. 
#This allows you to quickly tranform and compare time-series data. 
#We’ll use the ClCl function that will calculate the difference between the current and previous close. 
#We will use the difference between closes to determine if it is an up or down day bar 
#(if yesterday’s close is lower than today’s, then its an up day).

movement_MSFT <- ifelse(ClCl(MSFT)[-1] > 0, 1, -1)
movement_QQQ <- ifelse(ClCl(QQQ)[-1] > 0, 1, -1)
# use a table to see what matched and what didn't
table(movement_MSFT, movement_QQQ)

# Or a simpler way:
sum(movement_MSFT == movement_QQQ) / length(movement_QQQ)

#The resulting table matrix tells us that out of the 2167 trading days recorded, 
#they both had the same down days 762 times and the same up days 843 times. They basically were in sync 74% of the time.

#Let’s compare our other symbols:

movement_INTC <- ifelse(ClCl(INTC)[-1] > 0, 1, -1)
sum(movement_INTC[-1] == movement_QQQ) / length(movement_QQQ)

movement_YHOO <- ifelse(ClCl(YHOO)[-1] > 0, 1, -1)
sum(movement_YHOO[-1] == movement_QQQ[-1]) / length(movement_QQQ)

movement_CSCO <- ifelse(ClCl(CSCO)[-1] > 0, 1, -1)
sum(movement_CSCO == movement_QQQ[-1]) / length(movement_QQQ)

movement_TXN <- ifelse(ClCl(TXN)[-1] > 0, 1, -1)
sum(movement_TXN == movement_QQQ[-1]) / length(movement_QQQ)

print ("STEP 2.9:Basket Analysis * Overall correlation * Time-split correlations")

library(quantmod)
basket_symbols <- c('MSFT', 'INTC', 'YHOO', 'CSCO', 'TXN', 'QQQ')
getSymbols(basket_symbols, src='google')
basket <- data.frame(as.xts(merge(MSFT, INTC, YHOO, CSCO, TXN, QQQ)))
basket <- basket[,names(basket)[grepl(x=names(basket), pattern='Close')]]

#Overall correlation

#So, how correlated are our stocks in our basket? Let’s find out. 
#We’ll use the base cor function in R. It basically compares two vectors applying covariances and standard deviations
# Look at the last column, this shows the QQQ’s correlation to each stock:
results <- c()
for (basket_name in names(basket)) {
       result <- round(as.numeric(cor(basket)[,basket_name]),2)
       results <- rbind(results, c(basket_name,result))
}
results <- data.frame(results)
names(results)[-1] <- names(basket)
results


#Time-split correlations

#Let’s dig deeper and build a function to generelaize the process of getting a correlation table. 
#With this function in hand, we will split the data by time and compare different time periods

# time for a correlation function
Get_Column_Correlations <- function(objDF){
        results <- c()
        for (col_name in names(objDF)) {
               result <- round(as.numeric(cor(objDF)[,col_name]),2)
               results <- rbind(results, c(col_name,result))
        }
        results <- data.frame(results)
        names(results)[-1] <- names(objDF)
        return (results)
}
Get_Column_Correlations(basket[as.Date(rownames(basket)) < '2015-01-01',])[,c('X1','QQQ.Close')]

Get_Column_Correlations(basket[as.Date(rownames(basket)) >= '2015-01-01',])[,c('X1','QQQ.Close')]

par(mfrow=c(3,1))
plot(as.Date(rownames(basket)), basket$YHOO.Close, type='l', col='black', main='YHOO')
plot(as.Date(rownames(basket)), basket$INTC.Close, type='l', col='black', main='INTC')
plot(as.Date(rownames(basket)), basket$QQQ.Close, type='l', col='black', main='QQQ')

#Let’s look at all of these by year and analyze correlations with the QQQ:
basket_years <- unique(substr(rownames(basket), start=1, stop=4))
small_basket <- basket 
MSFT_QQQ <- c()
INTC_QQQ <- c()
YHOO_QQQ <- c()
TXN_QQQ <- c()
CSCO_QQQ <- c()
for (year in basket_years) {
        print(year)
        temp_df <- small_basket[substr(rownames(basket), start=1, stop=4)==year,]
        MSFT_QQQ <- cbind(MSFT_QQQ, cor(temp_df$MSFT.Close, temp_df$QQQ.Close)) 
        INTC_QQQ <- cbind(INTC_QQQ, cor(temp_df$INTC.Close, temp_df$QQQ.Close))
        YHOO_QQQ <- cbind(YHOO_QQQ, cor(temp_df$YHOO.Close, temp_df$QQQ.Close))
        TXN_QQQ <- cbind(TXN_QQQ, cor(temp_df$TXN.Close, temp_df$QQQ.Close))
        CSCO_QQQ <- cbind(CSCO_QQQ, cor(temp_df$CSCO.Close, temp_df$QQQ.Close))
}


small_basket_correlations <- data.frame(rbind(MSFT_QQQ, INTC_QQQ, YHOO_QQQ, TXN_QQQ, CSCO_QQQ))
colnames(small_basket_correlations) <- basket_years
plot(names(small_basket_correlations), small_basket_correlations[1,], type='l', col='darkgreen')
lines(names(small_basket_correlations), small_basket_correlations[2,], type='l', col='red')
lines(names(small_basket_correlations), small_basket_correlations[3,], type='l', col='blue')
lines(names(small_basket_correlations), small_basket_correlations[4,], type='l', col='yellow')
lines(names(small_basket_correlations), small_basket_correlations[5,], type='l', col='pink')
legend(x='bottomleft', legend=c("MSFT", "INTC", "YHOO", "TXN", "CSCO"), col=c("darkgreen","red", "blue", "yellow", "pink"), lwd=1, lty=c(0,0), 
        pch=c(3,3))
        
        
#This is very revealing how the correlation of both stocks with the index waxes and wanes. Let’s visualize these results.

basket_months <- unique(substr(rownames(basket), start=1, stop=7))
small_basket <- basket #[,names(basket)[grepl(x=names(basket), pattern='MSFT|INTC|QQQ')]]
MSFT_QQQ <- c()
INTC_QQQ <- c()
YHOO_QQQ <- c()
TXN_QQQ <- c()
CSCO_QQQ <- c()
for (yearmonth in basket_months) {
        temp_df <- small_basket[substr(rownames(basket), start=1, stop=7)==yearmonth,]
        MSFT_QQQ <- cbind(MSFT_QQQ, cor(temp_df$MSFT.Close, temp_df$QQQ.Close)) 
        INTC_QQQ <- cbind(INTC_QQQ, cor(temp_df$INTC.Close, temp_df$QQQ.Close))
        YHOO_QQQ <- cbind(YHOO_QQQ, cor(temp_df$YHOO.Close, temp_df$QQQ.Close))
        TXN_QQQ <- cbind(TXN_QQQ, cor(temp_df$TXN.Close, temp_df$QQQ.Close))
        CSCO_QQQ <- cbind(CSCO_QQQ, cor(temp_df$CSCO.Close, temp_df$QQQ.Close)) 
}

small_basket_correlations <- data.frame(rbind(MSFT_QQQ, INTC_QQQ, YHOO_QQQ, TXN_QQQ, CSCO_QQQ))
time_axis <- seq(1,ncol(small_basket_correlations))
plot(time_axis, small_basket_correlations[1,], type='l', col='darkgreen')
lines(time_axis, small_basket_correlations[2,], type='l', col='red')
lines(time_axis, small_basket_correlations[3,], type='l', col='blue')
lines(time_axis, small_basket_correlations[4,], type='l', col='yellow')
lines(time_axis, small_basket_correlations[5,], type='l', col='pink')
legend(x='bottomleft', legend=c("MSFT", "INTC", "YHOO", "TXN", "CSCO"), col=c("darkgreen","red", "blue", "yellow", "pink"), lwd=1, lty=c(0,0), 
        pch=c(3,3))
        
        
 
 
 print ("STEP 2.10:Basket Analysis * Applying correlations to entries")
 library(quantmod)
 library(binhf)
 basket_symbols <- c('TXN', 'QQQ')
 getSymbols(basket_symbols, src='google')
 basket <- data.frame(as.xts(merge(TXN, QQQ)))
basket <- basket[,names(basket)[grepl(x=names(basket), pattern='Close')]]

#This is a very simplistic arbitrage-type trade.

#So, what if we buy/hold one of these whenever its far from the index? 
#So , let’s pick a stock that doesn’t overly control the index TXN.

getSymbols(c('TXN', 'QQQ'), src='google')

basket_years <- unique(substr(rownames(basket), start=1, stop=4))
basket_months <- unique(substr(rownames(basket), start=1, stop=7))
small_basket <- basket[,names(basket)[grepl(x=names(basket), pattern='TXN|QQQ')]]
TXN_QQQ <- c()
for (yearmonth in basket_years) {
        temp_df <- small_basket[substr(rownames(basket), start=1, stop=4)==yearmonth,]
        TXN_QQQ <- cbind(TXN_QQQ, cor(temp_df$TXN.Close, temp_df$QQQ.Close))
}

small_basket_correlations <- data.frame(rbind(TXN_QQQ))
colnames(small_basket_correlations) <- basket_years
 
par(mfrow=c(2,1))
plot(as.Date(row.names(basket)), basket$TXN.Close, col='red', 
     type='l', ylab="price", xlab='')
par(new=TRUE)
plot(as.Date(row.names(basket)), basket$QQQ.Close, col='gray', type='l', xaxt="n",yaxt="n",ylab="", xlab='time')
legend("topright",col=c("red","gray"),lty=1,legend=c("TXN","QQQ"))
 
plot(type='l', col='darkgreen', x=as.factor(names(small_basket_correlations)),  y=as.numeric(small_basket_correlations[1,]))
lines(type='l', col='darkgreen', x=as.factor(names(small_basket_correlations)), 
     y=as.numeric(small_basket_correlations[1,]))

#So, let’s create moving-average differences like we did in previous lectures to capture trends:

EMA.Fast <- EMA(TXN$TXN.Close, n=30) 
EMA.Medium <- EMA(TXN$TXN.Close, n=100) 
EMA.Slow <- EMA(TXN$TXN.Close, n=200) 
EMA_Diff_Fast <- EMA.Fast - EMA.Medium
EMA_Diff_Slow <- EMA.Medium - EMA.Slow

chartSeries(TXN, theme="white", TA="addEMA(n=100, col='red');addEMA(n=200, col='blue')")
addTA(EMA_Diff_Slow, col='blue')

QQQ$QQQ.movement <- EMA(ifelse(ClCl(QQQ)  > 0, 1, -1),50)
TXN$TXN.movement <- EMA(ifelse(ClCl(TXN) > 0, 1, -1),50)

plot(as.numeric(TXN$TXN.movement ), col='black', ylab="movement",  main='TXN-QQQ', type = 'l')
abline(h=0, col='red')
par(new=TRUE)
plot(as.numeric(QQQ$QQQ.movement ), col='gray', xaxt="n",yaxt="n",ylab="", xlab='time', type='l')
abline(h=0, col='green')


par(mfrow=c(2,1))
diff <- as.numeric(TXN$TXN.movement)-as.numeric(QQQ$QQQ.movement)
## Warning in as.numeric(TXN$TXN.movement) - as.numeric(QQQ$QQQ.movement):
## longer object length is not a multiple of shorter object length
plot(diff, 
     col='black', type='l', xlab='time', ylab='difference',
     main='TXN-QQQ')
abline(h=0, col='green')
plot(EMA_Diff_Slow)
abline(h=0, col='green')

print ("end of script.")



