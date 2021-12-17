# CP640_OC1_ML
ML project 
This project is designed for Machine Learning project - WLU
Saeid Rezaei - 205812010
In this project , I'm going to build few models to review the stock markets prices and predict the future price - 
I will be using Time Series , SMA , and LSTM model for this ML project
I will use Perl script to clean and fill the missed data
I will use R programing to analyses the data and create plat , and compare few stock markets
I will use Phyton to build LSTM model 



Project Proposal
Stock Market and price prediction

Saeid Rezaei
WLU – CP-640 – Machine Learning 

Instructor
Elham Harirpoush


Abstract
In this project, I’m going to analyze US stock market mainly New York stock market. There are two prices that are critical for any investor to know: the current price of the investment they own or plan to own and its future selling price. Despite this, investors are constantly reviewing past pricing history and using it to influence their future investment decisions. Some investors won't buy a stock or index that has risen too sharply, because they assume it's due for a correction, while other investors avoid a falling stock because they fear it will continue to deteriorate.
I will be using stock market price and will analyze change of few famous stock and would like to predict the market price base on the history of price and global economy.
I will be using Time series algorithm to solve this problem, and will take data from kaggle.   
Keywords: Stock Market, NYSM, current price, future price, time series, global economy.

Introduction
Predicting how the stock market will perform is one of the most difficult things to do. There are so many factors involved in the prediction – physical factors vs. psychological, rational and irrational behavior, etc. All these aspects combine to make share prices volatile and very difficult to predict with a high degree of accuracy.
Can we use machine learning as a game-changer in this domain? Using features like the latest announcements about an organization, their quarterly revenue results, etc., machine learning techniques have the potential to unearth patterns and insights we didn’t see before, and these can be used to make unerringly accurate predictions.
 In this project, I will use historical stock market related to US market in NY for some famous stock such as amazon, Facebook, and google. For this analysis I will use liner regression to analyze the price and will use advance machine learning methods such as moving average techniques and LSTMs.

The outcome of this predication could help individual investors and Portfolio managers in all financial institutions to have better confidence on their investment without monitoring the market so frequency.

Data Sets analysis
In this project, I will use NY stock market data from either Kaggle website or stock price library in Python. Price of stock could change several times in the short period of times and there is no benefit to look into interim price, essentially each investors make decision based on closing price, so I will be using close price in my analysis.
⦁	Steps for data analysis
⦁	Clean data and find the missing information. For the missing information I will use price for the closest day 
⦁	Analyze stock market based on Time series approach
Despite the volatility, stock prices aren’t just randomly generated numbers. So, they can be analyzed as a sequence of discrete-time data; in other words, time-series observations taken at successive points in time (usually on a daily basis). Time series forecasting (predicting future values based on historical values) applies well to stock forecasting.  
⦁	For training data sets I will be using %80 of oldest data and price available and for test data will use the other %20 of the most recent price.

Techniques and Virtualization
⦁	Time series – Stock market changes almost every day based on different factors and it’s not just random numbers so they can be analyzed as discrete-time data; hence I will be using time series analysis to predict the price.

⦁	Moving average (MA) – In this project, I will be using MA technique to analysis the stock price - MA is a popular to smooth out random movements in the stock market. I will be analyzing two method on MA as below:

Simple MA – SMA the calculates the average of a range of stock prices over the specific periods of time:
 SMA = P1 + P2 + P3 /N, where Pn = the stock price at time point n. and is the number of point.
Exponential MA - In EMA we assign equal weight to all historical data point which is diffrenet from SMA:
EMA = Pt * k + EMA t-1 (1-k), where pt = the price at the time point t
EMA t-1 = EMA at the time of t-1

⦁	 LSTM Model – I will use LSTM to classify the time-series data, long short-term memory is an artificial recurrent neural network (RNN) architect that uses for time-series data.
I will be using different chart to show the predication, mainly time series chart to show the stock trends and prediction of price. Below is one example:
 
    

     References:
⦁	Long Short – term memory – ⦁	Wikipedia
⦁	Prediction Model for Stock market in India – ⦁	Aparna Nayak
⦁	US Stock Market Index – ⦁	Investopedia 






