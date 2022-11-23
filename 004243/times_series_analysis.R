library(tidyverse)
library(data.table)
library(lubridate)
library(xts) 
library(dynlm)
library(vars)
library(quantmod)

etf <- read_csv("./004243/data/etf.csv")
oil <- read_csv("./004243/data/oil.csv")

#create xts of etf from 2020-end
etf_ts <- xts(etf$Close, etf$Date)["2020/"]
etf_wkly <- to.weekly(etf_ts) #convert to weekly OHLC series

oil_ts <- xts(as.numeric(oil$Close), oil$Date)["2020/"]
oil_wkly <- to.weekly(oil_ts) #convert to weekly

#combine the weekly etf and oil 
sum(index(etf_wkly) != index(oil_wkly)) #check time discrepancy
etf_oil_wkly <- cbind(etf_wkly$etf_ts.Close, oil_wkly$oil_ts.Close)
etf_oil_wkly <- log(etf_oil_wkly)
sum(is.na(etf_oil_wkly)) #check NAs

plot.zoo(etf_oil_wkly, plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
cor(etf_oil_wkly$etf_ts.Close, etf_oil_wkly$oil_ts.Close) 
#high correlation between price

plot.zoo(diff(etf_oil_wkly), plot.type = "single", col = c("blue", "red"),
         lty = 1:2) #graphically cointegration
cor(diff(log(etf_oil_wkly$etf_ts.Close))[-1], 
    diff(log(etf_oil_wkly$oil_ts.Close))[-1]) 
#low correlation between return

#check integration order of each wkly ts
etf_df <- summary(ur.df(etf_oil_wkly$etf_ts.Close, 
             type = "trend", 
             lags = 20, 
             selectlags = "AIC"))
etf_df #strong I(1) evidence with drift + trend @10%

oil_df <- summary(ur.df(etf_oil_wkly$oil_ts.Close,
                        type = "trend",
                        lags = 20, 
                        selectlags = "AIC"))
oil_df #strong I(1) with drift + trend @10% 

etfoil_spread <- summary(ur.df(etf_oil_wkly$etf_ts.Close - 
                                 etf_oil_wkly$oil_ts.Close,
                               type = "trend",
                               lags = 20, 
                               selectlags = "AIC"))
etfoil_spread #showing I(1) across all three types 

