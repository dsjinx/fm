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
etf_oil_wkly <- cbind(etf_wkly)
