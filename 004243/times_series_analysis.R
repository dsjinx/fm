library(tidyverse)
library(data.table)
library(lubridate)
library(xts) 
library(dynlm)
library(vars)
library(caret)

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
             lags = 26, 
             selectlags = "AIC"))
etf_df #strong I(1) evidence with drift + trend @10%

oil_df <- summary(ur.df(etf_oil_wkly$oil_ts.Close,
                        type = "trend",
                        lags = 26, 
                        selectlags = "AIC"))
oil_df #strong I(1) with drift + trend @10%, but for RW, and drift only @5%

#estimate cointegration vector
etfoil_coint <- dynlm(zoo(etf_oil_wkly$etf_ts.Close) ~ 
                        zoo(etf_oil_wkly$oil_ts.Close))
summary(etfoil_coint)

#check stationary of coint model residual
etfoil_z_df <- ur.df(resid(etfoil_coint), 
                     type = "none",
                     lags = 26,
                     selectlags = "AIC")
summary(etfoil_z_df) #strong I(1), so the cointegration is rejected

########wanke000002########
wanke <- read_csv("~/Downloads/stockdata/000002.csv") 
baoli <- read_csv("~/Downloads/stockdata/600048.csv")
jindi <- read_csv("~/Downloads/stockdata/600383.csv")
zssk <- read_csv("~/Downloads/stockdata/001979.csv")

wanke <- xts(log(wanke$close), wanke$date)
baoli <- xts(log(baoli$close), baoli$date)
jindi <- xts(log(jindi$close), jindi$date)["2020/"]
zssk <- xts(log(zssk$close), zssk$date)["2020-10/"]

sum(index(wanke) != index(baoli))
sum(index(wanke) != index(jindi))
sum(index(wanke["2020-10/"]) != index(zssk))

wanbao_dly <- merge.xts(wanke, baoli)
wanjin_dly <- merge.xts(wanke, jindi)
wanzs_dly <- merge.xts(wanke["2020-10/"], zssk["2020-10/"])

plot.zoo(wanbao_dly, plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(wanjin_dly, plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(wanzs_dly, plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(merge.xts(baoli, jindi), plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(merge.xts(baoli["2020-10/"], zssk), plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(merge.xts(jindi["2020-10/"], zssk), plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
######df######
wankeur <- summary(ur.df(wanbao_dly$wanke, 
                         type = "trend",
                         lags = 120,
                         selectlags = "AIC"))
wankeur #I(1) with drift + trend @ 5%, none @ 5%

baour <- summary(ur.df(wanbao_dly$baoli,
                       type = "trend",
                       lags = 120,
                       selectlags = "AIC"))
baour #I(1) @10% in all three situations

jindiur <- summary(ur.df(wanjin_dly$jindi,
                         type = "trend",
                         lags = 120,
                         selectlags = "AIC"))
jindiur #none@5%, drift@5%, trend@10%

zsskur <- summary(ur.df(wanzs_dly$zssk..2020.10...,
                         type = "trend",
                         lags = 120,
                         selectlags = "AIC"))
zsskur #all cases@10%
######coint######
wanbao_coint <- dynlm(zoo(wanbao_dly$wanke) ~ zoo(wanbao_dly$baoli))
summary(wanbao_coint) #no coint relation

wanbao_z <- ur.df(resid(wanbao_coint),
                  type = "none",
                  lags = 120,
                  selectlags = "AIC")
summary(wanbao_z) 

wanjin_coint <- dynlm(zoo(wanjin_dly$wanke) ~ zoo(wanjin_dly$jindi))
summary(wanjin_coint)

wanjin_z <- ur.df(resid(wanjin_coint),
                  type = "none",
                  lags = 120,
                  selectlags = "AIC")
summary(wanjin_z) #showing strong coint relation but z is I(1) 

wanzs_coint <- dynlm(zoo(wanzs_dly$wanke..2020.10...) ~ 
                       zoo(wanzs_dly$zssk..2020.10...))
summary(wanzs_coint)

wanzs_z <- ur.df(resid(wanzs_coint),
                  type = "none",
                  lags = 120,
                  selectlags = "AIC")
summary(wanjin_z) #strong lm but z is I(1) 

sum(index(baoli) != index(jindi))
baojin_coint <- dynlm(zoo(baoli) ~ zoo(jindi))
summary(baojin_coint)
baojin_z <- ur.df(resid(baojin_coint),
                  type = "none",
                  lags = 120,
                  selectlags = "AIC")
summary(baojin_z) #good lm but z is I(1) 

sum(index(baoli["2020-10/"]) != index(zssk))
baozs_coint <- dynlm(zoo(baoli["2020-10/"]) ~ zoo(zssk))
summary(baozs_coint)
baozs_z <- ur.df(resid(baozs_coint),
                  type = "none",
                  lags = 120,
                  selectlags = "AIC")
summary(baozs_z) #!!!!!!good coint relation @1% 

######backtest######
plot.zoo(resid(baozs_coint))
