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
baozs_dly <- merge.(baoli["2020-10/"], zssk)

######plots######
plot.zoo(wanbao_dly, plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(wanjin_dly, plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(wanzs_dly, plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(merge.xts(baoli, jindi), plot.type = "single", col = c("blue", "red"),
         lty = 1:2)
plot.zoo(merge.xts(baoli["2020-10/"], zssk), plot.type = "single", 
         col = c("blue", "red"), lty = 1:2)
plot.zoo(merge.xts(jindi["2020-10/"], zssk), plot.type = "single", 
         col = c("blue", "red"), lty = 1:2)
######dft######
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

######baoli/zssk######
sum(index(baoli["2020-10/"]) != index(zssk))
baozs_coint <- dynlm(zoo(baoli["2020-10/"]) ~ zoo(zssk))
summary(baozs_coint) 
baozs_z <- ur.df(resid(baozs_coint),
                  type = "none",
                  lags = 120,
                  selectlags = "AIC")
summary(baozs_z) #!!!!!!good coint relation @1% 

#past n days rolling z to forecast/compare with the historical z
baozs_z12mt_sd <- rollapplyr(baozs_dly, function(ts){
  sd(resid(dynlm(ts[,1] ~ ts[,2])))}, 
  by.column = FALSE, width = 240, partial = FALSE)

summary(ur.df(baozs_z12mt_sd,
              type = "none",
              lags = 120,
              selectlags = "AIC")) #no ur at 1%!!!! 

baozs_z12mt_mean <- rollapplyr(baozs_dly, function(ts){
  mean(resid(dynlm(ts[,1] ~ ts[,2])))}, 
  by.column = FALSE, width = 240, partial = FALSE)

summary(ur.df(baozs_z12mt_mean,
      type = "none",
      lags = 120,
      selectlags = "AIC")) #no ur @5% 

baozs_z12mt_median <- rollapplyr(baozs_dly, function(ts){
  median(resid(dynlm(ts[,1] ~ ts[,2])))}, 
  by.column = FALSE, width = 240, partial = FALSE) 

summary(ur.df(baozs_z12mt_median,
              type = "none",
              lags = 120,
              selectlags = "AIC")) #has ur

baozs_z12mt_max <- rollapplyr(baozs_dly, function(ts){
  max(resid(dynlm(ts[,1] ~ ts[,2])))}, 
  by.column = FALSE, width = 240, partial = FALSE) 

summary(ur.df(baozs_z12mt_max,
              type = "none",
              lags = 120,
              selectlags = "AIC")) #no ur @5%

baozs_z12mt_min <- rollapplyr(baozs_dly, function(ts){
  min(resid(dynlm(ts[,1] ~ ts[,2])))}, 
  by.column = FALSE, width = 240, partial = FALSE) 

summary(ur.df(baozs_z12mt_min,
              type = "none",
              lags = 120,
              selectlags = "AIC")) #no ur @1%l!!!

plot.zoo(merge.zoo(baozs_z12mt_max, baozs_z12mt_median, baozs_z12mt_mean, 
                   baozs_z12mt_sd, baozs_z12mt_min))
######backtest######
####baoli/zssk####
plot.zoo(resid(baozs_coint))
boxplot(coredata(resid(baozs_coint)))
plot(density(coredata(resid(baozs_coint))))
summary(coredata(resid(baozs_coint)))

#test 2022 YtoD performance
baozs_zresid <- as.xts(resid(baozs_coint))["2022"] 
sum(abs(baozs_zresid) >= 0.1)/length(baozs_zresid)

baozs_zmed <- summary(coredata(resid(baozs_coint)))["Median"]
baozs_signal <- c() #trading signal based on z-median
for(i in 1:length(baozs_zresid)){
  baozs_signal <- c(baozs_signal, ifelse(
    baozs_zresid[i] >  baozs_zmed, -1, ifelse(
      baozs_zresid[i] < baozs_zmed, 1, 0)
    )
  )
}

#gross of fee account holding based on 10k
baozs_date <- index(baozs_zresid)
baozs_pos <- as.numeric(10000/exp(zssk[baozs_date[1]])) 
#the loop uses the t+0 price for position switching
for(i in 2:length(baozs_zresid)){
  baozs_pos <- c(baozs_pos, ifelse(baozs_signal[i-1]*baozs_signal[i] == 1, 
                                      baozs_pos[i-1], 
    ifelse(baozs_signal[i-1]*baozs_signal[i] == -1, 
      ifelse(baozs_signal[i] == 1, 
        baozs_pos[i-1] * exp(zssk[baozs_date[i]]) / exp(baoli[baozs_date[i]]), 
        ifelse(baozs_signal[i] == -1, 
        baozs_pos[i-1] * exp(baoli[baozs_date[i]]) / exp(zssk[baozs_date[i]]), 
        baozs_pos[i-1])
      ), baozs_pos[i-1])
    )
    )
}

#gross nav 
baozs_nav <- as.numeric(exp(zssk[baozs_date[1]]) * baozs_pos[1])
for(i in 2:length(baozs_zresid)){
  baozs_nav <- c(baozs_nav, ifelse(baozs_signal[i-1]*baozs_signal[i] == 1, 
                                   baozs_nav[i-1], 
    ifelse(baozs_signal[i-1]*baozs_signal[i] == -1, 
          ifelse(baozs_signal[i] == 1, 
       baozs_pos[i] * exp(baoli[baozs_date[i]]), 
       ifelse(baozs_signal[i] == -1, 
        baozs_pos[i] * exp(zssk[baozs_date[i]]), 
        baozs_nav[i-1])
          ), baozs_nav[i-1])
  )
  )
}

plot.zoo(merge.xts(xts(baozs_nav, baozs_date), 
                   xts(baozs_signal, baozs_date)),
         col = c("blue", "red"), lty = 1:2)
coredata(baoli[baozs_date[216]]) - coredata(baoli[baozs_date[1]])
coredata(zssk[baozs_date[216]]) - coredata(zssk[baozs_date[1]])               
log(baozs_nav[216]/baozs_nav[1])

#12m trailing sd for signal
baozs_sd12mtsig <- signal_generator(ref = xts(baozs_z12mt_sd)["2022"], 
                                    residz = baozs_zresid)
baozs_sd12mtbk <- backtest(10000, signal = baozs_sd12mtsig, x = zssk,
                           y = baoli, residz = baozs_zresid) #???something goes wrong with principal

qplot(baozs_sd12mtbk$bt_date, baozs_sd12mtbk$bt_nav, geom = c("point", "line"))
plot.zoo(merge.xts(xts(log(baozs_backtest$bt_nav), baozs_backtest$bt_date),
                   xts(log(baozs_sd12mtbk$bt_nav), baozs_sd12mtbk$bt_date)), 
         plot.type = "single", col = c("red", "blue"), 
         lwd = 2)

baozs_backtest <- backtest(10000, signal = baozs_signal, x = zssk,
                           y = baoli, residz = baozs_zresid)
qplot(baozs_backtest$bt_date, baozs_backtest$bt_nav, geom = c("point", "line"))
plot.zoo(merge.xts(xts(log(baozs_backtest$bt_nav / 1000), 
                       baozs_backtest$bt_date),
                   xts(baozs_dly)["2022"]), plot.type = "single", 
         col = c("red", "blue", "green"), lwd = 2)

#####backtesting functions#####
signal_generator <- function(ref, residz){
  #ref is a signal decider from the statistics of residz
  #ref is a vector having same length of residz
  signal <- c()
  for(i in 1:length(residz)){
    signal <- c(signal, ifelse(
      residz[i] > ref[i] & ref[i] > 0, -1, ifelse(
        residz[i] < ref[i] & ref[i] <0, 1, 0)
      )
    )
  }
  
  return(signal)
}

backtest <- function(principal, signal, x, y, residz){
  #base pair relationship: y ~ x + residz
  #x, y are log prices
  #x, y, residz are xts class
  #gross of fee account holding based on principal dollar
  bt_date <- index(residz)
  bt_pos <- ifelse(signal[1] == 1, 
                   as.numeric(principal / exp(y[bt_date[1]])), 
              ifelse(signal[1] == -1,
                   as.numeric(principal / exp(x[bt_date[1]])),
                   principal))
  #gross nav 
  bt_nav <- ifelse(signal[1] == 1, 
                   as.numeric(exp(y[bt_date[1]]) * bt_pos[1]),
                   as.numeric(exp(x[bt_date[1]]) * bt_pos[1]))
  #the loop uses t+0 price for position switching
  for(i in 2:length(residz)){
    bt_pos <- c(bt_pos, ifelse(signal[i-1] * signal[i] == 1, bt_pos[i-1], 
                ifelse(signal[i-1] * signal[i] == -1, 
                  ifelse(signal[i] == 1, 
                    bt_pos[i-1] * exp(x[bt_date[i]]) / exp(y[bt_date[i]]), 
                      ifelse(signal[i] == -1, 
                        bt_pos[i-1] * exp(y[bt_date[i]]) / exp(x[bt_date[i]]), 
                        bt_pos[i-1])
                    ), bt_pos[i-1])
                )
              )
  }
  
  for(i in 2:length(residz)){
    bt_nav <- c(bt_nav, ifelse(signal[i-1]*signal[i] == 1, ifelse(
      signal[i] == 1, bt_pos[i] * exp(y[bt_date[i]]), ifelse(signal[i] == -1, 
       bt_pos[i] * exp(x[bt_date[i]]), bt_nav[i-1])),
                ifelse(signal[i-1] * signal[i] == -1, 
                  ifelse(signal[i] == 1, bt_pos[i] * exp(y[bt_date[i]]), 
                    ifelse(signal[i] == -1, bt_pos[i] * exp(x[bt_date[i]]), 
                      ifelse(signal[i] == 1, bt_pos[i] * exp(y[bt_date[i]]), 
                        ifelse(signal[i] == -1, bt_pos[i] * exp(x[bt_date[i]]), 
                               bt_nav[i-1])))
                    ), bt_nav[i-1])
                   )
                  )
  }
  
  return(data.frame(bt_date, bt_pos, bt_nav)) 
}

