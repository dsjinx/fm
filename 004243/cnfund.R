library(tidyverse)
library(lubridate)
library(data.table)
library(caret)
library(rvest)
library(readxl)
library(doParallel)
options(digits = 3)
registerDoParallel(cores = 3)

#eastmoney funds nav
url <- paste0("http://fund.eastmoney.com/f10/F10DataApi.aspx?type=lsjz",
              "&code=004243",
              "&sdate=2019-11-04",
              "&edate=2022-11-04",
              "&per=20&page=1")

pg1 <- read_html(url)
tb1 <- html_table(pg1)[[1]][, 1:2] #1st page

urlt <- paste0("http://fund.eastmoney.com/f10/F10DataApi.aspx?type=lsjz",
              "&code=004243",
              "&sdate=2019-11-04",
              "&edate=2022-11-04",
              "&per=20&page=%s")
for(p in 2:37){
  url_temp <- sprintf(urlt, p)
  tb_temp <- read_html(url_temp) %>% html_table()
  tb1 <- rbind(tb1, tb_temp[[1]][, 1:2])
  rm(url_temp, tb_temp)
}

names(tb1) <- c("DATE", "NAV")
setDT(tb1) #fund

#benchmark DJSOEP
bm <- read_xls("DJSOEP.xls")
setDT(bm) #benchmark index
names(bm) <- c("DATE", "index")
bm[, DATE := as.character(DATE)]

rm(p, url, urlt, pg1)

data <- bm[tb1, on = .(DATE)] #combine fund and benchmark
data[, DATE := as.Date(DATE)]

data_plot <- melt(data[, index := index/5000],
                  id.vars = "DATE")[order(DATE),]
data_plot %>% ggplot(aes(x = DATE, y = log2(value), col = variable)) + 
  geom_line() #visually in perfect correlation, so it is strict index fund 
#so use DJSOEP as proxy to predict the performance of the fund

#oil
oil <- read_csv("oil.csv")
names(oil) <- c("DATE","oil")
setDT(oil)
oil[, DATE := as.character(DATE)]

bm_oil <- bm[oil, on = .(DATE)][, index := index/100]
bo_plot <- melt(bm_oil, id.vars = "DATE")[, 
              DATE := as.Date(DATE)][order(DATE),]
bo_plot %>% ggplot(aes(x = DATE, y = log2(value), col = variable)) + 
  geom_line() #sign of cointegration

bm_oil[, DATE := as.Date(DATE)]
cor(bm_oil[DATE >= "2022-01-01", "index"], 
    bm_oil[DATE >= "2022-01-01", "oil"]) #2022 big divergence
cor(bm_oil[DATE < "2022-01-01", "index"], 
    bm_oil[DATE < "2022-01-01", "oil"]) #prior 2022 high correlation
bo_plot[DATE >= "2022-01-01",] %>% 
  ggplot(aes(x = DATE, y = log2(value), col = variable)) + 
  geom_line()
bo_plot[DATE < "2022-01-01",] %>% 
  ggplot(aes(x = DATE, y = log2(value), col = variable)) + 
  geom_line()

#vix
vx <- read_csv("vix.csv")
names(vx) <- c("DATE","vix")
setDT(vx)
vx[, DATE := as.character(DATE)]

bm_vx <- bm[vx, on = .(DATE)][, index := index/500]
bvx_plot <- melt(bm_vx, id.vars = "DATE")[, 
                DATE := as.Date(DATE)][order(DATE),]
bvx_plot %>% ggplot(aes(x = DATE, y = log2(value), col = variable)) + 
  geom_line() #strong sign of negative correlation

#gold
gd <- read_csv("gold.csv")
names(gd) <- c("DATE","gold")
setDT(gd)
gd[, DATE := as.character(DATE)]

bm_gd <- bm[gd, on = .(DATE)][, index := index/5]
bgd_plot <- melt(bm_gd, id.vars = "DATE")[, 
                DATE := as.Date(DATE)][order(DATE),]
bgd_plot %>% ggplot(aes(x = DATE, y = log2(value), col = variable)) + 
  geom_line() #negative correlation in median-long term 

#usdx
usd <- read_csv("usdx.csv")
names(usd) <- c("DATE","usd")
setDT(usd)
usd[, DATE := as.character(DATE)]

bm_usd <- bm[usd, on = .(DATE)][, index := index/60]
busd_plot <- melt(bm_usd, id.vars = "DATE")[, 
                 DATE := as.Date(DATE)][order(DATE),]
busd_plot %>% ggplot(aes(x = DATE, y = log2(value), col = variable)) + 
  geom_line() #no relation visually

#combined return table
cmb <- read_csv("returntable.csv")
cmb <- cmb[-1,]
plot_cols <- names(cmb)[-c(2, 8, 10, 12, 14)]
cmb_returns <- cmb[, plot_cols] #selecting all the return cols
setDT(cmb_returns)

return_plot <- melt(cmb_returns, id.var = "date")[order(date),]
return_plot %>% ggplot(aes(x = date, y = value, col = variable)) + 
  geom_line()
return_plot %>% ggplot(aes(x = date, y = value)) + geom_line() + 
  facet_wrap(~ variable, scales = "free_y", ncol = 2)
#seasonality in djsoep returns

oil_usd <- oil[usd, on = "DATE"]
oil_usd <- oil_usd[-which(is.na(oil_usd$oil)),]
oil_usd[, DATE := as.Date(DATE)]
cor(oil_usd$oil, oil_usd$usd)
ggplot(melt(oil_usd, id.var = "DATE")[order(DATE),], 
       aes(x = DATE, y = value, col = variable)) + geom_line()

#######yahoo finance######
#basic historical data url
#query1.finance.yahoo.com/v7/finance/download/IEO?period1=1147046400&period2=1667952000&interval=1d&events=history&includeAdjustedClose=true
yahoo_url <- paste0("query1.finance.yahoo.com/v7/finance/download/",
                    "IEO",
                    "?period1=1147046400",
                    "&period2=1667952000",
                    "&interval=1d",
                    "&events=history&includeAdjustedClose=true")

#period time is in unix timestamp form
start_date <- as.character(
  as.numeric(ISOdate(2006,05,06, hour = 23, min = 59)))
end_date <- as.character(
  as.numeric(ISOdate(2022,11,08, hour = 23, min = 59)))

djsoep_etf <- "IEO"
oil_index <- "CL=F"
gold_index <- "GC=F"
vix <- "^VIX"
usdx <- "DX-Y.NYB"

etf_url <- paste0("query1.finance.yahoo.com/v7/finance/download/",
                  djsoep_etf,
                  "?period1=",start_date,
                  "&period2=",end_date,
                  "&interval=1d",
                  "&events=history&includeAdjustedClose=true")
download.file(etf_url, "./004243/data/etf.csv")
etf <- read_csv("./004243/data/etf.csv")

oil_url <- paste0("query1.finance.yahoo.com/v7/finance/download/",
                    oil_index,
                    "?period1=",start_date,
                    "&period2=",end_date,
                    "&interval=1d",
                    "&events=history&includeAdjustedClose=true")
download.file(oil_url, "./004243/data/oil.csv")
oil <- read_csv("./004243/data/oil.csv")

gold_url <- paste0("query1.finance.yahoo.com/v7/finance/download/",
                  gold_index,
                  "?period1=",start_date,
                  "&period2=",end_date,
                  "&interval=1d",
                  "&events=history&includeAdjustedClose=true")
download.file(gold_url, "./004243/data/gold.csv")
gold <- read_csv("./004243/data/gold.csv")

vix_url <- paste0("query1.finance.yahoo.com/v7/finance/download/",
                   vix,
                   "?period1=",start_date,
                   "&period2=",end_date,
                   "&interval=1d",
                   "&events=history&includeAdjustedClose=true")
download.file(vix_url, "./004243/data/vix.csv")
vix <- read_csv("./004243/data/vix.csv")

usdx_url <- paste0("query1.finance.yahoo.com/v7/finance/download/",
                  usdx,
                  "?period1=",start_date,
                  "&period2=",end_date,
                  "&interval=1d",
                  "&events=history&includeAdjustedClose=true")
download.file(usdx_url, "./004243/data/usdx.csv")
usdx <- read_csv("./004243/data/usdx.csv")

#visualising relationship of etf-oil prices
setDT(etf)
setDT(oil)
etf_oil <- oil[etf, on = "Date"]
#check NAs
names(etf_oil)
etf_oil[, lapply(.SD, function(j) sum(is.na(j))), 
        .SDcols = c("Close", "i.Close")]
etf_oil[which(is.na(etf_oil$Close)), .(Date, Close, i.Close)]
wday(etf_oil[which(is.na(etf_oil$Close)), 
             .(Date, Close, i.Close)]$Date)
#clean NAs
etf_oil <- etf_oil[-which(is.na(etf_oil$Close)), 
                   .(Date, Close, i.Close)]
etf_oil[, lapply(.SD, function(j) sum(is.na(j)))]
setnames(etf_oil, names(etf_oil), c("Date", "oil", "etf"))
names(etf_oil)
str(etf_oil)
etf_oil[, oil := as.numeric(oil)]
etf_oil[, lapply(.SD, function(j) sum(is.na(j)))]
etf_oil[which(is.na(etf_oil$oil)),] #why there is NA again after clean
oil[Date %in% c(etf_oil[which(is.na(etf_oil$oil)), Date]), 
    c("Date", "Close")]
range(oil$Close) #innate null, so clean them again
etf_oil <- etf_oil[-which(is.na(etf_oil$oil)),]
etf_oil[, lapply(.SD, function(j) sum(is.na(j)))] #check again for NAs

ggplot(melt(etf_oil, id.vars = "Date")[order(Date),], 
       aes(x = Date, y = value, col = variable)) + geom_line()
cor(etf_oil$oil, etf_oil$etf)

#etf return table
setDT(etf)
range(wday(etf$Date)) #all the prices are in mon-fri
etf_return <- etf[, .(Date, Close)][
  , ":="(r1d = c(NA, diff(log(Close))), 
         r5d = c(rep(NA, 5), diff(log(Close), 5)),
         r10d = c(rep(NA, 10), diff(log(Close), 10)), 
         r30d = c(rep(NA, 30), diff(log(Close), 30)),
         r90d = c(rep(NA, 90), diff(log(Close), 90)))]
#check extra NAs apart for paddings
etf_return[, lapply(.SD, function(j) sum(is.na(j)))]
etf_return_plot <- melt(etf_return, id.var = "Date")[order(Date),]
ggplot(etf_return_plot, aes(Date, value)) + geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2)

#oil return table
setDT(oil)
str(oil) #data are in chr class
oil_return <- oil[, .(Date, Close)][, Close := as.numeric(Close)]
str(oil_return)
oil_return[, lapply(.SD, range)] #there're NAs
oil_return[, lapply(.SD, function(j) sum(is.na(j)))] #recheck after below
oil_return <- oil_return[, ":="(r1d = c(NA, diff(log(Close))), 
         r5d = c(rep(NA, 5), diff(log(Close), 5)),
         r10d = c(rep(NA, 10), diff(log(Close), 10)), 
         r30d = c(rep(NA, 30), diff(log(Close), 30)),
         r90d = c(rep(NA, 90), diff(log(Close), 90)))]

#check for NaNs from original set
oil_return[, lapply(.SD, function(j) sum(is.nan(j)))]
nans_ind <- oil_return[, lapply(.SD, function(j) which(is.nan(j)))][,
                .(r1d, r5d, r10d, r30d, r90d)]
oil_return[unique(c(nans_ind$r1d, nans_ind$r5d, nans_ind$r10d, nans_ind$r10d,
       nans_ind$r30d, nans_ind$r90d)),] 
#nan is caused by negative price incident in 2020

#vix return table
setDT(vix)
str(vix)
vix_return <- vix[, .(Date, Close)][
  , ":="(r1d = c(NA, diff(log(Close))), 
         r5d = c(rep(NA, 5), diff(log(Close), 5)),
         r10d = c(rep(NA, 10), diff(log(Close), 10)), 
         r30d = c(rep(NA, 30), diff(log(Close), 30)),
         r90d = c(rep(NA, 90), diff(log(Close), 90)))]
vix_return[, lapply(.SD, function(j) sum(is.na(j)))] #check NAs

#gold return table
setDT(gold)
str(gold)
gold_return <- gold[, .(Date, Close)][, Close := as.numeric(Close)]
gold_return <- gold_return[, .(Date, Close)][
  , ":="(r1d = c(NA, diff(log(Close))), 
         r5d = c(rep(NA, 5), diff(log(Close), 5)),
         r10d = c(rep(NA, 10), diff(log(Close), 10)), 
         r30d = c(rep(NA, 30), diff(log(Close), 30)),
         r90d = c(rep(NA, 90), diff(log(Close), 90)))]
gold_return[, lapply(.SD, function(j) sum(is.na(j)))] #excessive number of NAs
gold_return[, lapply(.SD, function(j) sum(is.nan(j)))] #but no NaNs

#usdx return table
setDT(usdx)
str(usdx)
usd_return <- usdx[, .(Date, Close)][, Close := as.numeric(Close)]
usd_return <- usd_return[, .(Date, Close)][
  , ":="(r1d = c(NA, diff(log(Close))), 
         r5d = c(rep(NA, 5), diff(log(Close), 5)),
         r10d = c(rep(NA, 10), diff(log(Close), 10)), 
         r30d = c(rep(NA, 30), diff(log(Close), 30)),
         r90d = c(rep(NA, 90), diff(log(Close), 90)))]
usd_return[, lapply(.SD, function(j) sum(is.na(j)))]
usd_return[, lapply(.SD, function(j) sum(is.nan(j)))]
#excessive number of NAs but no NaN

#prepare the training table
#log(N) - log(N-k) = sum(log(n)); n is from N- k to N
#so just subset all the predictors by etf, and log() the daily close
data_tbl <- usdx[, c(1, 5)][gold[, c(1, 5)][vix[, c(1, 5)][oil[, c(1, 5)][
  etf[, c(1, 5)], on = "Date"], on = "Date"], on = "Date"], on = "Date"]
str(data_tbl) #rename the cols and some cols are in chr form
names(data_tbl) <- c("Date", "usdx", "gold", "vix", "oil", "etf")
data_tbl <- data_tbl[, lapply(.SD, function(j) 
                    if(is.character(j)) as.numeric(j) else j)]
data_tbl[, lapply(.SD, function(j) sum(is.na(j))),
         .SDcols = c("usdx", "gold", "vix", "oil", "etf")]
#delete all the NA rows, because log returns can include all the intra returns
NA_ind <- lapply(data_tbl[,-1], function(j) which(is.na(j)))
NA_ind <- unlist(NA_ind) %>% unique() 
data_tbl[sort(NA_ind), ]#locate the NA rows
#do not need to filter the NA values, construct the log return table 1st
#if any NA return is due to the NA price, then skip the row
#we only use the cumulative log returns for training, if one return is NA
#it can be left out without impairing the training

#make a log return table
return_tbl <- data.table(date = data_tbl$Date,
                         etf_1d = c(NA, diff(log(data_tbl$etf))),
                         oil_1d = c(NA, diff(log(data_tbl$oil))),
                         vix_1d = c(NA, diff(log(data_tbl$vix))),
                         gold_1d = c(NA, diff(log(data_tbl$gold))),
                         usdx_1d = c(NA, diff(log(data_tbl$usdx)))
                         )
head(return_tbl)
return_tbl[, -1][, lapply(.SD, function(j) sum(is.na(j)))]
#the NaN is counted as NA, so check NaN along for extreme data
return_tbl[, -1][, lapply(.SD, function(j) sum(is.nan(j)))]
#check the NAs and NaNs
rNAs_ind <- lapply(return_tbl, function(j) which(is.na(j)))
rNAs_ind <- unlist(rNAs_ind) %>% unique()
return_tbl[sort(rNAs_ind),]
data_tbl[sort(rNAs_ind),] #check the cause of the NaNs

#making y~x return table
#check out number of valid ys
etf_return[,-1][, lapply(.SD, function(j) length(j[-which(is.na(j))]))]


