library(tidyverse)
library(lubridate)
library(data.table)
library(caret)
library(rvest)
library(readxl)
library(doParallel)

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

#yahoo finance
#query1.finance.yahoo.com/v7/finance/download/IEO?period1=1147046400&period2=1667952000&interval=1d&events=history&includeAdjustedClose=true
#
yahoo_url <- paste0("query1.finance.yahoo.com/v7/finance/download/",
                    "IEO",
                    "?period1=1147046400",
                    "&period2=1667952000",
                    "&interval=1d",
                    "&events=history&includeAdjustedClose=true")
download.file(paste0("query1.finance.yahoo.com/v7/finance/download/",
                     "IEO",
                     "?period1=1592179200",
                     "&period2=1592524800",
                     "&interval=1d",
                     "&events=history",
                     "./004243/data/sample.csv"))
#period time is in unix timestamp form
start_date <- as.numeric(as.Date("2006-05-06"))
end_date <- as.numeric(today())

djsoep_etf <- "CL=F"
oil_index <- "IEO"
gold_index <- "GC=F"
vis <- "^VIX"
usdx <- "DXYN"



