library(tidyverse)
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
setDT(tb1)

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
#so work on benchmark index can predict the performance of the fund

