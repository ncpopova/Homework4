Homework20220407.R

# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)

#####Problem 1#####
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.
# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.
# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.
# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.
# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.

data <- tidyquant::tq_get(c("FB", "AMZN", "NFLX"),
            get = "stock.prices",
            from = "2019-01-01",
            to   = "2021-04-01")%>%
  dplyr::select(symbol, date, adjusted)

Dates <- data.frame(Date = rep(seq.Date(from = ymd('2019-01-01'), 
                                    to = ymd('2021-04-01'), 
                                    by = 'days'), 3),
                    Symbol = c(rep("AMZN", 822),rep("FB", 822),rep("NFLX", 822)))

Join <- Dates %>%
  dplyr::left_join(data, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")

DataFrame <- Join %>%
  filter(Symbol %in% c("AMZN", "FB"),
         between(Date, ymd("2019-01-01"), ymd("2019-07-01")) | 
         between(Date, ymd("2020-04-01"), ymd("2020-07-01"))) %>%
  dplyr::arrange(Symbol, desc(Date))

a <- DataFrame %>%
 filter((Date == min(ymd(Date)) | Date == max(ymd(Date))) &
          (Symbol == "AMZN" | Symbol == "FB"))

b <- NULL
b <- DataFrame %>%
  mutate(year = substring(Date, 1, 4),
         month = substring(Date, 6, 7),
         day = substring(Date, 9, 10)) %>%
  group_by(year, month) %>%
  filter(day == max(day)) %>%
  arrange(year, month)
view(b)

#####Problem 1#####

#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.

Crossing <- DataFrame %>%
  mutate(SMA10 = SMA(adjusted, n = 10),
         SMA26 = SMA(adjusted, n = 26),
         LagSMA10 = lag(SMA10),
         LagSMA26 = lag(SMA26)) %>%
  filter(!is.na(LagSMA26)) %>%
  mutate(Crossed = case_when(LagSMA10 > LagSMA26 & SMA10 < SMA26 ~ "crossed from below",
                             LagSMA10 < LagSMA26 & SMA10 > SMA26 ~ "crossed from above",
                             TRUE ~ "no cross"),
         Signal = case_when(Crossed == "crossed from below" ~ "sell",
                            Crossed == "crossed from above" ~ "buy",
                            TRUE ~ "do nothing"))

sum(Crossing$Crossed == "crossed from below")

sum(Crossing$Crossed == "crossed from above")

#prepisah :P
#####Problem 2#####
