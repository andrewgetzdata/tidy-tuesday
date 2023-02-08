# Load the data set
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2023-02-07')

big_tech_stock_prices <- tuesdata$big_tech_stock_prices
write.csv(big_tech_stock_prices, "~/Documents/code/tidy-tuesday/datasets/big_tech_stock_prices.csv", row.names=FALSE)

big_tech_companies <- tuesdata$big_tech_companies
write.csv(big_tech_companies, "~/Documents/code/tidy-tuesday/datasets/big_tech_companies.csv", row.names=FALSE)
head(big_tech_stock_prices)

library(tidyverse)
library(tidyquant)

AAPL <- big_tech_stock_prices %>% filter(stock_symbol == 'AAPL') %>% filter(between(date, as.Date('2021-09-01'), as.Date('2022-12-29')))
end <- as_date("2022-12-29")

aapl_range_60_tbl <- AAPL %>%
  tail(60) %>%
  summarize(
    max_high = max(high),
    min_low  = min(low)
  )

AAPL %>%
  ggplot(aes(x = date, y = close, open = open,
             high = high, low = low, close = close)) +
  geom_candlestick() +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
  labs(title = "AAPL Candlestick Chart", 
       subtitle = "BBands with SMA Applied (2021.09.01 - 2022.12.19)", 
       y = "Closing Price", x = "") + 
  coord_x_date(xlim = c(end - weeks(24), end),
               ylim = c(aapl_range_60_tbl$min_low * 0.85, 
                        aapl_range_60_tbl$max_high) * 1.15) + 
  theme_tq()

