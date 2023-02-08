# Load the data set
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2023-02-07')

big_tech_stock_prices <- tuesdata$big_tech_stock_prices
big_tech_companies <- tuesdata$big_tech_companies

head(big_tech_stock_prices)

library(tidyverse)
library(tidyquant)
library(ggtext)
library(ggimage)

AAPL <- big_tech_stock_prices %>% filter(stock_symbol == 'AAPL') %>% filter(between(date, as.Date('2021-09-01'), as.Date('2022-12-29')))
end <- as_date("2022-12-29")

aapl_range_60_tbl <- AAPL %>%
  tail(60) %>%
  summarize(
    max_high = max(high),
    min_low  = min(low)
  )

# Main Plot
(main_plot <- AAPL %>%
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
)



# Caption plot
(caption_plot <- ggplot() +
    annotate(
      geom = "text", x = 0, y = 0, label = "Tidytuesday Week-06 2023 \n Data from: Kaggle \n Andrew Getz,@andrewgetz",
      family = "Arial Bold",
      size = 4,
      fill = NA,
      label.size = unit(0, "pt"),
      label.r = unit(0, "pt")
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
)

main_plot +
  patchwork::inset_element(caption_plot, left = 0.66, bottom = 0, right = 0.999, top = 0.15) &
  theme(
    plot.margin = margin(c(.25, .25, .25, .25), unit = "cm")
  )





