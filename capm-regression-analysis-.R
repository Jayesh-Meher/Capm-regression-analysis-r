library(tidyverse)
library(tidyquant)

# Get data
apple <- tq_get("AAPL", from = "2020-01-01")
market <- tq_get("^GSPC", from = "2020-01-01")

# Keep only adjusted price and convert to monthly
apple_m <- apple %>%
  distinct(date, .keep_all = TRUE) %>%
  tq_transmute(select = adjusted,
               mutate_fun = to.monthly,
               indexAt = "lastof",
               col_rename = "apple_price")

market_m <- market %>%
  distinct(date, .keep_all = TRUE) %>%
  tq_transmute(select = adjusted,
               mutate_fun = to.monthly,
               indexAt = "lastof",
               col_rename = "market_price")

# Calculate log returns
apple_ret <- apple_m %>%
  mutate(apple_return = log(apple_price / lag(apple_price))) %>%
  drop_na()

market_ret <- market_m %>%
  mutate(market_return = log(market_price / lag(market_price))) %>%
  drop_na()

# Merge both datasets by date
capm_data <- inner_join(apple_ret, market_ret, by = "date")

# Run regression
model <- lm(apple_return ~ market_return, data = capm_data)
summary(model)

# Scatter plot with regression line
ggplot(capm_data, aes(x = market_return, y = apple_return)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "CAPM Regression: Apple vs Market",
       x = "Market Return (S&P 500)",
       y = "Apple Return")