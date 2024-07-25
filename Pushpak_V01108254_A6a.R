# Plot the data
ggplot(data.frame(Date = index(df), Adj.Close = df), aes(x = Date, y = Adj.Close)) +
  geom_line() +
  ggtitle('tesladata.NS Adj Close Price') +
  xlab('Date') +
  ylab('Adj Close Price') +
  theme_minimal()

# Decompose the time series
decomposed <- decompose(ts(df, frequency = 12), type = "multiplicative")
autoplot(decomposed)

# Split the data into training and test sets
trainIndex <- createDataPartition(y = df, p = 0.8, list = FALSE)
train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]

# Resample data to monthly frequency
monthly_data <- to.monthly(df, indexAt = "lastof", OHLC = FALSE)
trainIndex <- createDataPartition(y = monthly_data, p = 0.8, list = FALSE)
train_data <- monthly_data[trainIndex,]
test_data <- monthly_data[-trainIndex,]

# Fit Holt-Winters model and forecast
holt_winters_model <- HoltWinters(ts(train_data, frequency = 12), seasonal = "multiplicative")
holt_winters_forecast <- forecast(holt_winters_model, h = 12)

# Convert xts objects to data frames
train_data_df <- data.frame(Date = index(train_data), Cl = coredata(train_data))
test_data_df <- data.frame(Date = index(test_data), Cl = coredata(test_data))