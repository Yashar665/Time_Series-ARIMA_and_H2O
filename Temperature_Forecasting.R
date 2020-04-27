library(tidyverse)
library(lubridate)
library(hrbrthemes) #remotes::install_github("hrbrmstr/hrbrthemes")
library(plotly)
library(highcharter)
library(htmltools)
library(timetk)
library(skimr)
library(forecast)
library(h2o)

df <- read_csv('C:/Users/Yashar/Desktop/Data Science Bootcamp/R programming/Week 10/daily-minimum-temperatures-in-me.csv') 

df <- df %>% na.omit()
colnames(df) <- c('Date','Count')

df %>% glimpse()
df <- df %>% separate(col = Date,into = c("m","d","y"),sep = "/") %>% 
             mutate(Date = as.Date(paste(y,m,d,sep = "/")),Count = as.double(Count)) %>%
             group_by(m,y) %>% summarise(Average_temp = mean(Count,na.rm = T)) %>% 
             as.data.frame() %>% mutate(Date = as.Date(paste0(y,"-",m,"-01"))) %>% 
             select(Date,Average_temp) %>% arrange(Date)


time_plot <- df %>%
  group_by(Date) %>%
  summarise(total_count = sum(Average_temp)) %>%
  ungroup() %>%
  ggplot(aes(Date, total_count)) +
  geom_line(color = "green") +
  geom_point(color = "red", size = 0.5) +
  geom_smooth(colour = "red", fill = "black") +
  theme_modern_rc() +
  scale_x_date(date_breaks = "1 year") +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = seq(0,1000,20)) +
  labs(title = "Average monthly temperatures",
       x = "", y = "")

time_plot %>% ggplotly()


# H2O ----

all_time_arg <- df %>% tk_augment_timeseries_signature()

all_time_arg %>% skim()

# Data Cleaning
all_time_arg$diff[is.na(all_time_arg$diff)] <- 0
all_time_arg <- all_time_arg %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

# Split
train <- all_time_arg %>% filter(year < 1987)
valid <- all_time_arg %>% filter(year >= 1987, year < 1989)
test  <- all_time_arg %>% filter(year >= 1989)

h2o.init()    
train_h2o <- train %>% as.h2o()
valid_h2o <- valid %>% as.h2o()
test_h2o  <- test %>% as.h2o()

y <- "Average_temp"
x <- train %>% select(-Average_temp) %>% names()

model <- h2o.automl(
  x = x, y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o,
  stopping_metric = "RMSE",
  max_runtime_secs = 180, 
  exclude_algos = c("DRF", "GBM","GLM",'XGBoost')) 
#yalniz DEEP-LEARNING alqoritmi Time series ile yaxshi netice gosterir

model@leaderboard %>% as_tibble() 
automl_leader <- model@leader 

pred_h2o <- automl_leader %>% h2o.predict(newdata = test_h2o) 

automl_leader %>% h2o.performance(newdata = test_h2o)


error_tbl <- df %>% 
  filter(lubridate::year(Date) >= 1989) %>% 
  add_column(pred = pred_h2o %>% 
               as_tibble() %>% 
               pull(predict)) %>%
  rename(actual = Average_temp) %>% 
  mutate(
    error = actual - pred,
    error_pct = error / actual 
  ) 

v1 <- highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='H2O AUTOML')


# ARIMA (Auto-Regressive Integrated Moving-Average) ----

# Split
df[1:96,] -> train_forecast 
df[97:120,] -> test_forecast

# Transformate data for ARIMA - (time series data format)
train_forecast_arima <- train_forecast$Average_temp %>% tk_ts(start = 1981, freq = 12) 
test_forecast_arima <- test_forecast$Average_temp %>% tk_ts(start = 1989, freq = 12)

# Components of this time series
train_forecast_arima %>% decompose() %>% plot()
test_forecast_arima %>% decompose() %>% plot()
# Observed – the actual data plot
# Trend – the overall upward or downward movement of the data points
# Seasonal – any monthly/yearly pattern of the data points
# Random – unexplainable part of the data


train_forecast_arima %>% ets() %>% # Error Trend Seasonality - (automatic selection of exponential and ARIMA models)
  forecast(h = 24, level = 95) %>% # 48 / 12 = 4years
  as_tibble() -> comparison

test_forecast_arima %>% 
  tk_tbl() %>% 
  mutate(predicted = comparison$`Point Forecast`) %>% 
  mutate(
    error = value - predicted,
    error_pct = error / value
  ) -> errors

errors$error^2 %>% mean() %>% sqrt()

v2 <- highchart() %>% 
  hc_xAxis(categories = df[97:120,]$Date) %>% 
  hc_add_series(data=errors$value, type='line',color='red', name='Actual') %>% 
  hc_add_series(data=errors$predicted, type='line',color='green', name='Predicted') %>% 
  hc_title(text='AUTO-ARIMA')

hw_grid(v1,v2) %>% browsable()


# New predictions for H2O ----

data <- seq(as.Date("1991/01/01"), as.Date("1991/12/01"), "months") %>% 
  as.data.frame() %>% 
  add_column(Count=c(0)) # new data for next year

colnames(data) <- c('Date','Count')

validation <- data %>% tk_augment_timeseries_signature()

validation$diff[is.na(validation$diff)] <- 0
validation <- validation %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

validation_h2o <- validation %>% as.h2o() 

# predict new data with h2o model
new_predictions <- automl_leader %>% 
  h2o.predict(newdata = validation_h2o) %>% 
  as_tibble() %>%
  add_column(Date = data$Date) %>% 
  select(Date,predict) 

colnames(new_predictions) <- c('Date','Average_temp')


# Bind old & new datas for visualization
new_predictions <- df %>% 
  bind_rows(new_predictions) %>% 
  as_tibble() %>% 
  mutate(colors = c(rep('Actual',120), rep('Predicted',12))) 

v3 <- hchart(new_predictions, "line", hcaes(Date, Average_temp, group = new_predictions$colors)) %>% 
  hc_title(text='H2O Forecast') %>% 
  hc_colors(colors = c('red','green'))


# New predictions for ARIMA ----

df_arima <- df$Average_temp %>% tk_ts(start = 1981, freq = 12)

v4 <- df_arima %>% 
  ets() %>% 
  forecast(h = 12, level = 90) %>% # 48 / 12 = 4years
  hchart() %>% 
  hc_title(text='ARIMA Forecast') %>% 
  hc_colors(colors = c('red','green'))

hw_grid(v1,v2,v3,v4) %>% 
  browsable()
