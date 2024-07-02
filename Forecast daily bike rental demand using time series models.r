

---
title: "Forecast daily bike rental demand using time series models"
date: "r Sys.Date()"
output: html_document
author: "Put your name!"
---

# About Data Analysis Report

This RMarkdown file contains the report of the data analysis done for the project on forecasting daily bike rental demand using time series models in R. It contains analysis such as data exploration, summary statistics and building the time series models. The final report was completed on r date(). 

**Data Description:**

This dataset contains the daily count of rental bike transactions between years 2011 and 2012 in Capital bikeshare system with the corresponding weather and seasonal information.

**Data Source:** [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset)

**Relevant Paper:** 

Fanaee-T, Hadi, and Gama, Joao. Event labeling combining ensemble detectors and background knowledge, Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg



# Task One: Load and explore the data

## Load data and install packages

## Import required packages
```{r setup, include=FALSE}
# Suppress warnings and messages
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
# Install required packages if not already installed
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(timetk)) install.packages("timetk")
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")

# Load required packages
library(tidyverse)
library(lubridate)
library(timetk)
library(forecast)
library(tseries)

# Load the dataset from local file
bike_data <- read.csv("day.csv")

# View the first few rows of the dataset
head(bike_data)


```
## Describe and explore the data

```{r}
# Summarize the dataset
summary(bike_data)

# Check the structure of the dataset
str(bike_data)

# Convert date column to Date type
bike_data$dteday <- as.Date(bike_data$dteday)

# Plot the daily count of bike rentals
ggplot(bike_data, aes(x = dteday, y = cnt)) +
  geom_line(color = "blue") +
  labs(title = "Daily Bike Rentals", x = "Date", y = "Count of Bike Rentals")



```
# Task Two: Create interactive time series plots

```{r}
## Read about the timetk package
# ?timetk

# Use timetk package to create an interactive plot
bike_data %>%
  tk_tbl() %>%
  plot_time_series(dteday, cnt, .interactive = TRUE, .title = "Interactive Plot: Daily Bike Rentals")


```

# Task Three: Smooth time series data

```{r}

# Smooth the time series using a moving average
bike_data <- bike_data %>%
  mutate(cnt_ma = ma(cnt, order = 7))

# Plot the smoothed data
ggplot(bike_data, aes(x = dteday)) +
  geom_line(aes(y = cnt, color = "Original")) +
  geom_line(aes(y = cnt_ma, color = "Smoothed")) +
  labs(title = "Daily Bike Rentals with Moving Average Smoothing", x = "Date", y = "Count of Bike Rentals") +
  scale_color_manual(values = c("Original" = "blue", "Smoothed" = "red"))


```
# Task Four: Decompose and assess the stationarity of time series data

```{r}

# Decompose the time series
bike_ts <- ts(bike_data$cnt, start = c(2011, 1), frequency = 365)
decomposed_ts <- stl(bike_ts, s.window = "periodic")
plot(decomposed_ts)

# Assess stationarity using Augmented Dickey-Fuller test
adf.test(bike_ts)


```
# Task Five: Fit and forecast time series data using ARIMA models

```{r}
# Fit an ARIMA model
auto_arima_model <- auto.arima(bike_ts)
summary(auto_arima_model)

# Forecast the next 30 days
forecast_arima <- forecast(auto_arima_model, h = 30)
plot(forecast_arima)


```

# Task Six: Findings and Conclusions





