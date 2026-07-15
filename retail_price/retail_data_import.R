###Volatility project, consumer side

#### Libraries ####

library(arrow)
library(broom)
library(glue)
library(haven)
library(plotly)
library(scales)
library(shiny)
library(ggplot2)
library(data.table)
library(xts)
library(tseries)
library(dplyr)
library(lubridate)
library(rugarch)
library(stringr)

pacman::p_load(tidyverse,  here, tsibble, feasts, gridExtra)


if(sum(as.numeric(rownames(installed.packages())=="statcanR"))==0){
  if(sum(as.numeric(rownames(installed.packages())=="devtools"))==0){
    install.packages("devtools")
  }
  devtools::install_github("warint/statcanR")
}
library(statcanR)
library(tidyverse)
options(scipen = 999)

#### Data Import ####
##If you want to work with the other datasets, you can adjust the code

#ippi_food<-statcan_download_data("18-10-0266-03", "eng")
#fppi_food<-statcan_download_data("32-10-0098-01", "eng")
#food_retail_price<-statcan_download_data("18-10-0245-02", "eng")
cpi_food<-statcan_download_data("18-10-0004-01", "eng")
#rspi<-statcan_download_data("18-10-0251-01", "eng")

#### Data Preparation ####
#replacing the variable name of interest
names(cpi_food)[names(cpi_food) == "Products and product groups"] <- "PRODUCT"

cpi_fah_df <- cpi_food %>%
  filter(GEO == "Canada", PRODUCT == "Food purchased from stores")
cpi_fah_df$REF_DATE <- as.Date(cpi_fah_df$REF_DATE)
class(cpi_fah_df$REF_DATE)


# Check for missing values in REF_DATE and VALUE columns
sum(is.na(cpi_fah_df$REF_DATE))
sum(is.na(cpi_fah_df$VALUE))

#separate out date into year, month, day
cpi_fah_df$REF_DATE<- as.Date(cpi_fah_df$REF_DATE)

cpi_fah_df$Year <- format(cpi_fah_df$REF_DATE, "%Y")
cpi_fah_df$Month <- format(cpi_fah_df$REF_DATE, "%m")
cpi_fah_df$Day <- format(cpi_fah_df$REF_DATE, "%d")

cpi_fah_df$Year <- as.numeric(cpi_fah_df$Year)
cpi_fah_df$Month <- as.numeric(cpi_fah_df$Month)

cpi_fah_df <- cpi_fah_df %>%
  filter(Year >= 1994)

# Find the earliest year in the 'year' column
earliest_year <- min(cpi_fah_df$Year)

# Create the time trend variable using the earliest year as the starting point
cpi_fah_df$time_trend_y <- cpi_fah_df$Year - earliest_year + 1

cpi_fah_df$month_trend<-((cpi_fah_df$time_trend_y-1)*12)+cpi_fah_df$Month

####Initial tests

#Augmented Dickey Fuller Test without package

#Convert your data to time series
cpi_fah_xts <- xts(cpi_fah_df$VALUE, order.by = cpi_fah_df$REF_DATE)
adf_fah_result <- adf.test(cpi_fah_xts, alternative = "stationary", k = 0)
print(adf_fah_result)

# First difference of your series
cpi_diff_fah <- diff(cpi_fah_xts)

# Remove NA values resulting from differencing the first one is na 
cpi_diff_fah <- na.omit(cpi_diff_fah)

# Run ADF test on the differenced series
adf_result_diff_fah <- adf.test(cpi_diff_fah, alternative = "stationary", k = 0)

# View the result
print(adf_result_diff_fah)

#Plot a simple graph 
plot(cpi_diff_fah, type = "l",  # "l" for line plot
     main = "Price of Food Purchased from Stores in Canada Over Time",
     xlab = "Date",
     ylab = "CPI",
     col = "blue",   # Line color
     lwd = 2,        # Line width
     cex = 0.7)      # Label size

###Mean 

mean_fah <- mean(cpi_fah_df$VALUE)
mean_fah_diff <- mean(cpi_diff_fah)

#####Standard deviations

sd_value_fah <- sd(cpi_diff_fah)

### coefficient of variation 

cv_fah<- (sd_value_fah /mean_fah_diff)*100

####Sarima to determine the parameters and specifications ##add here

###BIC 

###GARCH

####Garch package
#spec <- ugarchspec(
  #variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  #mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
  #distribution.model = "norm"  # Alternatives: "std", "ged", "snorm", etc.
#)

#fit <- ugarchfit(spec = spec, data = cpi_diff_fah)

#show(fit)          # Basic model output
#summary(fit)       # More detailed output
#plot(fit)          # Diagnostic plots

###Rugarch and Sarima

#Install and load the necessary packages:
install.packages("forecast")
install.packages("rugarch")
library(forecast)
library(rugarch)
library(ggplot2)
library(tseries)
#Fit the SARIMA model:
# Assuming your time series data is in a variable called 'ts_data'

sarima<- auto.arima(cpi_fah_df$VALUE, seasonal = TRUE)
summary(sarima)


sarima_model <- Arima(cpi_fah_df$VALUE, order=c(3,2,1), seasonal=c(1,1,1))

summary(sarima_model)

#Extract residuals from the SARIMA model:
residuals_sarima <- residuals(sarima_model)

#Specify and fit the EGARCH model:
spec <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
    distribution.model = "norm"
  )

garch_model <- ugarchfit(spec, residuals_sarima, solver="hybrid")
# Combine the models: Forecast using the SARIMA model

model<-list(sarima_model,garch_model)
names(model)<-c("sarima",
                "garch")

model_fitted<-as.numeric(fitted(sarima_model))+as.numeric(fitted(garch_model))

cpi_fah_df<-cbind(cpi_fah_df,model_fitted)


beginning_value <- cpi_fah_df$VALUE[1]
cpi_fah_df$num_months_beginning_end <-  cpi_fah_df$month_trend-1

cpi_fah_df$cagr <- NA

# Starting value is the value for the first cpi
start_value <- cpi_fah_df$VALUE[1]

# Calculate monthly CAGR using the month trend
for (i in 2:nrow(cpi_fah_df)) {
  num_months <- cpi_fah_df$month_trend[i] - 1  # Subtract 1 to avoid dividing by 0 for the first month
  
  if (num_months > 0) {
    # Monthly CAGR formula
    cpi_fah_df$cagr[i] <- (cpi_fah_df$VALUE[i] / start_value)^(1 / num_months) - 1
  }
}

cpi_fah_df$cagr[1] <- 0

cpi_fah_df$std_dev_cagr <- NA  # Create a column to store standard deviation values

# Loop to calculate the standard deviation of the growth rate for each month
for (i in 1:nrow(cpi_fah_df)) {
  # Calculate the standard deviation of the monthly CAGR values from month 1 to month 'i'
  cpi_fah_df$std_dev_cagr[i] <- sd(cpi_fah_df$cagr[1:i], na.rm = TRUE)  # Excluding NA values
}


cpi_fah_df$std_dev_cagr[1] <- 0

###calculate average cagr monthly 

average_cagr <- mean(cpi_fah_df$cagr, na.rm = TRUE)  # na.rm = TRUE ignores NA 

cpi_fah_df$average_cagr <- average_cagr

###calculate the monthly indicator of food price anomalies 

cpi_fah_df$x_IFPA <- NA

#testing calculation
#cpi_fah_df$x_IFPA[3] <- (cpi_fah_df$cagr[3] - average_cagr) / cpi_fah_df$std_dev_cagr[3]

for (i in 1:nrow(cpi_fah_df)) {
  
  if (!is.na(cpi_fah_df$mod[i])) {
    
  cpi_fah_df$x_IFPA[i] <- (cpi_fah_df$cagr[i] - average_cagr) / cpi_fah_df$std_dev_cagr[i]
  }
}

cpi_fah_df$x_IFPA[1] <- NA

filtered_data <- data %>%
  filter(REF_DATE >= as.Date("2000-01-01"))

#plot the x_IFPA
ggplot(cpi_fah_df %>% filter(REF_DATE >= as.Date("2000-01-01")), aes(x = REF_DATE, y = x_IFPA)) +
  geom_line(color = "blue") +  # Line plot for standard deviations over time
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +  # Horizontal line at 1
  geom_hline(yintercept = 0.5, color = "green", linetype = "dashed") +  # Horizontal line at 0.5
  geom_hline(yintercept = -0.5, color = "green", linetype = "dashed") +  # Horizontal line at -0.5
  labs(title = "Indicator of Food Price Anomalies.",
       x = "Time (yyyy-mm-dd)",
       y = "Indicator of Food Price Anomalies") +
  theme_minimal()

##placeholder for equation 15

##for equation 16

rt<- log(cpi_fah_df$VALUE[3])-log(cpi_fah_df$VALUE[2])
cpi_fah_df$log_return <- c(NA, diff(log(cpi_fah_df$VALUE)))

