#### Libraries ####

library(arrow)
library(broom)
library(dplyr)
if(sum(as.numeric(rownames(installed.packages())=="forecast"))==0){
  install.packages("forecast")
}
library(forecast)
library(ggplot2)
library(glue)
library(haven)
library(lubridate)
library(plotly)
if(sum(as.numeric(rownames(installed.packages())=="readxl"))==0){
  install.packages("readxl")
}
library(readxl)
library(rugarch)
library(scales)
library(shiny)
library(stringr)
if(sum(as.numeric(rownames(installed.packages())=="statcanR"))==0){
  if(sum(as.numeric(rownames(installed.packages())=="devtools"))==0){
    install.packages("devtools")
  }
  devtools::install_github("warint/statcanR")
}
library(statcanR)
library(tidyverse)
if(sum(as.numeric(rownames(installed.packages())=="tseries"))==0){
  install.packages("tseries")
}
library(tseries)
if(sum(as.numeric(rownames(installed.packages())=="writexl"))==0){
  install.packages("writexl")
}
library(writexl)
options(scipen = 999)

#### Data Import ####

fppi<-statcan_download_data("32-10-0098-01", "eng")

#### Paper Method ####

fppi_time<-as.data.frame(
  str_split_fixed(fppi$REF_DATE,
                  "-",
                  3))
names(fppi_time)<-c("year","month","day")
for(j in 1:ncol(fppi_time)){fppi_time[,j]<-as.numeric(fppi_time[,j])}

# selecting for year greater than or equal to 1994 to omit shocks in 70s and 80s (Marketing Acts?)

x_fppi<-cbind(fppi,
              fppi_time) %>%
  arrange(year,
          month,
          day) %>%
  filter(year>=1994)

year_trend<-x_fppi$year-min(x_fppi$year)+1
month_trend<-((year_trend-1)*12)+x_fppi$month

x_fppi<-cbind(x_fppi,
              year_trend,
              month_trend) %>%
  mutate(
    quarter = case_when(
      month %in% 1:3 ~ paste0(year,"-Q1"),
      month %in% 4:6 ~ paste0(year,"-Q2"),
      month %in% 7:9 ~ paste0(year,"-Q3"),
      month %in% 10:12 ~ paste0(year,"-Q4"),
      TRUE ~ NA),
    quarter_date=case_when(
      month %in% 1:3 ~ format(fast_strptime(paste0(year,
                                                   "-01-01"),
                                            "%Y-%m-%d"),
                              "%B%e, %Y"),
      month %in% 4:6 ~ format(fast_strptime(paste0(year,
                                                   "-04-01"),
                                            "%Y-%m-%d"),
                              "%B%e, %Y"),
      month %in% 7:9 ~ format(fast_strptime(paste0(year,
                                                   "-07-01"),
                                            "%Y-%m-%d"),
                              "%B%e, %Y"),
      month %in% 10:12 ~ format(fast_strptime(paste0(year,
                                                     "-10-01"),
                                              "%Y-%m-%d"),
                                "%B%e, %Y")),
    quarter_trend=
      quarter(fast_strptime(as.character(REF_DATE),
                            "%Y-%m-%d"))+
      (year_trend-1)*4)

x_fppi_init<-x_fppi

#### Time Series Methods ####

x_fppi_arch<-x_fppi_init 

prov_vars<-as.data.frame(
  unique(
    cbind(x_fppi_arch$GEO,
          x_fppi_arch$`Commodity groups`)))
names(prov_vars)<-c("province","variable")
prov_vars<-prov_vars %>%
  arrange(province,
          variable)

start_year<-1994

##### Generating Deseasonalized Series #####

models_garch<-vector("list",length = nrow(prov_vars))

for(i in 1:nrow(prov_vars)){
  
  print(paste0(i," of ",nrow(prov_vars)))
  
  data<-x_fppi_arch %>%
    filter(GEO==prov_vars$province[i],
           `Commodity groups`==prov_vars$variable[i],
           !is.na(VALUE),
           year>=start_year) %>%
    arrange(month_trend)
  
  if(nrow(data)<=((year(Sys.Date())-start_year)*11)|
     sd(data$VALUE)==0){
    status<-"InsufDat"
    name<-paste0(
      prov_vars$province[i],
      "-",
      prov_vars$variable[i])
    names(models_garch)[i]<-name
    models_garch[[i]]<-list(
      name,
      status)
    names(models_garch[[i]])<-c(
      "name",
      "status")
    
    rm(data,
       status,
       name)
    
    next
  }
  
  adf_result<-adf.test(data$VALUE)
  
  sarima<-auto.arima(data$VALUE,
                     seasonal=TRUE)
  
  # sarima<-tryCatch(
  #   Arima(data$VALUE,
  #         order=c(1,0,0),
  #         seasonal=list(order=c(2,0,0),
  #                       period=12),
  #         method="ML"),
  #   error=function(e){
  #     Arima(data$VALUE,
  #           order=c(1,0,0),
  #           seasonal=list(order=c(2,0,0),
  #                         period=12),
  #           method="CSS")}) 
  
  err_sarima<-residuals(sarima)
  
  spec<-ugarchspec(
    variance.model = list(
      model="eGARCH",
      garchOrder=c(1,1)),
    mean.model = list(
      armaOrder=c(1,1),
      include.mean=TRUE),
    distribution.model = "norm")
  
  garch<-ugarchfit(spec=spec,
                   data=err_sarima,
                   solver="hybrid")
  
  model<-list(sarima,garch)
  names(model)<-c("sarima",
                  "garch")
  
  model_fitted<-as.numeric(fitted(sarima))+as.numeric(fitted(garch))
  
  data<-cbind(data,model_fitted)
  
  if(adf_result$p.value>0.05){
    status<-"NonStat"
  }
  if(adf_result$p.value<=0.05){
    status<-"Stat"
  }
  
  name<-paste0(
    prov_vars$province[i],
    "-",
    prov_vars$variable[i])
  
  models_garch[[i]]<-list(
    name,
    status,
    adf_result,
    model,
    data)
  
  names(models_garch)[i]<-name
  names(models_garch[[i]])<-c(
    "name",
    "status",
    "adf_result",
    "model",
    "data")
  
  rm(data,
     status,
     name,
     adf_result,
     spec,
     model)
  
}

for(i in 1:length(models_garch)){
  
  print(paste0(i," of ",length(models_garch)))
  
  if(models_garch[[i]]$status=="InsufDat"){next}
  
  if(!exists("data_garch")){
    data_garch<-models_garch[[i]]$data
  }
  
  if(exists("data_garch")&nrow(data_garch)>=1){
    data_garch<-data_garch %>%
      bind_rows(models_garch[[i]]$data)
  }
  
}

write_xlsx(data_garch,
           path="C:/ag-vix/commodity_price/data_garch.xlsx")

##### Analysis of Deseasonalization #####


