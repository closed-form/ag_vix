#### Libraries ####

library(arrow)
library(broom)
library(dplyr)
library(ggplot2)
library(glue)
library(haven)
library(lubridate)
library(plotly)
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
options(scipen = 999)

source("C:/ag-vix/commodity_price/commodity_ts_analysis.R")

x_fppi<-read_xlsx(path="C:/ag-vix/commodity_price/data_garch.xlsx",
                  sheet="Sheet1")

# #### Data Import ####
# 
# fppi<-statcan_download_data("32-10-0098-01", "eng")
# 
# #### Paper Method ####
# 
# fppi_time<-as.data.frame(
#   str_split_fixed(fppi$REF_DATE,
#                   "-",
#                   3))
# names(fppi_time)<-c("year","month","day")
# for(j in 1:ncol(fppi_time)){fppi_time[,j]<-as.numeric(fppi_time[,j])}
# 
# # selecting for year greater than or equal to 1994 to omit shocks in 70s and 80s (Marketing Acts?)
# 
# x_fppi<-cbind(fppi,
#               fppi_time) %>%
#   arrange(year,
#           month,
#           day) %>%
#   filter(year>=1994)
# 
# year_trend<-x_fppi$year-min(x_fppi$year)+1
# month_trend<-((year_trend-1)*12)+x_fppi$month
# 
# x_fppi<-cbind(x_fppi,
#               year_trend,
#               month_trend) %>%
#   mutate(
#     quarter = case_when(
#       month %in% 1:3 ~ paste0(year,"-Q1"),
#       month %in% 4:6 ~ paste0(year,"-Q2"),
#       month %in% 7:9 ~ paste0(year,"-Q3"),
#       month %in% 10:12 ~ paste0(year,"-Q4"),
#       TRUE ~ NA),
#     quarter_date=case_when(
#       month %in% 1:3 ~ format(fast_strptime(paste0(year,
#                                                    "-01-01"),
#                                             "%Y-%m-%d"),
#                               "%B%e, %Y"),
#       month %in% 4:6 ~ format(fast_strptime(paste0(year,
#                                                    "-04-01"),
#                                             "%Y-%m-%d"),
#                               "%B%e, %Y"),
#       month %in% 7:9 ~ format(fast_strptime(paste0(year,
#                                                    "-07-01"),
#                                             "%Y-%m-%d"),
#                               "%B%e, %Y"),
#       month %in% 10:12 ~ format(fast_strptime(paste0(year,
#                                                      "-10-01"),
#                                               "%Y-%m-%d"),
#                                 "%B%e, %Y")),
#     quarter_trend=
#       quarter(fast_strptime(as.character(REF_DATE),
#                                         "%Y-%m-%d"))+
#       (year_trend-1)*4)

x_fppi_init<-x_fppi

prov_vars<-as.data.frame(
  unique(
    cbind(x_fppi$GEO,
          x_fppi$`Commodity groups`)))
names(prov_vars)<-c("province","variable")
prov_vars<-prov_vars %>%
  arrange(province,
          variable)

##### Montly Calculations #####

for(i in 1:nrow(prov_vars)){
  
  a<-x_fppi %>%
    filter(GEO==prov_vars$province[i] &
           `Commodity groups`==prov_vars$variable[i] &
           !is.na(VALUE)) %>%
    arrange(year,
            month,
            day)
  
  if(nrow(a)==0){next}
  
  xcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      xcgr[t]<-((a$VALUE[t]/a$VALUE[t-1])^
        (1/(a$month_trend[t]-a$month_trend[t-1])))-1
    }
  }
  
  a<-cbind(a,
           xcgr)
  
  ryt<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      ryt[t]<-log((a$VALUE[t]/a$VALUE[t-1]),
                  base = exp(1))
    }
  }
  
  a<-cbind(a,
           ryt)
  
  sd_ryt<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>2){
      sd_ryt[t]<-sd(a$ryt[1:t])
    }
  }
  
  a<-cbind(a,
           sd_ryt)
  
  vxcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    vxcgr[t]<-(1-a$sd_ryt[t])*a$xcgr[t]
  }
  
  a<-cbind(a,
           vxcgr)
  
  mean_vxcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t==1){mean_vxcgr[t]<-a$vxcgr[t]}
    
    if(t>1){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      mean_vxcgr[t]<-sum(w*a$vxcgr[1:t])
    }
    
  }
  
  a<-cbind(a,
           mean_vxcgr)
  
  sd_vxcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=2){sd_vxcgr[t]<-a$vxcgr[t]}
    
    if(t>2){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      sd_vxcgr[t]<-(sum(w*(a$vxcgr[1:t]-a$mean_vxcgr[t])^2))^0.5
    }
    
  }
  
  a<-cbind(a,
           sd_vxcgr)
  
  x_ifpa<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=1){x_ifpa[t]<-a$vxcgr[t]}
    
    if(t>1){
      x_ifpa[t]<-(a$vxcgr[t]-a$mean_vxcgr[t])/a$sd_vxcgr[t]
    }
    
  }
  
  a<-cbind(a,
           x_ifpa)
  
  if(i==1){
    x_fppi_temp<-a
  }
  
  if(i>1){
    x_fppi_temp<-rbind(x_fppi_temp,
                       a)
  }
  
}

x_fppi<-x_fppi_temp %>%
  mutate(method="Monthly")

##### Quarterly Calculations #####

x_fppi_grouped<-x_fppi_init %>%
  group_by(GEO,
           DGUID,
           `Commodity groups`,
           UOM,
           UOM_ID,
           SCALAR_FACTOR,
           SCALAR_ID,
           VECTOR,
           COORDINATE,
           # STATUS,
           SYMBOL,
           TERMINATED,
           DECIMALS,
           INDICATOR,
           year,
           quarter,
           quarter_date,
           quarter_trend) %>%
  summarise(VALUE=mean(VALUE),
            N=n(),
            STATUS="GROUPED") %>%
  ungroup() %>%
  select(GEO:COORDINATE,
         VALUE,N,
         STATUS,
         SYMBOL:quarter_trend)

prov_vars<-as.data.frame(
  unique(
    cbind(x_fppi_grouped$GEO,
          x_fppi_grouped$`Commodity groups`)))
names(prov_vars)<-c("province","variable")
prov_vars<-prov_vars %>%
  arrange(province,
          variable)

for(i in 1:nrow(prov_vars)){
  
  a<-x_fppi_grouped %>%
    filter(GEO==prov_vars$province[i] &
             `Commodity groups`==prov_vars$variable[i] &
             !is.na(VALUE)) %>%
    arrange(year,
            quarter)
  
  if(nrow(a)==0){next}
  
  xcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      xcgr[t]<-((a$VALUE[t]/a$VALUE[t-1])^
                  (1/(a$quarter_trend[t]-a$quarter_trend[t-1])))-1
    }
  }
  
  a<-cbind(a,
           xcgr)
  
  ryt<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      ryt[t]<-log((a$VALUE[t]/a$VALUE[t-1]),
                  base = exp(1))
    }
  }
  
  a<-cbind(a,
           ryt)
  
  sd_ryt<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>2){
      sd_ryt[t]<-sd(a$ryt[1:t])
    }
  }
  
  a<-cbind(a,
           sd_ryt)
  
  vxcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    vxcgr[t]<-(1-a$sd_ryt[t])*a$xcgr[t]
  }
  
  a<-cbind(a,
           vxcgr)
  
  mean_vxcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t==1){mean_vxcgr[t]<-a$vxcgr[t]}
    
    if(t>1){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      mean_vxcgr[t]<-sum(w*a$vxcgr[1:t])
    }
    
  }
  
  a<-cbind(a,
           mean_vxcgr)
  
  sd_vxcgr<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=2){sd_vxcgr[t]<-a$vxcgr[t]}
    
    if(t>2){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      sd_vxcgr[t]<-(sum(w*(a$vxcgr[1:t]-a$mean_vxcgr[t])^2))^0.5
    }
    
  }
  
  a<-cbind(a,
           sd_vxcgr)
  
  x_ifpa<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=1){x_ifpa[t]<-a$vxcgr[t]}
    
    if(t>1){
      x_ifpa[t]<-(a$vxcgr[t]-a$mean_vxcgr[t])/a$sd_vxcgr[t]
    }
    
  }
  
  a<-cbind(a,
           x_ifpa)
  
  if(i==1){
    x_fppi_temp<-a
  }
  
  if(i>1){
    x_fppi_temp<-rbind(x_fppi_temp,
                       a)
  }
  
}

x_fppi_grouped<-x_fppi_temp %>%
  mutate(method="Quarterly")

x_fppi<-bind_rows(x_fppi,
                  x_fppi_grouped)

rm(fppi_time,
   month_trend,
   year_trend,
   j,
   i,
   t,
   a,
   xcgr,
   ryt,
   sd_ryt,
   vxcgr,
   w,
   mean_vxcgr,
   sd_vxcgr,
   x_ifpa,
   x_fppi_temp)

write.csv(x_fppi,
          file = "C:/ag-vix/commodity_price/x_fppi.csv",
          row.names = FALSE)
