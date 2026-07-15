#### Settings ####

# add to the selected_vars/provs vector(s) if you want to select specific ones
# otherwise, leave the list(s) empty; looks like "selected_vars/provs<-c()"

selected_vars<-c("Total index",
                 "Total crops",
                 "Total livestock and animal products",
                 "Food",
                 "Food purchased from restaurants",
                 "Food purchased from stores",
                 "Meat",
                 "Bakery and cereal products (excluding baby food)",
                 "Dairy products",
                 "Eggs",
                 "Fresh fruit and vegetables")

# selected_vars<-c()

selected_provs<-c("Canada")

# selected_provs<-c()

#### Libraries ####

library(arrow)
library(broom)
library(dplyr)
if(sum(as.numeric(rownames(installed.packages())=="forecast"))==0){
  install.packages("forecast")
}
library(forecast)
if(sum(as.numeric(rownames(installed.packages())=="furrr"))==0){
  install.packages("furrr")
}
library(furrr)
library(ggplot2)
library(glue)
library(haven)
library(lubridate)
if(sum(as.numeric(rownames(installed.packages())=="parallel"))==0){
  install.packages("parallel")
}
library(plotly)
if(sum(as.numeric(rownames(installed.packages())=="purrr"))==0){
  install.packages("purrr")
}
library(purrr)
if(sum(as.numeric(rownames(installed.packages())=="readxl"))==0){
  install.packages("readxl")
}
library(readxl)
if(sum(as.numeric(rownames(installed.packages())=="rugarch"))==0){
  install.packages("rugarch")
}
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

fppi<-statcan_download_data("32-10-0098-01", "eng") %>%
  mutate(source="FPPI")

cpi<-statcan_download_data("18-10-0004-01", "eng")
cpi_vars<-as.data.frame(
  read.csv(file = "C:/ag-vix/retail_price/vars_list.csv",
           header = TRUE))
names(cpi_vars)<-"food_variable"
cpi<-cpi %>%
  filter(`Products and product groups` %in% cpi_vars$food_variable,
         TERMINATED!="t",
         str_length(UOM)==8) %>%
  rename("Commodity groups"=`Products and product groups`) %>%
  mutate(source="CPI")
rm(cpi_vars)

index_table<-bind_rows(fppi,
                       cpi)

#### Data Pre-Processing  ####

index_table_time<-as.data.frame(
  str_split_fixed(index_table$REF_DATE,
                  "-",
                  3))
names(index_table_time)<-c("year","month","day")
for(j in 1:ncol(index_table_time)){
  index_table_time[,j]<-as.numeric(index_table_time[,j])
}

# selecting for year greater than or equal to 1994 to omit shocks in 70s and 80s (Marketing Acts?)

start_year<-max(index_table_time$year)-42

x_index_table<-cbind(index_table,
                     index_table_time) %>%
  arrange(year,
          month,
          day) %>%
  filter(year>=(start_year))

rm(index_table_time)

year_trend<-x_index_table$year-min(x_index_table$year)+1
month_trend<-((year_trend-1)*12)+x_index_table$month

x_index_table<-cbind(x_index_table,
                     year_trend,
                     month_trend)

rm(month_trend,
   year_trend,
   j)

#### Stepwise Calculation of XIFPA ####

##### Setup for loop #####

x_index_table_init<-x_index_table

prov_vars<-as.data.frame(
  unique(
    cbind(x_index_table_init$GEO,
          x_index_table_init$`Commodity groups`)))
names(prov_vars)<-c("province","variable")
prov_vars<-prov_vars %>%
  arrange(province,
          variable)

if(!is.null(selected_vars)){
  
  prov_vars<-prov_vars %>%
    filter(variable %in% c(selected_vars))
  
}

if(!is.null(selected_provs)){
  
  prov_vars<-prov_vars %>%
    filter(province %in% c(selected_provs))
  
}

##### Beginning Stepwise Calculations #####

ptm_loop<-proc.time()[3]

plan(multisession)

for(p in 1:nrow(prov_vars)){

  x_index_table_arch<-x_index_table_init %>%
    filter(GEO==prov_vars$province[p],
           `Commodity groups`==prov_vars$variable[p])
  
  prov_var<-paste0(prov_vars$province[p],"-",prov_vars$variable[p])
  
  ###### Generating Deseasonalized Series ######
  
  timepoints<-x_index_table_arch %>%
    filter(year>=start_year+10) %>%
    select(month_trend) %>%
    arrange(month_trend)
  
  x_index_table_arch_grouped<-list()
  
  for(i in 1:nrow(timepoints)){
    x_index_table_arch_grouped_tmp<-x_index_table_arch %>%
      filter(month_trend<=timepoints$month_trend[i],
             !is.na(VALUE))
    x_index_table_arch_grouped[[i]]<-x_index_table_arch_grouped_tmp
  }
  
  rm(i,
     timepoints,
     x_index_table_arch_grouped_tmp)
  
  sgarch_modelling<-function(x){
    
    nulldat<-cbind(`REF_DATE`=rep(NA,120),
                   `GEO`=rep(NA,120),
                   `DGUID`=rep(NA,120),
                   `Commodity groups`=rep(NA,120),
                   `UOM`=rep(NA,120),
                   `UOM_ID`=rep(NA,120),
                   `SCALAR_FACTOR`=rep(NA,120),
                   `SCALAR_ID`=rep(NA,120),
                   `VECTOR`=rep(NA,120),
                   `COORDINATE`=rep(NA,120),
                   `VALUE`=rep(NA,120),
                   `STATUS`=rep(NA,120),
                   `SYMBOL`=rep(NA,120),
                   `TERMINATED`=rep(NA,120),
                   `DECIMALS`=rep(NA,120),
                   `INDICATOR`=rep(NA,120),
                   `year`=rep(NA,120),
                   `month`=rep(NA,120),
                   `day`=rep(NA,120),
                   `year_trend`=rep(NA,120),
                   `month_trend`=rep(NA,120)) %>%
      as.data.frame()
    
    data<-x %>%
      filter(!is.na(VALUE))
    
    smonth<-max(data$month_trend)-(10*12)+1
    
    data<-data %>%
      filter(month_trend>=smonth)
    
    if(is.null(nrow(data))){
      name<-NULL
      status<-"NoDat"
      adf_result<-NULL
      model<-NULL
      data<-nulldat
      model_output<-list(
        name,
        status,
        adf_result,
        model,
        data)
      names(model_output)<-c(
        "name",
        "status",
        "adf_result",
        "model",
        "data")

      return(model_output)
    }

    if(nrow(data)==0){
      name<-NULL
      status<-"NoDat"
      adf_result<-NULL
      model<-NULL
      data<-nulldat
      model_output<-list(
        name,
        status,
        adf_result,
        model,
        data)
      names(model_output)<-c(
        "name",
        "status",
        "adf_result",
        "model",
        "data")

      return(model_output)
    }

    if((nrow(data)<100)|
       (sd(data$VALUE)==0)){
      status<-"InsufDat"
      name<-paste0(
        data$GEO[1],
        "-",
        data$`Commodity groups`[1])
      adf_result<-NULL
      model<-NULL
      data<-nulldat
      model_output<-list(
        name,
        status,
        adf_result,
        model,
        data)
      names(model_output)<-c(
        "name",
        "status",
        "adf_result",
        "model",
        "data")

      return(model_output)
    }
    
    adf_result<-adf.test(data$VALUE)

    if(!(is.null(nrow(data))) &
       !(nrow(data)==0) &
       !(nrow(data)<100) &
       !(sd(data$VALUE)==0)){
      
      name<-paste0(
        data$GEO[1],
        "-",
        data$`Commodity groups`[1])

      sarima<-auto.arima(data$VALUE,
                         seasonal=TRUE,
                         seasonal.test.args = list(m=12))

      err_sarima<-residuals(sarima)

      spec<-ugarchspec(
        variance.model = list(
          model="eGARCH",
          garchOrder=c(1,1)),
        mean.model = list(
          armaOrder=c(1,1),
          include.mean=TRUE),
        distribution.model = "norm")
      
      garch<-tryCatch(
        {ugarchfit(spec=spec,
                   data=err_sarima,
                   solver="hybrid",
                   solver.control = list(tol = 1e-6))},
        error=function(e){return("ERROR")}
      )
      
      if(is.character(garch)){
        
        status<-"BadDat"
        name<-paste0(
          data$GEO[1],
          "-",
          data$`Commodity groups`[1])
        adf_result<-NULL
        model<-NULL
        data<-nulldat
        model_output<-list(
          name,
          status,
          adf_result,
          model,
          data)
        names(model_output)<-c(
          "name",
          "status",
          "adf_result",
          "model",
          "data")
        
        return(model_output)
      }

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

      model_output<-list(
        name,
        status,
        adf_result,
        model,
        data)

      names(model_output)<-c(
        "name",
        "status",
        "adf_result",
        "model",
        "data")
      
      return(model_output)
    }
    
  }
  
  cat(paste0("\n",prov_var," - ",p,
             " of ",nrow(prov_vars),
             " - Estimating SARIMA+GARCH\n"))
  
  ptm<-proc.time()[3]
  
  safe_sgarch_modelling<-possibly(sgarch_modelling,
                                  otherwise = NULL)

  models_garch<-future_map(x_index_table_arch_grouped,
                           safe_sgarch_modelling,
                           .options = furrr_options(
                             packages = c("tidyverse",
                                          "tseries",
                                          "forecast",
                                          "rugarch"),
                             seed = TRUE))
  
  # models_garch<-list()
  # 
  # for(i in 1:length(x_index_table_arch_grouped)){
  # 
  #   models_garch[[i]]<-sgarch_modelling(x_index_table_arch_grouped[[i]])
  # 
  # }
  
  cat(paste0(round(proc.time()[3]-ptm,0),
             " Seconds to Complete Estimation"))
  
  cat(paste0("\n",prov_var," - ",p,
             " of ",nrow(prov_vars),
             " - Knitting Datasets\n\n"))

  for(i in 1:length(models_garch)){
    
    if(is.null(models_garch[[i]])){next}
    
    if(models_garch[[i]]$status=="InsufDat"|
       models_garch[[i]]$status=="NoDat"|
       models_garch[[i]]$status=="BadDat"){next}
    
    a<-models_garch[[i]]$data
    
    if(i==1){point<-i}
    if(i>1){point<-i-1}
    
    if(is.null(models_garch[[point]])){
      a<-a %>%
        filter(month_trend>=(max(a$month_trend)-1))
    }
    
    if(!is.null(models_garch[[point]])){
      if(models_garch[[point]]$status=="InsufDat"|
         models_garch[[point]]$status=="NoDat"|
         models_garch[[point]]$status=="BadDat"){
        a<-a %>%
          filter(month_trend>=(max(a$month_trend)-1))
      }
    }
    
    if(!is.null(models_garch[[point]])){
      if(models_garch[[point]]$status!="InsufDat"|
         models_garch[[point]]$status!="NoDat"|
         models_garch[[point]]$status!="BadDat"){
        a<-a %>%
          filter(month_trend>=(max(a$month_trend)))
      }
    }
    
    if(!exists("data_garch")){
      data_garch<-a
      next
    }
    
    if(exists("data_garch")&nrow(data_garch)>=1){
      
      if(i<length(models_garch)){
        
        data_garch<-data_garch %>%
          bind_rows(a)
        
      }
      
      if(i==length(models_garch)){
        
        span_fc<-month(today())-month(max(models_garch[[i]]$data$REF_DATE))
        
        sarima_fc<-models_garch[[i]]$model$sarima
        sarima_fc<-as.numeric(forecast(sarima_fc,span_fc)$mean)
        
        garch_fc<-models_garch[[i]]$model$garch
        garch_fc<-ugarchforecast(garch_fc,n.ahead=span_fc)
        garch_fc<-as.numeric(fitted(garch_fc))
        
        model_fitted_fc<-sarima_fc+garch_fc
        
        rows_fc<-a
        
        for(s in 1:(span_fc-1)){
          rows_fc<-rows_fc %>%
            bind_rows(a)
        }
        
        rows_fc<-rows_fc %>%
          mutate(REF_DATE=a$REF_DATE[1] %m+% months(seq(1,span_fc,1)),
                 year=year(a$REF_DATE[1] %m+% months(seq(1,span_fc,1))),
                 month=month(a$REF_DATE[1] %m+% months(seq(1,span_fc,1)))) %>%
          mutate(year_trend=year-start_year+1,
                 month_trend=month_trend+seq(1,nrow(rows_fc),1))
        rows_fc$model_fitted<-model_fitted_fc
        
        data_garch<-data_garch %>%
          bind_rows(a,
                    rows_fc)
        
        data_garch<-data_garch %>%
          mutate(fc_start=max(a$REF_DATE))
        
        rm(sarima_fc,
           garch_fc,
           model_fitted_fc,
           rows_fc,
           s)
        
      }

    }
    
  }
  
  rm(x_index_table_arch,
     x_index_table_arch_grouped,
     i,
     a,
     ptm,
     point)
  
  ###### Calculating XIPFA at Each Step ######
  
  if(!exists("data_garch")){next}
  
  data_garch<-data_garch %>%
    distinct(month_trend,
             .keep_all = TRUE)
  
  a<-data_garch %>%
    filter(!is.na(model_fitted)) %>%
    arrange(year,
            month,
            day) %>%
    mutate(
      model_fitted_lag12=lag(
        model_fitted,
        n = 12,
        order_by = month_trend),
      month_trend_lag12=lag(
        month_trend,
        n = 12,
        order_by = REF_DATE)) %>%
    filter(!is.na(model_fitted_lag12)|
           !is.na(month_trend_lag12))
  
  if(nrow(a)==0){next}
  
  # year over year xcgr calculation
  
  xcgr_y<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      xcgr_y[t]<-((a$model_fitted[t]/a$model_fitted_lag12[t])^
                  (1/(a$month_trend[t]-a$month_trend_lag12[t])))-1
    }
  }
  
  xcgr_y<-replace_na(xcgr_y,0)
  
  a<-cbind(a,
           xcgr_y)
  
  ryt_y<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      ryt_y[t]<-log((a$model_fitted[t]/a$model_fitted_lag12[t]),
                  base = exp(1))
    }
  }
  
  ryt_y<-replace_na(ryt_y,0)
  
  a<-cbind(a,
           ryt_y)
  
  sd_ryt_y<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>2){
      sd_ryt_y[t]<-sd(a$ryt_y[1:t])
    }
  }
  
  sd_ryt_y<-replace_na(sd_ryt_y,0)
  
  a<-cbind(a,
           sd_ryt_y)
  
  vxcgr_y<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    vxcgr_y[t]<-(1-a$sd_ryt_y[t])*a$xcgr_y[t]
  }
  
  vxcgr_y<-replace_na(vxcgr_y,0)
  
  a<-cbind(a,
           vxcgr_y)
  
  mean_vxcgr_y<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t==1){mean_vxcgr_y[t]<-a$vxcgr_y[t]}
    
    if(t>1){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      mean_vxcgr_y[t]<-sum(w*a$vxcgr_y[1:t])
    }
    
  }
  
  mean_vxcgr_y<-replace_na(mean_vxcgr_y,0)
  
  a<-cbind(a,
           mean_vxcgr_y)
  
  sd_vxcgr_y<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=2){sd_vxcgr_y[t]<-mean(a$vxcgr_y[1:t]^2)^0.5}
    
    if(t>2){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      sd_vxcgr_y[t]<-(sum(w*((a$vxcgr_y[1:t]-a$mean_vxcgr_y[t])^2)))^0.5
    }
    
  }
  
  sd_vxcgr_y<-replace_na(sd_vxcgr_y,0)
  
  a<-cbind(a,
           sd_vxcgr_y)
  
  x_ifpa_y<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=1){x_ifpa_y[t]<-a$vxcgr_y[t]}
    
    if(t>1){
      x_ifpa_y[t]<-(a$vxcgr_y[t]-a$mean_vxcgr_y[t])/a$sd_vxcgr_y[t]
    }
    
  }
  
  x_ifpa_y<-replace_na(x_ifpa_y,0)
  
  a<-cbind(a,
           x_ifpa_y)
  
  a<-a %>%
    select(-model_fitted_lag12,
           -month_trend_lag12)
  
  # month by month xcgr calculation
  
  xcgr_m<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      xcgr_m[t]<-((a$model_fitted[t]/a$model_fitted[t-1])^
                  (1/(a$month_trend[t]-a$month_trend[t-1])))-1
    }
  }
  
  xcgr_m<-replace_na(xcgr_m,0)
  
  a<-cbind(a,
           xcgr_m)
  
  ryt_m<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>1){
      ryt_m[t]<-log((a$model_fitted[t]/a$model_fitted[t-1]),
                  base = exp(1))
    }
  }
  
  ryt_m<-replace_na(ryt_m,0)
  
  a<-cbind(a,
           ryt_m)
  
  sd_ryt_m<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    if(t>2){
      sd_ryt_m[t]<-sd(a$ryt_m[1:t])
    }
  }
  
  sd_ryt_m<-replace_na(sd_ryt_m,0)
  
  a<-cbind(a,
           sd_ryt_m)
  
  vxcgr_m<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    vxcgr_m[t]<-(1-a$sd_ryt_m[t])*a$xcgr_m[t]
  }
  
  vxcgr_m<-replace_na(vxcgr_m,0)
  
  a<-cbind(a,
           vxcgr_m)
  
  mean_vxcgr_m<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t==1){mean_vxcgr_m[t]<-a$vxcgr_m[t]}
    
    if(t>1){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      mean_vxcgr_m[t]<-sum(w*a$vxcgr_m[1:t])
    }
    
  }
  
  mean_vxcgr_m<-replace_na(mean_vxcgr_m,0)
  
  a<-cbind(a,
           mean_vxcgr_m)
  
  sd_vxcgr_m<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=2){sd_vxcgr_m[t]<-a$vxcgr_m[t]}
    
    if(t>2){
      w<-exp((1:t)/100)
      w<-w/sum(w)
      sd_vxcgr_m[t]<-(sum(w*((a$vxcgr_m[1:t]-a$mean_vxcgr_m[t])^2)))^0.5
    }
    
  }
  
  sd_vxcgr_m<-replace_na(sd_vxcgr_m,0)
  
  a<-cbind(a,
           sd_vxcgr_m)
  
  x_ifpa_m<-rep(0,nrow(a))
  
  for(t in 1:nrow(a)){
    
    if(t<=1){x_ifpa_m[t]<-a$vxcgr_m[t]}
    
    if(t>1){
      x_ifpa_m[t]<-(a$vxcgr_m[t]-a$mean_vxcgr_m[t])/a$sd_vxcgr_m[t]
    }
    
  }
  
  x_ifpa<-replace_na(x_ifpa_m,0)
  
  a<-cbind(a,
           x_ifpa_m)
  
  # # do pca to extract weights - Method 1 - NOT USING BUT EQUIVALENT
  # 
  # pca_xifpa<-a %>%
  #   select(x_ifpa_m,
  #          x_ifpa_y)
  # pca_xifpa<-eigen(cov(pca_xifpa))
  # pca_xifpa<-abs(pca_xifpa$vectors[,1])/sum(abs(pca_xifpa$vectors[,1]))
  # gamma<-pca_xifpa[1]
  
  # do pca to extract weights - Method 2 - CURRENTLY USING
  
  pca_xifpa<-a %>%
    select(x_ifpa_m,
           x_ifpa_y)
  pca_xifpa<-prcomp(pca_xifpa)
  gamma<-abs(pca_xifpa$rotation[,1])/sum(abs(pca_xifpa$rotation[,1]))
  gamma<-as.numeric(gamma[1])
  
  x_ifpa<-((gamma)*x_ifpa_m)+((1-gamma)*x_ifpa_y)
  
  gamma<-rep(gamma,nrow(a))
  
  a<-cbind(a,
           gamma,
           x_ifpa) %>%
    mutate(x_ifpa_m=abs(x_ifpa_m),
           x_ifpa_y=abs(x_ifpa_y),
           x_ifpa=abs(x_ifpa))
  
  rm(xcgr_y,
     ryt_y,
     sd_ryt_y,
     vxcgr_y,
     mean_vxcgr_y,
     sd_vxcgr_y,
     x_ifpa_y,
     xcgr_m,
     ryt_m,
     sd_ryt_m,
     vxcgr_m,
     mean_vxcgr_m,
     sd_vxcgr_m,
     x_ifpa_m,
     pca_xifpa,
     gamma,
     x_ifpa,
     t,
     w)
  
  if(p==1){
    x_index_table_stepwise<-a
  }
  
  if(p>1){
    x_index_table_stepwise<-x_index_table_stepwise %>%
      bind_rows(a)
  }
  
  rm(a,
     data_garch,
     prov_var)
  
}

plan(sequential)

rm(prov_vars,
   p)

print(paste0(round(((proc.time()[3]-ptm_loop)/60)/60,1),
             " Hours to Complete All Indices"))

####  Writeout of Results ####

write.csv(x_index_table_stepwise,
          file="C:/ag-vix/commodity_price/x_ifpa.csv",
          row.names = FALSE)

save(models_garch,
     file="C:/ag-vix/commodity_price/models_garch_ifpa.RData")
