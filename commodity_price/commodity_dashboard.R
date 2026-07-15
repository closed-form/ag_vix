library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(stringr)
library(tidyverse)

options(scipen = 999)

dash_data<-as.data.frame(
  read.csv(file = paste0("C:/ag-vix/commodity_price/",
                         "x_ifpa",
                         ".csv"),
           header = TRUE)) %>%
  mutate(Commodity.groups=str_replace(Commodity.groups,
                                      " \\[.*",
                                      ""),
         REF_DATE=as.Date(REF_DATE,format="%Y-%m-%d"),
         fc_start=as.Date(fc_start,format="%Y-%m-%d")) %>%
  select(REF_DATE,
         GEO,
         Commodity.groups,
         source,
         year,
         month,
         day,
         year_trend,
         month_trend,
         VALUE,
         model_fitted,
         x_ifpa,
         xcgr_m,
         ryt_m,
         sd_ryt_m,
         vxcgr_m,
         mean_vxcgr_m,
         sd_vxcgr_m,
         x_ifpa_m,
         xcgr_y,
         ryt_y,
         sd_ryt_y,
         vxcgr_y,
         mean_vxcgr_y,
         sd_vxcgr_y,
         x_ifpa_y,
         fc_start) %>%
  filter(!is.na(REF_DATE)) %>%
  mutate(Commodity.groups=paste0(source,": ",Commodity.groups)) %>%
  select(-source)

names(dash_data)<-c(
  "Date",
  "Geography",
  "Index",
  "Year",
  "Month",
  "Day",
  "Year Trend",
  "Month Trend",
  "Index Value",
  "Fitted Value",
  "IFPA [Composite]",
  "XCGR [MoM]",
  "r [MoM]",
  "SD(r) [MoM]",
  "vXCGR [MoM]",
  "mean(vXCGR) [MoM]",
  "sd(vXCGR) [MoM]",
  "IFPA [MoM]",
  "XCGR [YoY]",
  "r [YoY]",
  "SD(r) [YoY]",
  "vXCGR [YoY]",
  "mean(vXCGR) [YoY]",
  "sd(vXCGR) [YoY]",
  "IFPA [YoY]",
  "Forecasting Start")

dash_data<-dash_data %>%
  pivot_longer(cols = `Index Value`:`IFPA [YoY]`,
               names_to = "Component",
               values_to = "Value")

# Turn into a table

dash_data_table<-dash_data %>%
  filter(Component=="IFPA [Composite]",
         Geography=="Canada") %>%
  mutate(
    moderate=case_when(
      abs(Value)>=0.5&abs(Value)<1~1,
      TRUE~0),
    abnormal=case_when(
      abs(Value)>=1~1,
      TRUE~0),
    moderate_fix=case_when(
      abs(Value)>=1&abs(Value)<2~1,
      TRUE~0),
    abnormal_fix=case_when(
      abs(Value)>=2~1,
      TRUE~0)) %>%
  group_by(Component,
           Geography,
           Index) %>%
  summarise(
    moderate=sum(moderate),
    abnormal=sum(abnormal),
    moderate_fix=sum(moderate_fix),
    abnormal_fix=sum(abnormal_fix)) %>%
  ungroup()

# Define UI

ui <- fluidPage(
  
  # titlePanel("FAO Volatility-Statistics Canada Farm Product Price Indices"),
  
  titlePanel("FAO Volatility-Statistics Canada Farm to Food Price Indices"),
  
  # Sidebar with select inputs for filtering and choosing the plot type
  sidebarLayout(
    sidebarPanel(
      selectInput("province",
                  "Select Geography",
                  choices = unique(dash_data$Geography),
                  selected = "Canada"),
      checkboxGroupInput("index",
                         "Select Index/Indices",
                         choices = unique(dash_data$Index),
                         selected = "FPPI: Total index"),
      selectInput("component",
                  "Select Component",
                  choices = unique(dash_data$Component),
                  selected = "Index Value"),
      sliderInput("range",
                  "Select a Time Range",
                  min = min(dash_data$Date),
                  max = max(dash_data$Date),
                  value = c(max(dash_data$Date)-(365*2),
                            max(dash_data$Date)),
                  timeFormat="%Y-%m-%d")
    ),
    
    # Main panel for displaying plots
    mainPanel(
      plotlyOutput("plot")  ## added this plot feature
    )
  )
)

# Define server logic

server <- function(input, output){
  
  date_range<-reactive({
    req(input$range)
    seq(as.Date(input$range[1],format="%Y-%m-%d"),
        as.Date(input$range[2],format="%Y-%m-%d"),
        1)
  })
  
  filtered_data <- reactive({
    validate(
      need(input$index != "",
           "Please select an index."))
    
    dash_data %>%
      filter(Geography == input$province &
               Index %in% input$index &
               Component == input$component &
               Date %in% date_range())
  })
  
  output$plot <- renderPlotly({
    
    `Forecast Start`<-as.Date(
      unique(
        filtered_data()$`Forecasting Start`),
      format="%Y-%m-%d")
    
    print(`Forecast Start`)
    
    p<-NULL
    
    if(input$component %in% c("IFPA [Composite]",
                              "IFPA [MoM]",
                              "IFPA [YoY]")){
      
      p<-ggplot(filtered_data(),
                aes(x = Date,
                    y = Value,
                    group = Index,
                    colour = Index,
                    shape = Index)) +
        geom_line() +
        geom_point(size=1) +
        geom_hline(
          yintercept = 2,
          colour = "red3",
          linetype="dashed",
          show.legend = FALSE) +
        geom_hline(
          yintercept = 1,
          colour = "gold3",
          linetype="dashed",
          show.legend = FALSE) +
        scale_x_date(
          limits = c(as.Date(input$range[1],format="%Y-%m-%d"),
                     as.Date(input$range[2],format="%Y-%m-%d"))) +
        labs(title = paste("Selected Indices'",
                           input$component,
                           "for",
                           input$province),
             x = "Date",
             y = "Value") +
        theme_minimal()
      
    }
    
    if(!input$component %in% c("IFPA [Composite]",
                               "IFPA [MoM]",
                               "IFPA [YoY]")){
      
      p<-ggplot(filtered_data(),
                aes(x = Date,
                    y = Value,
                    group = Index,
                    colour = Index,
                    shape = Index)) +
        geom_line() +
        geom_point(size=1) +
        scale_x_date(
          limits = c(as.Date(input$range[1],format="%Y-%m-%d"),
                     as.Date(input$range[2],format="%Y-%m-%d"))) +
        labs(title = paste("Selected Indices'",
                           input$component,
                           "for",
                           input$province),
             x = "Date",
             y = "Value") +
        theme_minimal()
      
    }
    
    p<-p +
      geom_vline(xintercept = `Forecast Start`,
                 linetype = "solid",
                 colour = "black",
                 linewidth = 2,
                 show.legend = TRUE)
    
    ggplotly(p) %>% 
      layout(hovermode = "closest")  # Enable hover mode to display values
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
