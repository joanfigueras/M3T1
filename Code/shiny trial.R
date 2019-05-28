library("pacman")
p_load(dplyr, ggplot2,forecast,DescTools,corrplot,plotly,ggfortify, 
       GGally, readr,caret,readxl,RMySQL, scales,zoo, stringr,
       knitr,printr,party,polycor,padr,BBmisc,car,tidyr,
       rstudioapi,reshape,lubridate,raster,opera,prophet, shiny,shinydashboard,highcharter)
#Github environment####
current_path <- getwd()
setwd(dirname(current_path))
#Creating proper dataset####
energy <- read.csv("Datasets/energy.csv",as.is = TRUE)
energy$datetime <- as_datetime(energy$timedate,tz = "GMT")
energy$datetime <- as_datetime(energy$datetime,tz = "Europe/Paris")
energy$timedate <- NULL
energy <- energy[,c(6,1,2,3,4,5)]
energy <- energy[-which(year(energy$datetime) == 2010),]
energy <- pad(energy,break_above = 2)
for (i in 2:ncol(energy)) {
  energy[which(is.na(energy[,i])),i] <- Mode(energy[,i])
}
energy$year <- year(energy$datetime)
energy$quarter <- quarter(energy$datetime,fiscal_start = 3)
energy$month <- month(energy$datetime)
energy$day <- day(energy$datetime)
energy$minute <- minute(energy$datetime)
energy$hour <- hour(energy$datetime)
energy$week <- week(energy$datetime)
energy$yearmonth <- paste(month.abb[energy$month],energy$year,sep = "-")
energy$weekday <- wday(energy$datetime,abbr = FALSE,label = TRUE,locale = "English")
energy$active <- energy$active/60

vector <- c("kitchen","laundry","hvac")
for(i in 1:length(vector)){
  energy[vector[i]] <- round(energy[vector[i]]/1000,digits = 20)
} #TURNING KWH

##SETTING PRICES####  
price <- data.frame("peak" = c(0.158),"valley" = c(0.123))
energy$peak <- ifelse(between(energy$hour,0,7)|energy$hour == 23|between(energy$hour,14,17),yes = 0,no = 1)
vector <- c("kitchen","laundry","hvac","active")
for (i in 1:length(vector)) {
  
  energy[,ncol(energy)+1] <- ifelse(energy$peak == 1,
                                    yes = energy[vector[i]]*price$peak,
                                    no = energy[vector[i]]*price$valley)
  
  colnames(energy)[ncol(energy)] <- paste("price",vector[i],sep = "")
  
} 

energy$totalprice <- energy$priceactive
energy$priceactive <- NULL

energy_month <- read.csv("Datasets/energy_month.csv",row.names = 1,colClasses=c("Date"="Date"))
energy_year <- read.csv("Datasets/energy_year.csv",row.names = 1,colClasses=c("Date"="Date"))

DashboardPlot <- function(Year,Month = 0,Day = 0){
  require(ggplot2)
  require(ggfortify)
  require(dplyr)
  require(GGally)
  if(missing(Year)){
    p <- "Missing Year argument"
  } else {
    if(Month == 0){
      if(Year %in% unique(energy$year)){
        Day <- NULL
        df <- energy %>% group_by(year,month)  %>%
          dplyr::filter(year == Year)  %>%
          summarise(KitchenKw = sum(kitchen),
                    LaundryKw = sum(laundry),
                    HVACKw = sum(hvac),
                    TotalKw = sum(active),
                    KitchenPrice = sum(pricekitchen),
                    LaundryPrice = sum(pricelaundry),
                    HVACPrice = sum(pricehvac),
                    TotalPrice = sum(totalprice))
        
        p <- plot_ly(df, x = ~df$month, 
                     y = ~df$KitchenPrice, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
          add_trace(y = ~df$LaundryPrice, name = 'Laundry', mode = 'lines') %>%
          add_trace(y = ~df$HVACPrice, name = 'HVAC', mode = 'lines') %>%
          add_trace(y = ~df$TotalPrice, name = 'Total', mode = 'lines') %>%
          layout(title = paste(df$year[1]),
                 xaxis = list(title = "Month"),
                 yaxis = list (title = "Price in €"))
        
      }
      else {
        p <- "We don't have data from that year"
      }
    } else {
      if(Day == 0){ 
        if(Year %in% unique(energy$year) & Month %in% unique(energy$month)){
          df <- energy %>% group_by(year,month,day) %>% 
            dplyr::filter(year == Year & month == Month) %>% 
            summarise(KitchenKw = sum(kitchen),
                      LaundryKw = sum(laundry),
                      HVACKw = sum(hvac),
                      TotalKw = sum(active),
                      KitchenPrice = sum(pricekitchen),
                      LaundryPrice = sum(pricelaundry),
                      HVACPrice = sum(pricehvac),
                      TotalPrice = sum(totalprice))
          p <- plot_ly(df, x = ~df$day, 
                       y = ~df$KitchenPrice, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~df$LaundryPrice, name = 'Laundry', mode = 'lines') %>%
            add_trace(y = ~df$HVACPrice, name = 'HVAC', mode = 'lines') %>%
            add_trace(y = ~df$TotalPrice, name = 'Total', mode = 'lines') %>%
            layout(title = paste(df$year[1],df$month[1],sep = "-"),
                   xaxis = list(title = "Day"),
                   yaxis = list (title = "Price in €"))
        }
        else {
          p <- "We don't have data for that month or year"
        }
      }
      else 
      {
        if(Year %in% unique(energy$year) & Month %in% unique(energy$month) & Day %in% unique(energy$day)){
          df <- energy %>% group_by(year,month,day,hour) %>%
            dplyr::filter(year == Year & month == Month & day == Day) %>%
            summarise(KitchenKw = sum(kitchen),
                      LaundryKw = sum(laundry),
                      HVACKw = sum(hvac),
                      TotalKw = sum(active),
                      KitchenPrice = sum(pricekitchen),
                      LaundryPrice = sum(pricelaundry),
                      HVACPrice = sum(pricehvac),
                      TotalPrice = sum(totalprice))
          p <- plot_ly(df, x = ~df$hour, 
                       y = ~df$KitchenPrice, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~df$LaundryPrice, name = 'Laundry', mode = 'lines') %>%
            add_trace(y = ~df$HVACPrice, name = 'HVAC', mode = 'lines') %>%
            add_trace(y = ~df$TotalPrice, name = 'Total', mode = 'lines') %>%
            layout(title = paste(df$year[1],df$month[1],df$day[1],sep = "-"),
                   xaxis = list(title = "Hour"),
                   yaxis = list (title = "Price in €"))
          
        } else {
          p <- "We don't have data from that date"
        }
      }
    }
  } 
  return(p)
}

#SHINY####
# USER INTERFACE
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Energy Consumption"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "graphs", icon =icon("bar-chart-o")),       
      menuItem("Datasets", tabName = "datasets", icon = icon("database")),
      menuItem("Forecast",tabName = "forecast", icon = icon("bar-chart")),
      selectInput(inputId = "Granularity", label = "Select a granularity",
                  choices=c("Day","Month"),selected = "Month"),
      
      selectInput(inputId = "Variable", label = "Select a variable", selected = "TotalPrice",
                  choices=c("KitchenKw","LaundryKw","HVACKw","TotalKw","KitchenPrice","LaundryPrice","HVACPrice","TotalPrice")),
      
      dateRangeInput(inputId = "Dates", label = "Select date ranges",
                     start = min(energy_month$Date),end = max(energy_month$Date),
                     min = min(energy_month$Date))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets", box(shiny::dataTableOutput("datasetTable"),width=12)),
      
      tabItem(tabName = "graphs",
              tabsetPanel(
                tabPanel(title = "Exploration",
                  box(plotlyOutput("plot"),width = 12),
                  valueBoxOutput(outputId = "Sum",width = 6),
                  valueBoxOutput(outputId = "Average",width = 6)),
                tabPanel(title = "Plot",
                     inputPanel(selectInput("Year", "Choose a Year", 
                                            choices = unique(energy$year),width = "60%"),
                                selectInput("Month", "Month (Optional)", 
                                            choices = 0:12,selected = 0,width = "60%"),
                                selectInput("Day", "Day (Optional)", 
                                            choices = 0:31,selected = 0,width = "60%"),width = 8),
                     box(plotlyOutput("plotfun"),width = 12)))),
      
      tabItem(tabName = "forecast",
              box(plotlyOutput("predictions"),width = 12),
              box(shiny::numericInput(inputId = "datepredict",
                                   label = "Number of days to forecast",
                                   min = 1,
                                   value = 365)),
                  box(selectInput(inputId = "forecast",
                              label = "Variable to forecast",
                              choices = c("kitchen","active",
                                         "laundry","hvac","pricekitchen",
                                         "pricelaundry","pricehvac","totalprice"))))
    )
  )
)

# SERVER
server <- function(input, output) {
  
  get.granularity <- reactive({
    switch(input$Granularity,
           "Day" = energy_month,
           "Month" = energy_year)
  })
  
  filteredData <- reactive({
    get.granularity() %>% 
      dplyr::select(Variable=input$Variable, Date) %>% 
      dplyr::filter(Date <= input$Dates[2] & Date >= input$Dates[1])
  })
  
  output$datasetTable <- shiny::renderDataTable ({
    final_data <- filteredData()
    colnames(final_data) <- c(input$Variable,"Date")
    final_data
  })
  
  output$Dates <- renderPrint ({ 
    input$Dates 
  })
  
  output$plot <- renderPlotly({
    data_plot<-filteredData()
    plot_ly(data_plot, x = ~data_plot$Date, 
            y = ~data_plot$Variable, name = input$Variable, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = input$Granularity),
             yaxis = list (title = input$Variable))
  })
  output$plotfun <-  renderPlotly({
    p <- DashboardPlot(input$Year,input$Month,input$Day)
  })
  output$Sum <- renderValueBox({
    sum_data <- filteredData()
    valueBox(subtitle = paste("Sum of ",input$Variable," Between ",input$Dates[1]," and ",input$Dates[2]),color = "navy",
             value = ifelse(input$Variable %in% c("KitchenKw","LaundryKw","HVACKw","TotalKw"),
                            yes = paste(round(sum(sum_data$Variable),digits = 2),"Kw/h"),
                            no = paste(round(sum(sum_data$Variable),digits = 2),"€")),width = 6
    )
  })
  output$Average <- renderValueBox({
    average_data <- filteredData()
    valueBox(subtitle = paste("Average of ",input$Variable," Between ",input$Dates[1]," and ",input$Dates[2]),color = "navy",
             value = ifelse(input$Variable %in% c("KitchenKw","LaundryKw","HVACKw","TotalKw"),
                            yes = paste(round(mean(average_data$Variable),digits = 2),"Kw/h"),
                            no = paste(round(mean(average_data$Variable),digits = 2),"€")),width = 6
    )
  })
  output$predictions <-  renderPlotly({
    prophetdf <- energy %>% gather(key = "key", value = "value", kitchen,active,
                                   laundry,hvac,pricekitchen,
                                   pricelaundry,pricehvac,totalprice) %>% 
      filter(key %in% input$forecast) %>% 
      group_by(date(datetime)) %>% 
      summarise(y = sum(value)) 
    names(prophetdf) <- c("ds","y")
    prophetresult <- prophet(prophetdf,daily.seasonality = TRUE)
    futuredf <- make_future_dataframe(prophetresult,periods = input$datepredict)
    prophetforecast2010 <- predict(prophetresult,futuredf)
    prophetdf[(nrow(prophetdf)+1):length(prophetforecast2010$yhat),2] <- NA
    prophetdf$ds <- seq.Date(from = prophetdf$ds[1],by = "day",along.with = prophetdf$ds)
    prophetdf$predictions <- prophetforecast2010$yhat
    prophetdf <- prophetdf %>% dplyr::filter(ds >= input$Dates[1] & ds <= (max(date(energy$datetime))+input$datepredict)) %>% group_by(year(ds),month(ds)) %>% summarise(y = sum(y),
                                                                                                                                     predictions = sum(predictions))
    prophetdf$ds <- as.Date(paste(prophetdf$`year(ds)`,prophetdf$`month(ds)`,"01",sep = "-"),format = "%Y-%m-%d")
    colnames(prophetdf) <- c("years","month",input$forecast,"predictions","date")
    p1 <- ggplot(prophetdf,aes_string(x = "date")) +  
      geom_line(aes_string(y = input$forecast), col = "blue") + 
      geom_line(aes_string(y = "predictions"), col = "orange") +
      xlab("Monthly Predictions") + ylab("Price in €")
    plotly::ggplotly(p1)  })
}

# RUNNING APP
shinyApp(ui, server)

