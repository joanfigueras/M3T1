library("pacman")
p_load(dplyr, ggplot2,forecast,DescTools,corrplot,plotly,ggfortify, 
       GGally, readr,caret,readxl,RMySQL, scales,zoo, stringr,
       knitr,printr,party,polycor,padr,BBmisc,car,
       rstudioapi,reshape,lubridate,raster,opera,prophet)
#Creating proper dataset####
energy <- read.csv("energy.csv",as.is = TRUE)
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
energy$weekday <- wday(energy$datetime,abbr = FALSE,label = TRUE)
energy$active <- energy$active/60

vector <- c("kitchen","laundry","hvac")
for(i in 1:length(vector)){
  energy[vector[i]] <- round(energy[vector[i]]/1000,digits = 20)
} #TURNING KWH

#Time every room consumes energy####
cat(round(length(which(energy$kitchen > 0))/length(energy$kitchen)*100,digits = 2),
    "% of the time the kitchen is consuming energy\n",sep = "")
cat(round(length(which(energy$laundry > 0))/length(energy$laundry)*100,digits = 2),
    "% of the time the laundry is consuming energy\n",sep = "")
cat(round(length(which(energy$hvac > 0))/length(energy$hvac)*100,digits = 2),
    "% of the time the hvac is consuming energy\n",sep = "")
cat(round(length(which(energy$kitchen + energy$laundry + energy$hvac > 0))/length(energy$kitchen)*100,digits = 2),
    "% of the time the submitters are consuming energy somewhere in the house\n",sep = "")
#CREATING DAY PLOTS####
vector <- levels(energy$weekday)
dayplots <- list()
for (i in 1:length(levels(energy$weekday))){
  
  dayplots[[i]] <- energy %>% group_by(hour) %>% filter(weekday == vector[i]) %>% 
    summarise(kitchen = mean(kitchen),
              hvac = mean(hvac),
              laundry = mean(laundry),
              active = mean(active))
  
  dayplots[[i]] <- ggplot(dayplots[[i]],aes(x=hour)) +
    geom_line(aes(y = kitchen,col = "kitchen")) +
    geom_line(aes(y = laundry,col = "laundry")) +
    geom_line(aes(y = active,col = "active")) +
    geom_line(aes(y = hvac,col = "hvac")) +
    geom_point(aes(y = active,col = "active")) +    
    geom_point(aes(y = kitchen,col = "kitchen")) +
    geom_point(aes(y = laundry,col = "laundry")) +
    geom_point(aes(y = hvac,col = "hvac")) +
    ylab("Kw/h") + xlab("Hour") + ggtitle(vector[i])
} 

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
#AVERAGE PRICES PER HOUR
cat("Average Prices per Hour")
cat("Kitchen ",(sum(energy$pricekitchen)/(length(energy$pricekitchen))/(1/60)),"€\n",sep = "")
cat("Laundry ",(sum(energy$pricelaundry)/(length(energy$pricelaundry))/(1/60)),"€\n",sep = "")
cat("Hvac ",(sum(energy$pricehvac)/(length(energy$pricehvac))/(1/60)),"€\n",sep = "")
cat("TOTAL ",(sum(energy$totalprice)/(length(energy$totalprice))/(1/60)),"€\n",sep = "")
#PLOT BY MONTH PRICED####
monthlyplots <- list()
vector <- unique(energy$yearmonth)
for (i in 1:length(unique(energy$yearmonth))) {
  energyts <- energy %>% filter(yearmonth == vector[i])  %>% group_by(day) %>%
    summarise(pricekitchen = sum(pricekitchen),
              pricelaundry = sum(pricelaundry),
              pricehvac = sum(pricehvac),
              totalprice = sum(totalprice))
  
  monthlyplots[[i]] <- plot_ly(energyts, x = ~energyts$day, 
                               y = ~energyts$pricekitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~energyts$pricelaundry, name = 'Laundry', mode = 'lines') %>%
    add_trace(y = ~energyts$pricehvac, name = 'HVAC', mode = 'lines') %>%
    add_trace(y = ~energyts$totalprice, name = 'Total', mode = 'lines') %>%
    layout(title = "Price on average per day",
           xaxis = list(title = vector[i]),
           yaxis = list (title = "€"))
} 
names(monthlyplots) <- unique(energy$yearmonth)
#TIME SERIES####
#Grouping by year and week
energytimed <- energy %>% group_by(year,month,day) %>% 
                summarise(kitchen = sum(kitchen),
                          laundry = sum(laundry),
                          hvac = sum(hvac),
                          active = sum(active),
                          pricekitchen = sum(pricekitchen),
                          pricelaundry = sum(pricelaundry),
                          pricehvac = sum(pricehvac),
                          totalprice = sum(totalprice))
#Creating Time Series####
energyts <- msts(energytimed$active,seasonal.periods = 365,ts.frequency = 365,
                 start = c(2007,1),end = c(2009,365))
decomposedts <- stl(energyts,s.window = "period")
plot(energyts, main="Active consumption", xlab="Year", ylab="KW/h")
autoplot(decomposedts)

energytsholt <- energyts - decomposedts$time.series[,1]

#Creating train and test####  
trainset <- window(energyts, start=2007,end = c(2008,365))
testset <- window(energyts, start=2009)

trainsetholt <- window(energytsholt, start=2007,end = c(2008,365))
testsetholt <- window(energytsholt, start=2009)
#Holtwinters####
holtresult <- HoltWinters(x = trainsetholt,beta = FALSE,gamma = FALSE)
holtforecast2009 <- forecast(holtresult,h = 365)
holtacc <- accuracy(f = holtforecast2009,testsetholt)
plot(holtresult)
#Autoarima####
arimaresult <- auto.arima(trainset)
arimaforecast2009 <- forecast(arimaresult,h = 365)
arimaacc <- accuracy(f = arimaforecast2009,testset)
plot(arimaforecast2009)
#CrossValidation other models####
crossvalidation <- c()
vector <- c(meanf,rwf,naive)
accuracycv <- NULL
for (i in vector) {
  crossvalidation <- tsCV(y = energyts,h = 53,forecastfunction = i)
  accuracycv <- rbind(accuracycv,accuracy(crossvalidation,energyts))
}
rownames(accuracycv) <- c("meanf","rwf","naive")
#Prophet####
prophetdf <- energy %>% group_by(date(datetime)) %>% 
  summarise(y = sum(active))
names(prophetdf) <- c("ds","y")
partytime <- data.frame(holiday = 'summer',
                        ds = c(seq(as.Date('2007-08-01'),as.Date('2007-08-31'),by = 'd'),
                               seq(as.Date('2008-08-01'),as.Date('2008-08-31'),by = 'd'),
                               seq(as.Date('2009-08-01'),as.Date('2009-08-31'),by = 'd')))
prophetresult <- prophet(prophetdf,daily.seasonality = TRUE,
                         holidays = partytime)
futuredf <- make_future_dataframe(prophetresult,periods = 365)
prophetforecast2010 <- predict(prophetresult,futuredf)
prophet_plot_components(prophetresult,fcst = prophetforecast2010)
plot(prophetresult,prophetforecast2010,pch = 24,cex = 3)
#CV PROPHET
cvdf <- cross_validation(prophetresult,initial = 2*365,units = 'days',horizon = 365)
cvdfper <- performance_metrics(cvdf,rolling_window = 0.2)

accuracycv
arimaacc
holtacc
prophetaacc
