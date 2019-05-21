library("pacman")
p_load(dplyr, ggplot2,forecast,DescTools,corrplot,plotly,ggfortify, GGally, readr,caret,readxl,RMySQL, scales,zoo, stringr,
       knitr,printr,party,polycor,padr,BBmisc,car,rstudioapi,reshape,lubridate,raster)
# con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!',
#                 dbname='dataanalytics2018',
#                 host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
# dbListTables(con)
# dbListFields(con,'yr_2006')
# Reading df #####
# df2 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#                   Global_reactive_power,Global_active_power,
#                   Sub_metering_2,Sub_metering_3 FROM yr_2007")
# df3 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#                   Global_reactive_power,Global_active_power,
#                   Sub_metering_2,Sub_metering_3 FROM yr_2008")
# df4 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#                   Global_reactive_power,Global_active_power,
#                   Sub_metering_2,Sub_metering_3 FROM yr_2009")
# energy <- bind_rows(df2,df3,df4)
# energy$timedate <- paste(energy$Date,energy$Time)
# colnames(energy) <- c("date","time","kitchen","reactive","active","laundry","hvac","timedate")
# energy <- energy[,c(ncol(energy), 1:(ncol(energy)-1))]
# write.csv(energy,file = "energy.csv",row.names = FALSE)
# energy$datetime <- lubridate::as_datetime(energy$timedate,tz = "Europe/Paris")

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
cat(round(length(which(energy$kitchen > 0))/length(energy$kitchen)*100,digits = 2),
    "% of the time the kitchen is consuming energy\n",sep = "")
cat(round(length(which(energy$laundry > 0))/length(energy$laundry)*100,digits = 2),
    "% of the time the laundry is consuming energy\n",sep = "")
cat(round(length(which(energy$hvac > 0))/length(energy$hvac)*100,digits = 2),
    "% of the time the hvac is consuming energy\n",sep = "")
cat(round(length(which(energy$kitchen + energy$laundry + energy$hvac > 0))/length(energy$kitchen)*100,digits = 2),
    "% of the time the submitters are consuming energy somewhere in the house\n",sep = "")
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
} #DAY PLOT

#using peak and valley price now 
price <- data.frame("peak" = c(0.158),"valley" = c(0.123))
energy$peak <- ifelse(between(energy$hour,0,7)|energy$hour == 23|between(energy$hour,14,17),yes = 0,no = 1)
vector <- c("kitchen","laundry","hvac","active")
for (i in 1:length(vector)) {
  
  energy[,ncol(energy)+1] <- ifelse(energy$peak == 1,
                                    yes = energy[vector[i]]*price$peak,
                                    no = energy[vector[i]]*price$valley)
  
  colnames(energy)[ncol(energy)] <- paste("price",vector[i],sep = "")
  
} #SETTING PRICES

energy$totalprice <- energy$priceactive
energy$priceactive <- NULL
cat("Average Prices per Hour")
cat("Kitchen ",(sum(energy$pricekitchen)/(length(energy$pricekitchen))/(1/60)),"€\n",sep = "")
cat("Laundry ",(sum(energy$pricelaundry)/(length(energy$pricelaundry))/(1/60)),"€\n",sep = "")
cat("Hvac ",(sum(energy$pricehvac)/(length(energy$pricehvac))/(1/60)),"€\n",sep = "")
cat("TOTAL ",(sum(energy$totalprice)/(length(energy$totalprice))/(1/60)),"€\n",sep = "")

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
energyweekly <- energy %>% group_by(year,week) %>% 
                summarise(kitchen = sum(kitchen),
                          laundry = sum(laundry),
                          hvac = sum(hvac),
                          active = sum(active),
                          pricekitchen = sum(pricekitchen),
                          pricelaundry = sum(pricelaundry),
                          pricehvac = sum(pricehvac),
                          totalprice = sum(totalprice))

energyts <- msts(energyweekly$active,seasonal.periods = 53,ts.frequency = 53,
                 start = c(2007,1),end = c(2009,53))
decomposedts <- stl(energyts,s.window = "period")
plot(energyts, main="Active consumption", xlab="Year", ylab="KW/h")
autoplot(decomposedts)

trainset <- window(energyts, start=2007,end = c(2008,53))
testset <- window(energyts, start=2009)


