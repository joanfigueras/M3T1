library("pacman")
p_load(dplyr, ggplot2,corrplot, GGally, readr,caret,readxl,RMySQL, scales,zoo,
       knitr,printr,party,polycor,BBmisc,car,rstudioapi,reshape,lubridate)
# con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
#                 dbname='dataanalytics2018', 
#                 host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
# dbListTables(con)
# dbListFields(con,'yr_2006')
#Reading df #####
# df1 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#           Global_reactive_power,Global_active_power,
#           Sub_metering_2,Sub_metering_3 FROM yr_2006")
# df2 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#           Global_reactive_power,Global_active_power,
#           Sub_metering_2,Sub_metering_3 FROM yr_2007")
# df3 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#           Global_reactive_power,Global_active_power,
#           Sub_metering_2,Sub_metering_3 FROM yr_2008")
# df4 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#           Global_reactive_power,Global_active_power,
#           Sub_metering_2,Sub_metering_3 FROM yr_2009")
# df5 <- dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,
#           Global_reactive_power,Global_active_power,
#           Sub_metering_2,Sub_metering_3 FROM yr_2010")
# energy <- bind_rows(df1,df2,df3,df4,df5)
# summary(energy)
# energy$timedate <- paste(energy$Date,energy$Time)
# colnames(energy) <- c("date","time","kitchen","reactive","active","laundry","hvac","datetime")
# energy <- energy[,c(ncol(energy), 1:(ncol(energy)-1))]
# energy$date <- NULL
# energy$time <- NULL
#write.csv(energy,file = "energy.csv",row.names = FALSE)
energy <- read.csv("energy.csv")
energy$datetime <- as.POSIXct(energy$datetime, "%Y-%m-%d %H:%M:%S",tz = "Europe/Paris")
#% Of use ####
cat(round(length(which(energy$kitchen > 0))/length(energy$kitchen)*100,digits = 2),
    "% of the time the kitchen is consuming energy\n",sep = "")
cat(round(length(which(energy$laundry > 0))/length(energy$laundry)*100,digits = 2),
    "% of the time the laundry is consuming energy\n",sep = "")
cat(round(length(which(energy$hvac > 0))/length(energy$hvac)*100,digits = 2),
    "% of the time the hvac is consuming energy\n",sep = "")
cat(round(length(which(energy$kitchen + energy$laundry + energy$hvac > 0))/length(energy$kitchen)*100,digits = 2),
    "% of the time the submitters are consuming energy somewhere in the house\n",sep = "")
##### 
energy$year <- year(energy$datetime)
energy$quarter <- quarter(energy$datetime,fiscal_start = 3)
energy$month <- month(energy$datetime)
energy$day <- day(energy$datetime)
seasonal <- summarize(group_by(energy,quarter),laundry = sum(laundry),
            kitchen = sum(kitchen),hvac = sum(hvac))
seasonal <- seasonal [1:4,]
seasonal$quarter <- NULL
seasonal <- as.matrix(seasonal)
rownames(seasonal) <- c("spring","summer","autumn","winter")
seasonal <- melt(seasonal,varnames = c("Season","Room"))
ggplot(data = seasonal,aes(x = Season,y = value,fill = Room)) + geom_col(position = "stack") +
                   ylab("") + xlab("Season")
timedf <- as.data.frame(energy[,c(2,5:10)])
timedf$consumption <- timedf$kitchen + timedf$laundry + timedf$hvac
plotting <- aggregate(list(timedf$kitchen,timedf$laundry,timedf$hvac), by=list(timedf$month,timedf$year), sum)
colnames(plotting) <- c("month","year","kitchen","laundry","hvac")
plotting$month <- month.abb[plotting$month]
plotting$date <- paste(plotting$year,plotting$month,sep = "-")
plotting$month <- NULL
plotting$year <- NULL
ggplot(data = melt(plotting),aes(x=date,y=value,fill = variable))+ 
  geom_col() + theme(axis.text.x = element_text(angle = 70))
