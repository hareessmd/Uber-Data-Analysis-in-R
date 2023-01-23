install.packages("ggthemes")
library(ggthemes)
install.packages("dplyr")
install.packages("DT")


getwd()
apr_data <- read.csv(file.choose())
head(apr_data)
may_data <- read.csv(file.choose())
jun_data <- read.csv(file.choose())
jul_data <- read.csv(file.choose())
aug_data <- read.csv(file.choose())
sep_data <- read.csv(file.choose())

data_2014 <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

head(data_2014)
str(data_2014)

data_2014$Date.Time <- as.factor(data_2014$Date.Time)
data_2014$Base <- as.factor(data_2014$Base)

str(data_2014)

summary(data_2014)


?as.POSIXct

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format= "%m/%d/%Y %H:%M:%S")

head(data_2014)

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format= "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

?ymd_hms

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- format(day(data_2014$Date.Time))

data_2014$month <- format(month(data_2014$Date.Time, label=TRUE))

data_2014$dayofweek <- format(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))


#viz - we are grouping by hour 
hour_data <- data_2014 %>%
  group_by(hour)%>%
  summarise(Total = n())
  
head(hour_data)


ggplot(data = hour_data, aes(x=hour, y=Total)) +
  geom_bar(stat = "identity", fill="Blue", colour="Black") +
  ggtitle("Trips by Hour") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

- most rides happend bw 15 and 21 from the above plot - mean 3pm to 9pm


#here we are grouping hour and month
month_hour_data <- data_2014 %>%
  group_by(month,hour)%>%
  summarise(Total = n())

#viz 1 - sep has more rides
ggplot(data = month_hour_data, aes(x=hour, y=Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour in different months") +
  facet_grid(month~.)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma) # this shows all the months hourly total

#alternate of viz 1
ggplot(data = month_hour_data, aes(x=hour, y=Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour in different months") +
  #facet_grid(month~.)+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

#sept data in hours
sep_hour_data <- data_2014 %>%
  group_by(hour,month)%>%
  filter(month == "Sep")%>%
  summarise(Total = n())

ggplot(data = sep_hour_data, aes(x=hour, y=Total)) +
  geom_bar(stat = "identity", fill="Black", colour="Blue") +
  ggtitle("Trips by Hour in September month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)
# 6pm has highest ride (approx. 70000 rides)

#april data in hours
apr_hour_data <- data_2014 %>%
  group_by(hour,month)%>%
  filter(month == "Apr")%>%
  summarise(Total = n())

ggplot(data = apr_hour_data, aes(x=hour, y=Total)) +
  geom_bar(stat = "identity", fill="Black", colour="Blue") +
  ggtitle("Trips by Hour in April month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=comma)

#5pm has highest rides (approx. 50000 rides)

#now we are grouping by day

day_data <- data_2014 %>%
  group_by(day)%>%
  summarise(Total = n())

ggplot(data = day_data, aes(x=day, y=Total, fill=day)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Day")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(labels = comma)

#grouping month and day

month_day_data <- data_2014 %>%
  group_by(month,day)%>%
  summarise(Total = n())

ggplot(data = month_day_data, aes(x=day, y=Total, fill=month))+
  geom_bar(stat = "identity") +
  ggtitle("Trips per day in different months") +
  facet_grid(month~.) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=comma)
         

#sept data in days

sep_day_data <- data_2014 %>%
  group_by(day,month)%>%
  filter(month == "Sep")%>%
  summarise(Total = n())

ggplot(data = sep_day_data, aes(x=day, y=Total)) +
  geom_bar(stat = "identity", fill="Black", colour="Blue") +
  ggtitle("Trips by Day in September month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)
#13th day highest rides

#monthly trend
month_data <- data_2014%>%
  group_by(month) %>%
  summarise(Total=n())

ggplot(data = month_data, aes(x=month, y=Total)) +
  geom_bar(stat = "identity", aes(fill=month)) +
  ggtitle("Trips by month")+
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5))+
  scale_y_continuous(labels=comma)
#sept has most rides


#weekday of month grouped
month_weekday_data <- data_2014 %>%
  group_by(month,dayofweek) %>%
  summarise(Total=n())

ggplot(data = month_weekday_data, aes(x=dayofweek, y=Total)) +
  geom_bar(stat = "identity", aes(fill=month)) +
  ggtitle("Trips by weekday of month")+
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5))+
  scale_y_continuous(labels=comma)

#only weekday data

weekday_data <- data_2014 %>%
  group_by(dayofweek) %>%
  summarise(Total=n())

ggplot(data = weekday_data, aes(x=dayofweek, y=Total)) +
  geom_bar(stat = "identity", aes(fill=dayofweek)) +
  ggtitle("Trips by weekday of month")+
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5))+
  scale_y_continuous(labels=comma)
#thursday and friday has more rides, find out reasons for that

#sep data of weekdays

sep_weekday_data <- data_2014 %>%
  group_by(dayofweek,month) %>%
  filter(month=="Sep")%>%
  summarise(Total=n())

ggplot(data = sep_weekday_data, aes(x=dayofweek, y=Total)) +
  geom_bar(stat = "identity", fill="Blue", colour="Black") +
  ggtitle("Trips by weekday of september")+
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5))+
  scale_y_continuous(labels=comma)

#data about Bases

data_2014$Base
str(data_2014)

ggplot(data = data_2014, aes(Base))+
  geom_bar(fill="yellow") +
  ggtitle("Trips by Base")+
  scale_y_continuous(labels=comma)
#Base - B02617 has highest rides find out factors why B02512 has lowest rides

#base and months grouped
ggplot(data = data_2014, aes(Base, fill=month))+
  geom_bar(position = "dodge") +
  ggtitle("Trips by Base and month")+
  scale_y_continuous(labels=comma)









