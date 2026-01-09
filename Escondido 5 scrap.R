
library(plotly)
library (dplyr)
hansonrain<-read.csv("data/escondido 5 1.7 data/JLDP Escondido 5 new Rain Gauge Rain Gauge 20231001-20260107.csv")
hansonwell<-read.csv("data/escondido 5 1.7 data/JLDP Escondido 5 Well Well 20231001-20260107.csv")
hansonwell$Date.and.Time<-as.POSIXct(hansonwell$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
hansonwell$ft..below.ground.<-hansonwell$ft..below.ground.-12.5

hourly_data<-hansonwell
daily_data<-hansonrain

daily_means <- hourly_data %>%
  mutate(Date = as.Date(Date.and.Time)) %>% # Extract the date part
  group_by(Date) %>%
  summarize(mean_value = mean(ft..below.ground., na.rm = TRUE)) %>%
  ungroup()

daily_data$Date<-as.Date(daily_data$Date,format="%Y-%m-%d (%A)")
daily_data<-daily_data[daily_data$Date>="2025-10-01",]
hourly_data<-hourly_data[hourly_data$Date.and.Time>="2025-10-01 00:00:00",]
daily_means<-daily_means[daily_means$Date>="2025-10-01",]
str(hourly_data)
tail(hourly_data)
### Make 2 seperate interactive plots with the same scale

a<-plot_ly(data= daily_data, x = daily_data$Date, 
           y = daily_data$Rain..in., type = 'bar',marker =list(color="99CCff")) %>%
  layout(yaxis = list(autorange = "reversed"))

b<-plot_ly(data = hourly_data, x = hourly_data$Date.and.Time, y = hourly_data$ft..below.ground., name = 'Hourly', type = 'scatter', mode = 'lines',line = list(color = "grey"),opacity = 0.3) %>% 
  add_trace(data = daily_means, x = daily_means$Date, y = daily_means$mean_value, mode = 'lines',line = list(color = "black"),opacity = 0.9)  %>% 
  layout(yaxis = list(autorange = "reversed"))

fig <- subplot(a, b, nrows =2 ) %>% 
  layout(title = list(text = "Escondido 5 2025"), 
         xaxis = list(gridcolor = 'lightgray'), 
         yaxis = list(gridcolor = 'lightgray')) 
fig






library(plotly)
library (dplyr)
hansonrain<-read.csv("data/pixley/Pixley_ Tulare_ Capinero Creek Rain Gauge Rain Gauge 20241201-20260107.csv")
hansonwell<-read.csv("data/pixley/Pixley_ Tulare_ Capinero Creek Well Well 20241201-20260107.csv")
hansonwell$Date.and.Time<-as.POSIXct(hansonwell$Date.and.Time,format="%Y-%m-%d %H:%M:%S")

hourly_data<-hansonwell
daily_data<-hansonrain

daily_means <- hourly_data %>%
  mutate(Date = as.Date(Date.and.Time)) %>% # Extract the date part
  group_by(Date) %>%
  summarize(mean_value = mean(ft..below.ground., na.rm = TRUE)) %>%
  ungroup()

daily_data$Date<-as.Date(daily_data$Date,format="%Y-%m-%d (%A)")
#daily_data<-daily_data[daily_data$Date>="2025-10-01",]
#hourly_data<-hourly_data[hourly_data$Date.and.Time>="2025-10-01 00:00:00",]
#daily_means<-daily_means[daily_means$Date>="2025-10-01",]
str(hourly_data)
tail(hourly_data)
### Make 2 seperate interactive plots with the same scale

a<-plot_ly(data= daily_data, x = daily_data$Date, 
           y = daily_data$Rain..in., type = 'bar',marker =list(color="99CCff")) %>%
  layout(yaxis = list(autorange = "reversed"))

b<-plot_ly(data = hourly_data, x = hourly_data$Date.and.Time, y = hourly_data$ft..below.ground., name = 'Hourly', type = 'scatter', mode = 'lines',line = list(color = "grey"),opacity = 0.3) %>% 
  add_trace(data = daily_means, x = daily_means$Date, y = daily_means$mean_value, mode = 'lines',line = list(color = "black"),opacity = 0.9)  %>% 
  layout(yaxis = list(autorange = "reversed"))

fig <- subplot(a, b, nrows =2 ) %>% 
  layout(title = list(text = "Tulare"), 
         xaxis = list(gridcolor = 'lightgray'), 
         yaxis = list(gridcolor = 'lightgray')) 
fig



###Now calculate cumulative rainfall and julian day


library(plotly)
library (dplyr)
E5rain<-read.csv("data/escondido 5 1.7 data/JLDP Escondido 5 new Rain Gauge Rain Gauge 20231001-20260107.csv")
E5well<-read.csv("data/escondido 5 1.7 data/JLDP Escondido 5 Well Well 20231001-20260107.csv")
E5well$Date.and.Time<-as.POSIXct(E5well$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
E5well$ft..below.ground.<-E5well$ft..below.ground.-12.5

hourly_data<-E5well
daily_data<-E5rain

daily_means <- hourly_data %>%
  mutate(Date = as.Date(Date.and.Time)) %>% # Extract the date part
  group_by(Date) %>%
  summarize(mean_value = mean(ft..below.ground., na.rm = TRUE)) %>%
  ungroup()

daily_data$Date<-as.Date(daily_data$Date,format="%Y-%m-%d (%A)")
daily_data<-daily_data[daily_data$Date>="2025-10-01",]
daily_data <- daily_data %>% arrange(Date)
daily_data$cum<-cumsum(daily_data$Rain..in.)
hourly_data<-hourly_data[hourly_data$Date.and.Time>="2025-10-01 00:00:00",]
daily_means<-daily_means[daily_means$Date>="2025-10-01",]
str(hourly_data)
tail(hourly_data)
### Make 2 seperate interactive plots with the same scale
a<-plot_ly(data= daily_data, x = daily_data$Date, 
           y = daily_data$cum, type = 'bar',marker =list(color="99CCff"))

b<-plot_ly(data = hourly_data, x = hourly_data$Date.and.Time, y = hourly_data$ft..below.ground., name = 'Hourly', type = 'scatter', mode = 'lines',line = list(color = "grey"),opacity = 0.3) %>% 
  add_trace(data = daily_means, x = daily_means$Date, y = daily_means$mean_value, mode = 'lines',line = list(color = "black"),opacity = 0.9)  %>% 
  layout(yaxis = list(autorange = "reversed"))



fig <- subplot(b,a, nrows =2 ) %>% 
  layout(title = list(text = "Escondido 5"), 
         xaxis = list(gridcolor = 'lightgray'), 
         yaxis = list(gridcolor = 'lightgray')) 
fig




## 2024 wATER YEAR
library(plotly)
library (dplyr)
E5rain<-read.csv("data/escondido 5 1.7 data/In lab- old Escondido 5 Rain Gauge Rain Gauge 20231001-20260108.csv")
E5well<-read.csv("data/escondido 5 1.7 data/JLDP Escondido 5 Well Well 20231001-20260107.csv")
E5well$Date.and.Time<-as.POSIXct(E5well$Date.and.Time,format="%Y-%m-%d %H:%M:%S")
E5well$ft..below.ground.<-E5well$ft..below.ground.-12.5

hourly_data<-E5well
daily_data<-E5rain

daily_means <- hourly_data %>%
  mutate(Date = as.Date(Date.and.Time)) %>% # Extract the date part
  group_by(Date) %>%
  summarize(mean_value = mean(ft..below.ground., na.rm = TRUE)) %>%
  ungroup()

daily_data$Date<-as.Date(daily_data$Date,format="%Y-%m-%d (%A)")
daily_data<-daily_data[daily_data$Date>="2023-10-01"& daily_data$Date<="2024-04-04",]
daily_data <- daily_data %>% arrange(Date)
daily_data$cum<-cumsum(daily_data$Rain..in.)


daily_means<-daily_means[daily_means$Date>="2023-10-21"&daily_means$Date<="2024-04-04",]

### Make 2 seperate interactive plots with the same scale
a<-plot_ly(data= daily_data, x = daily_data$Date, 
           y = daily_data$cum, type = 'bar',marker =list(color="99CCff"))

b<-plot_ly(data = daily_means, x = daily_means$Date, y = daily_means$mean_value, name = 'daily', type = 'scatter', mode = 'lines',line = list(color = "black")) %>% 
  layout(yaxis = list(autorange = "reversed"))



fig <- subplot(b,a, nrows =2 ) %>% 
  layout(title = list(text = "Escondido 5"), 
         xaxis = list(gridcolor = 'lightgray'), 
         yaxis = list(gridcolor = 'lightgray')) 
fig
