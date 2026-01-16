
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
#E5well$ft..below.ground.<-E5well$ft..below.ground.-12.5
tail(E5well)
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
           y = daily_data$Rain..in., type = 'bar',marker =list(color="99CCff"))

b<-plot_ly(data = daily_means, x = daily_means$Date, y = daily_means$mean_value, name = 'daily', type = 'scatter', mode = 'lines',line = list(color = "black")) %>% 
  layout(yaxis = list(autorange = "reversed"))



fig <- subplot(b,a, nrows =2 ) %>% 
  layout(title = list(text = "Escondido 5"), 
         xaxis = list(gridcolor = 'lightgray'), 
         yaxis = list(gridcolor = 'lightgray')) 
fig



#########hourly to daily
E5corr<-read.csv("data/escondido 5 1.7 data/Escondido 5 20231001-20260107 Corrected.csv")
str(E5corr)
E5corr$Date <- lubridate::mdy(E5corr$Date)
a<-ggplot(E5corrdaily)+geom_line(aes(x=Date,y=Corrected.ft.below.ground))

E5corrdaily<-aggregate(cbind(Raw.ft.below.ground,Corrected.ft.below.ground)~
                         Date,E5corr,FUN=mean)

write.csv(E5corrdaily,"data/escondido 5 1.7 data/Escondido5Corrected_daily.csv")



#############################################################################
##Yearly rainfall comparison
library(dataRetrieval)
library(dplyr)
library(fasstr)

rain<-read.csv("data/escondido 5 1.7 data/Escondido_5_Daily_Corrected_Jan_2026.csv")
rain$Date<- lubridate::mdy(rain$Date)

# Add date variables, including the DayofYear (dowy)
dates_with_vars <- add_date_variables(rain, water_year_start = 10)
dates_with_vars<-as.data.frame(dates_with_vars)

#Calculate cumulative rainfall per water year 
rain_adj <- dates_with_vars %>%
  group_by(WaterYear) %>%
  mutate(cumulative_rain = cumsum(Rain_in))

rain_adj$WaterYear<-as.character(rain_adj$WaterYear)




rain_adj <- rain_adj %>%
  filter(!is.na(Corrected.mean.ft.below.ground)) %>% 
  group_by(WaterYear) %>%
  arrange(Date) %>% 
  mutate(SOWY_Depth = first(Corrected.mean.ft.below.ground),
         Depth_Difference = Corrected.mean.ft.below.ground - SOWY_Depth
  ) %>%
  ungroup()

rain_adj<-as.data.frame(rain_adj)
rain_adj$Depth_Difference<-rain_adj$Depth_Difference * -1
tail(rain_adj)
rain_adj


coeff <- 0.5
p2 <- ggplot(data=rain_adj, aes(x=DayofYear, y=cumulative_rain)) +
  geom_line(alpha=0.7,aes(color=WaterYear),linewidth=3)+theme_bw()+
  geom_line(aes(y=Depth_Difference/coeff,color=WaterYear),linewidth = 1)+
  scale_y_continuous(name = "Cumulative Rainfall",
    sec.axis = sec_axis(~.*coeff, name="Change in Depth to Groundwater")
  )

p3<- ggplot(data=rain_adj,aes(x=cumulative_rain))+
  geom_point(aes(y=Depth_Difference,color=WaterYear))
p3

#write.csv(rain_adj,"data/Escondido5_summary_adj.csv")
#######################################
##try 7 day rolling sum
# Calculate the 7-day rolling sum
library(zoo)
rain_adj <- rain_adj %>%
  mutate(Roll_7D_SumRain = rollsumr(Rain_in, k = 14, fill = NA))

rain_adj <- rain_adj %>%
  arrange(Date) %>% # Ensure data is sorted by date
  mutate(
    value_7_days_ago = lag(Corrected.mean.ft.below.ground, n = 14), # Create a column with the value from 7 days ago
    value_3_days_ahead= lead(Corrected.mean.ft.below.ground, n=0),
    day7_diff_gw = value_7_days_ago - value_3_days_ahead # Calculate the difference
  )

rain_adj$Roll_7D_SumRain <- format(rain_adj$Roll_7D_SumRain, scientific = FALSE)
rain_adj$Roll_7D_SumRain<-as.numeric(rain_adj$Roll_7D_SumRain)
rain_adj$Roll_7D_SumRain <- round(rain_adj$Roll_7D_SumRain, 2)
##14 days for both works beast now
ggplot(data=rain_adj,aes(x=Roll_7D_SumRain))+
  geom_point(aes(y=day7_diff_gw,color=WaterYear))

##sum non 0 to non zero rain day- storm specific to reduce noise
precip_runs <- rle(rain_adj$Rain_in > 0.04)
rain_test<-rain_adj

# 3. Create a unique storm ID for each rainy period
# Initialize a vector for storm IDs, defaulting to 0 for no storm
rain_test$Storm_ID <- 0

# Generate IDs only for the runs where precipitation was TRUE (rle$values)
# rep() is used to repeat the sequence of IDs by the length of each run
storm_ids <- rep(1:sum(precip_runs$values), times = precip_runs$lengths[precip_runs$values])

# Assign the generated IDs to the corresponding rainy days in the data frame
rain_test$Storm_ID[rain_test$Rain_in > 0.04] <- storm_ids

storms <- rain_test %>%
  arrange(Date) %>%
  mutate(
    storm_expanded = Storm_ID,
    storm_expanded = if_else(
      storm_expanded == 0 &
        lag(Storm_ID) > 0 &
        Date - lag(Date) == 1,
      lag(Storm_ID),
      storm_expanded
    ),
    storm_expanded = if_else(
      storm_expanded == 0 &
        lead(Storm_ID) > 0 &
        lead(Date) - Date == 1,
      lead(Storm_ID),
      storm_expanded
    )
  )

storms<-storms[storms$storm_expanded!=0,]

storm_summary <- storms %>%
  filter(!is.na(storm_expanded)) %>%      # drop non-storm days
  arrange(Date) %>%
  group_by(storm_expanded) %>%
  summarise(
    start_date = first(Date),
    end_date   = last(Date),
    total_rain = sum(Rain_in, na.rm = TRUE),
    rain_int = max(Rain_in),
    start_depth = first(Corrected.mean.ft.below.ground),
    end_depth   = last(Corrected.mean.ft.below.ground),
    diff_depth = start_depth - end_depth,
    .groups = "drop"
  )

ggplot(data=storm_summary,aes(x=total_rain))+
  geom_point(aes(y=diff_depth))+ylim(-0.5,1.5)
