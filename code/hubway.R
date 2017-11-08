# Hubway data

options(scipen=999) # avoid scientic notation

# Set up current directory
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggmap)
setwd("/Users/albertogonzalez/Dropbox/work_17/bestiario/Mark")


# Import Natgeo water data set and have a initial overiew
hubway_1 = read.csv("hubway_shorttrips-2013.csv")
str(hubway_1)
head(hubway_1)

# Let's start checking some important variables, like the dates range
dates_range = as.data.frame(table(hubway_1$start_date))
str(dates_range)
head(dates_range)

# histogram of trips by day
g0 = ggplot(dates_range,aes(Freq)) + geom_histogram()
g0 + theme_minimal()



# filter one day to visualize its results
one_day = hubway_1 %>%
  filter(start_date == "10/1/2013")


# plot maps
q = get_map(location = "boston university", zoom = 13)
q_1 = ggmap(q) 
q_1 + geom_point(aes(x=end_statn_long, y=end_statn_lat,size = duration), data=one_day,col = "red",alpha=0.4, shape = 21) + scale_size(range=c(1,10)) + facet_wrap(~subsc_type)

#q_1 + geom_point(aes(x=end_statn_long, y=end_statn_lat,size = duration), data=hubway_1,col = "red",alpha=0.4, shape = 21) + scale_size(range=c(1,10)) + facet_wrap(~start_hour_2)

# long lat by gender, all days
g1 = ggplot(one_day,aes(start_statn_long,start_statn_lat)) + geom_point() + facet_wrap(~gender)
g1 + theme_minimal()

# Add duration as size and facet by gender
g2 = ggplot(one_day,aes(start_statn_long,start_statn_lat, size = duration)) + geom_point() + facet_wrap(~gender)
g2 + theme_minimal()

# Add some alpha to avoid overplotting
g3 = ggplot(one_day,aes(start_statn_long,start_statn_lat, size = duration)) + geom_point(alpha = 0.5) + facet_wrap(~gender)
g3 + theme_minimal()

# Delete circle fill to improve legibility
g4 = ggplot(one_day,aes(start_statn_long,start_statn_lat, size = duration)) + geom_point(shape = 21) + facet_wrap(~gender)
g4 + theme_minimal()

# Add gender as fill
g5 = ggplot(one_day,aes(start_statn_long,start_statn_lat, size = duration, fill = gender)) + geom_point(shape = 21)
g5 + theme_minimal()

# Facet by type of user
g6 = ggplot(one_day,aes(start_statn_long,start_statn_lat, size = duration, fill = gender)) + geom_point(shape = 21) + facet_wrap(~subsc_type)
g6 + theme_minimal()

g7 = ggplot(one_day,aes(start_statn_long,start_statn_lat, size = duration, fill = gender)) + geom_point(shape = 22) + facet_wrap(~subsc_type)
g7 + theme_minimal()

g8 = ggplot(one_day,aes(start_statn_long,start_statn_lat, size = duration, fill = gender)) + geom_point(shape = 23) + facet_wrap(~subsc_type)
g8 + theme_minimal()


# Lets try to assign day of week and study seasonality
head(hubway_1)
str(hubway_1)

# From factor to date
hubway_1$start_date = as.Date(hubway_1$start_date,format = "%m/%d/%Y")
str(hubway_1)
head(hubway_1)

hubway_1$week_day = wday(hubway_1$start_date,label = TRUE)
head(hubway_1)


# Total trips by weekday
trips_by_day = as.data.frame(table(hubway_1$week_day))
trips_by_day

g9 = ggplot(trips_by_day,aes(Var1,Freq)) + geom_bar(stat = "identity")
g9 + theme_minimal()

# Lon / lat, duration as size, gender as color, weekday as facet
g10 = ggplot(hubway_1,aes(start_statn_long,start_statn_lat, size = duration, fill = gender)) + geom_point(shape = 21)
g10 + theme_minimal() + facet_wrap(~week_day)

# Change gender to faceting mode
g11 = ggplot(hubway_1,aes(start_statn_long,start_statn_lat)) + geom_point(shape = 3)
g11 + theme_minimal() + facet_grid(gender~week_day)

# Could the starting hour of the day help us?
# We first need to convert start time factor to date
head(hubway_1)
hubway_1$start_hour_1 = hms(hubway_1$start_time)
hubway_1$start_hour_2 = hour(hubway_1$start_hour_1)

# Let's use 5 variables together in the same viz
g11 = ggplot(hubway_1,aes(start_statn_long,start_statn_lat,fill=gender)) + geom_point(shape = 21) + theme_minimal() + facet_grid(start_hour_2~week_day)
g11

g12 = ggplot(hubway_1,aes(start_statn_long,start_statn_lat,fill=gender)) + geom_point(shape = 21) + theme_minimal() + facet_wrap(~start_hour_2)
g12

# We can also add the month to see if it might show any given pattern
hubway_1$month = month(hubway_1$start_date)

g13 = ggplot(hubway_1,aes(start_statn_long,start_statn_lat,fill=gender)) + geom_point(shape = 21) + theme_minimal() + facet_wrap(~month)
g13


# It's still difficult to spot seasonality patterns, let's try to change
# the geometric shape and pass form circle x/y to line (time series)

g14 = ggplot(hubway_1,aes(start_date,duration, group = gender)) + geom_line() + theme_minimal()
g14


# We need to do some transformations
hubway_2 = hubway_1 %>%
  group_by(start_date,gender)%>%
  summarise(count = n())

head(hubway_2)

hubway_3 = as.data.frame(hubway_2)
str(hubway_3)


# plot results
g15 = ggplot(hubway_3,aes(start_date,count,group=gender,color=gender)) + geom_line() + theme_minimal()
g15


# Let's add in weekday in the groupings
hubway_4 = hubway_1 %>%
  group_by(start_date,gender,week_day)%>%
  summarise(count = n())

hubway_5 = as.data.frame(hubway_4)
head(hubway_5)


# plot results
g16 = ggplot(hubway_5,aes(start_date,count,group=gender,color=gender)) + geom_line() + theme_minimal() + facet_wrap(~week_day)
g16


# Let's add in hour in the groupings
hubway_6 = hubway_1 %>%
  group_by(start_date,gender,start_hour_2)%>%
  summarise(count = n())

hubway_7 = as.data.frame(hubway_6)
head(hubway_7)


# plot results
g17 = ggplot(hubway_7,aes(start_date,count,group=gender,color=gender)) + geom_line() + theme_minimal() + facet_wrap(~start_hour_2)
g17


# Let's add in hour in the groupings
hubway_8 = hubway_1 %>%
  group_by(start_date,gender,strt_statn_name)%>%
  summarise(count = n())

hubway_9 = as.data.frame(hubway_8)
head(hubway_9)

# plot results
g18 = ggplot(hubway_9,aes(start_date,count,group=gender,color=gender)) + geom_line() + theme_minimal() + facet_wrap(~strt_statn_name)
g18















