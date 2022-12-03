## Using "weather.RData" Exercise

weather
library(tidyr)
library(stringr)
library(lubridate)

# Q1
weather <- weather[2:35]
weather <- gather(weather, date, val, 4:34)
weather <- spread(weather, measure, val)
weather$date <- str_replace(weather$date, 'X', '')
weather <- unite(weather, Date, year, month, date, sep = '-')

weather$Date <- as.Date(weather$Date)
weather <- weather[order(weather$Date), ]

# Q2
# 초기에 첫번째 열은 아무 의미 없어서 삭제

# Q3
# year, month, date를 합쳐서 새로운 Date 변수 만들기
# Date 변수의 type은 Date 형식으로 형 변환

# Q4
str(weather$PrecipitationIn)
weather$PrecipitationIn <- str_replace(weather$PrecipitationIn, 'T', '0')
weather$PrecipitationIn <- as.numeric(weather$PrecipitationIn)

# Q5 ?
str(weather)
weather$Events <- as.factor(weather$Events)
#weather[, 4:23] <- as.double(weather[, 4:23])
weather[,c(2, 4:23)] <- sapply(weather[,c(2, 4:23)], as.numeric)

# Q6
colSums(is.na(weather))

# Q7
summary(weather$Max.Humidity)
weather$Max.Humidity <- ifelse(weather$Max.Humidity == 1000, 100, weather$Max.Humidity)

# Q8
summary(weather$Mean.VisibilityMiles)
weather$Mean.VisibilityMiles <- ifelse(weather$Mean.VisibilityMiles == -1, 1, weather$Mean.VisibilityMiles)

# Q9
weather$Events <- ifelse(weather$Events == "", "None", weather$Events)
table(weather$events)

# Q10
names(weather) <- tolower(names(weather))