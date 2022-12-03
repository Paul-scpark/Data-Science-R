getwd()

library(stringr)
library(tidyr)
library(lubridate)

#Q1
car_df <- read.csv("cars04.csv")
View(car_df)

#Q2
str(car_df)
# 428 obs. of  19 variables

#Q3
car_df$name <- as.character(car_df$name)

#Q4
mean(car_df$msrp - car_df$dealer_cost)

#Q5
high_city_mpg <- head(car_df[order(car_df$city_mpg, decreasing = T), ], 1)
high_hwy_mpg <- head(car_df[order(car_df$hwy_mpg, decreasing = T), ], 1)
high_city_mpg$city_mpg - high_city_mpg$hwy_mpg

#Q6
colSums(car_df[2:6])
sum(colSums(car_df[2:6]))
nrow(car_df[2:6]) - sum(rowSums(car_df[2:6]))

#Q7
aggregate(weight ~ suv == TRUE, car_df, mean)[2, 2] - 
  aggregate(weight ~ minivan == TRUE, car_df, mean)[2, 2]

#Q8
car_df$avg_mpg <- (car_df$city_mpg + car_df$hwy_mpg) / 2

#Q9
cuts <- quantile(car_df$avg_mpg, c(0, 0.2, 0.8, 1), na.rm = T)
car_df$eco_grade <- cut(car_df$avg_mpg, breaks = cuts, labels = c('good', 'normal', 'bad'))
table(car_df$eco_grade)

#Q10
aggregate(hwy_mpg ~ all_wheel == TRUE, car_df, mean)[2, 2] - 
  aggregate(hwy_mpg ~ rear_wheel == TRUE, car_df, mean)[2, 2]


#Q1
View(anscombe)
sapply(anscombe, mean)
sapply(anscombe, sd)

#Q2
plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)

#Q3
x <- anscombe[, 1:4]
y <- anscombe[, 5:8]

x_tidy <- gather(x, key.x, value.x, 1:4)
y_tidy <- gather(y, key.y, value.y, 1:4)

anscombe_tidy <- cbind(x_tidy, y_tidy)
