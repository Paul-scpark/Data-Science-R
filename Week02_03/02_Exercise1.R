## Using "mtcars" data's Exercise

data <- mtcars

# Q1
data$mpg <- data$mpg * 1.609344/3.785412

# Q2
head(data[order(data$mpg), ], 3)
tail(data[order(data$mpg), ], 3)

# Q3
tapply(data$mpg, factor(data$cyl), mean)
aggregate(mpg ~ cyl, data, mean)

# Q4
Genesis <- c(8.5, 8, 490, 3.8, 19.8, 1, 0, 5, 4)
data <- rbind(data, Genesis)
rownames(data)[33] <- "Genesis"

# Q5
tapply(data$mpg, factor(data$hp > 100), mean)
aggregate(mpg ~ hp < 100, data, mean)

# Q6
data[order(data$mpg), ]
data[order(data$hp, decreasing = T), ]

# Q7
data$wt <- data$wt * 453.592
heavy <- head(data[order(data$wt), ], 5)
light <- tail(data[order(data$wt), ], 5)

# Q8
mean(heavy$mpg)
mean(heavy$hp)
mean(light$mpg)
mean(light$hp)

# Q9
price <- c(rep(1000, 33))
data <- cbind(data, price)

# Q10
write.csv(data, file = "data.csv")

# Q11
save(data, file = "data.rda")