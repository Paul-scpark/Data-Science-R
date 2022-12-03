### Data Science 2020 Spring(Eng)
### HW1, 21500268, Park Seongchan

# Reading Data
data <- read.csv('bank_hw.csv')
head(data)

# Question 1
str(data)
table(data$age < 30)
table(data$age > 50)

# Question 2
data$balance_kw <- data$balance * 1200

# Question 3
table(data$y == 'yes')
mean(data$y == 'yes')

# Question 4
table(data$pdays == -1)
data$pdays <- ifelse(data$pdays == -1, NA, data$pdays)
sum(is.na(data$pdays))

# Question 5
table(data$job)

# Question 6
data$age_group <- ifelse(data$age < 20, 'under 20', 
                         ifelse(data$age < 30, '20~29', 
                                ifelse(data$age < 40, '30~39', 
                                       ifelse(data$age < 50, '40~49', 
                                              ifelse(data$age < 60, '50~59', 'over 60')))))
table(data$age_group)

# Question 7
tapply(data$y == 'yes', factor(data$age_group), mean)
aggregate(y == 'yes' ~ age_group, data, mean)

# Question 8
tapply(data$duration, factor(data$contact), mean)
aggregate(duration ~ contact, data, mean)

# Question 9
data <- data[order(data$age), ]
head(data)

# Question 10
save(data, file = 'bank_hw.rda')