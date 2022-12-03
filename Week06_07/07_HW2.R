### Data Science 2020 Spring(Eng)
### HW2, 21500268, Park Seongchan

# Reading Data, Library
load("~/homework2_2020.RData")
head(cust.df)
head(bankruptcy_df)

library(stringr)
library(tidyr)
library(lubridate)

## PART 1

# Question 1
str(cust.df)
sapply(cust.df[1:11], class)

# Question 2
summary(cust.df)
cust.df$custid <- as.character(cust.df$custid)
cust.df$state.of.res <- as.factor(cust.df$state.of.res)
cust.df$sex <- as.factor(cust.df$sex)
cust.df$marital.stat <- as.factor(cust.df$marital.stat)
cust.df$housing.type <- as.factor(cust.df$housing.type)

# Question 3
cust.df$custid <- str_pad(cust.df$custid, 7, 'left', pad = "0")
cust.df$custid <- str_pad(cust.df$custid, 8, 'left', pad = "c")

head(cust.df)

# Question 4
num_of_NA <- colSums(is.na(cust.df))
num_of_not_NA <- colSums(!is.na(cust.df))

num_of_NA
num_of_NA / (num_of_NA + num_of_not_NA)

# Question 5
summary(cust.df[, c(-4:-5)])
table(rowSums(is.na(cust.df[, c(-4:-5)])))

# Question 6
cust.df$is.employed <- as.factor(cust.df$is.employed)

cust.df$is.employed <- ifelse(is.na(cust.df$is.employed) == TRUE, "missing",
                              ifelse(cust.df$is.employed == "TRUE", "employed", "not employed"))

table(cust.df$is.employed)

# Question 7
median.income <- aggregate(Income ~ state.of.res, cust.df, median)
mean.income <- aggregate(Income ~ state.of.res, cust.df, mean)

avg_income <- cbind(median.income, mean.income)
avg_income <- avg_income[, c(1, 2, 4)]

names(avg_income)[2:3] <- ifelse(names(avg_income)[2:3] == "Income", "median.income", "mean.income")

head(avg_income)

# Question 8
cust.df <- merge(cust.df, avg_income, by = 'state.of.res')
cust.df$Income <- ifelse(is.na(cust.df$Income), cust.df$mean.income, cust.df$Income)
sum(is.na(cust.df$Income))

# Question 9
cust.df$income.relative <- cust.df$Income / cust.df$median.income
head(cust.df)

# Question 10
summary(cust.df)

par(mfrow = c(1, 2))
boxplot(cust.df$Income)
hist(cust.df$Income)

boxplot(cust.df$age)
hist(cust.df$age)

# Question 11
# Minus Income is impossible.
# age 0 need to confirm what it means.
# age 146.7 is necessary to check if it is possible.



## PART 2

# Question 1
head(bankruptcy_df)

# Question 2
bankruptcy_df <- bankruptcy_df[2:4]
bankruptcy_df <- spread(bankruptcy_df, index, rating)
head(bankruptcy_df)

save(bankruptcy_df, file = '~/bankruptcy_df_tidy.RData')

# Question 3
bankruptcy_df[2:8] <- ifelse(bankruptcy_df[2:8] == 'P', 'Positive', 
                             ifelse(bankruptcy_df[2:8] == 'A', 'Average', 
                                    ifelse(bankruptcy_df[2:8] == 'N', 'negative', 
                                           ifelse(bankruptcy_df[2:8] == 'B', 'Bankruptcy', 'Non-Bankruptcy'))))

head(bankruptcy_df)
