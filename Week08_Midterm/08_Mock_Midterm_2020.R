## Midterm
## Data Science 2020 Spring
## 21500268, Park Seongchan

install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("dplyr")

library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)

### Part 1

## Q1
insurance <- read_csv("insurance.csv")
head(insurance)

## Q2
str(insurance)
summary(insurance)
table(rowSums(is.na(insurance)))
colSums(is.na(insurance))

## Q3
insurance$bmi_group <- ifelse(insurance$bmi <= 18.5, 'light', 
                              ifelse(insurance$bmi <= 24.9, 'normal', 'heavy'))
head(insurance)
aggregate(charges ~ bmi_group, insurance, mean)

## Q4
aggregate(bmi ~ sex, insurance, mean)
aggregate(bmi ~ sex, insurance, sd)

female <- insurance %>% filter(sex == 'female')
male <- insurance %>% filter(sex == 'male')

par(mfrow = c(1, 2))
hist(female$bmi)
hist(male$bmi)

## Q5 
par(mfrow = c(1, 2))
quantile(insurance$charges)
table(insurance$charges >= 20000)
hist(insurance$charges)
boxplot(insurance$charges)

## Q6
aggregate(charges ~ smoker, insurance, mean)
aggregate(bmi ~ smoker, insurance, mean)
aggregate(age ~ smoker, insurance, mean)

## Q7
aggregate(charges ~ children, insurance, mean)

## Q8
cuts <- quantile(insurance$age, c(0, 0.1, 0.9, 1), na.rm = T)
cuts
young_group <- insurance %>% filter(age < 19)
old_group <- insurance %>% filter(age > 59)

mean(young_group$charges)
mean(old_group$charges)

## Q9
aggregate(charges ~ sex, insurance, mean)

## Q10
aggregate(smoker == 'yes' ~ sex, insurance, mean)
aggregate(age ~ sex, insurance, mean)
aggregate(bmi ~ sex, insurance, mean)
aggregate(children ~ sex, insurance, mean)


### Part 2
automobile <- read.table('automobile.tsv', header = TRUE, sep = '')
View(automobile)

## Q1
table(automobile$specification)
# No tidy data
# 'Specification' column have a lot of variables.

## Q2
automobile <- spread(automobile, specification, value)
save(automobile, file = "C:/Users/Paul/Documents\\automobile_tidy.RData")