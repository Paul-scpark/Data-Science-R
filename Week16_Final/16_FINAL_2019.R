## 2019_spring_midterm
load('midterm2019_ds.RData')
View(pums.sample)

#Q1
str(pums.sample)
sapply(pums.sample, class)

#Q2
pums.sample$SEX <- ifelse(pums.sample$SEX == 1, 'Male', 'Female')
table(pums.sample$SEX)
pums.sample$SEX <- as.factor(pums.sample$SEX)

#Q3
pums.sample$MAR <- ifelse(pums.sample$MAR == 1, 'Married', 
                          ifelse(pums.sample$MAR == 2, 'Widowed', 
                                 ifelse(pums.sample$MAR == 3, 'Divorced', 
                                        ifelse(pums.sample$MAR == 4, 'Separated', 'Never married or under 15 years old'))))
table(pums.sample$MAR)
pums.sample$MAR <- as.factor(pums.sample$MAR)

#Q4
colSums(is.na(pums.sample))
colSums(is.na(pums.sample)) / nrow(pums.sample)

#Q5
table(pums.sample$FER)
aggregate(FER == 'yes' ~ SEX, pums.sample, sum)

table(pums.sample$AGEP > 50)
table(Female_data$AGEP > 50)

library(dplyr)
Female_data <- pums.sample %>% filter(SEX == 'Female')
aggregate(FER == 'yes' ~ AGEP < 15, Female_data, sum)
aggregate(FER == 'yes' ~ AGEP > 50, Female_data, sum)

#Q6
summary(pums.sample)

#Q7
aggregate(COW ~ SCHL, pums.sample, table)

#Q8
table(pums.sample$AGEP < 10)
pums.sample$age_group <- ifelse(pums.sample$AGEP < 20, '10대',
                                ifelse(pums.sample$AGEP < 30, '20대', 
                                       ifelse(pums.sample$AGEP < 40, '30대', 
                                              ifelse(pums.sample$AGEP < 50, '40대', 
                                                     ifelse(pums.sample$AGEP < 60, '50대', '60대 이상')))))
table(pums.sample$age_group)
aggregate(PINCP ~ age_group, pums.sample, mean)

#Q9
plot(pums.sample$WKHP, pums.sample$PINCP)


#Q1
View(automobile.long)

#Q2
automobile.long <- spread(automobile.long, specification, value)
