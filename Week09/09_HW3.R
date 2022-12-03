library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ROCR)
library(dplyr)

### Sing Variable Model for Regression
PRSA <- read.csv('PRSA_data.csv')
PRSA <- na.omit(PRSA)

PRSA <- PRSA[-1]
PRSA_train <- PRSA %>% filter(year < 2014) 
PRSA_test <- PRSA %>% filter(year == 2014)

hist(PRSA$pm2.5)
summary(PRSA$pm2.5)

data_2010 <- PRSA %>% filter(year == 2010)
data_2011 <- PRSA %>% filter(year == 2011)
data_2012 <- PRSA %>% filter(year == 2012)
data_2013 <- PRSA %>% filter(year == 2013)

data_2010 <- unite(data_2010, Date, year, month, day, hour, sep = '-')
data_2010$Date <- ymd_h(data_2010$Date)

data_2011 <- unite(data_2011, Date, year, month, day, hour, sep = '-')
data_2011$Date <- ymd_h(data_2011$Date)

data_2012 <- unite(data_2012, Date, year, month, day, hour, sep = '-')
data_2012$Date <- ymd_h(data_2012$Date)

data_2013 <- unite(data_2013, Date, year, month, day, hour, sep = '-')
data_2013$Date <- ymd_h(data_2013$Date)

PRSA_month1 <- PRSA_train %>% filter(month == 1)
PRSA_month2 <- PRSA_train %>% filter(month == 2)
PRSA_month3 <- PRSA_train %>% filter(month == 3)
PRSA_month4 <- PRSA_train %>% filter(month == 4)
PRSA_month5 <- PRSA_train %>% filter(month == 5)
PRSA_month6 <- PRSA_train %>% filter(month == 6)
PRSA_month7 <- PRSA_train %>% filter(month == 7)
PRSA_month8 <- PRSA_train %>% filter(month == 8)
PRSA_month9 <- PRSA_train %>% filter(month == 9)
PRSA_month10 <- PRSA_train %>% filter(month == 10)
PRSA_month11 <- PRSA_train %>% filter(month == 11)
PRSA_month12 <- PRSA_train %>% filter(month == 12)

PRSA_month1 <- unite(PRSA_month1, Date, year, month, day, hour, sep = '-')
PRSA_month1$Date <- ymd_h(PRSA_month1$Date)

PRSA_month2 <- unite(PRSA_month2, Date, year, month, day, hour, sep = '-')
PRSA_month2$Date <- ymd_h(PRSA_month2$Date)

PRSA_month3 <- unite(PRSA_month3, Date, year, month, day, hour, sep = '-')
PRSA_month3$Date <- ymd_h(PRSA_month3$Date)

PRSA_month4 <- unite(PRSA_month4, Date, year, month, day, hour, sep = '-')
PRSA_month4$Date <- ymd_h(PRSA_month4$Date)

PRSA_month5 <- unite(PRSA_month5, Date, year, month, day, hour, sep = '-')
PRSA_month5$Date <- ymd_h(PRSA_month5$Date)

PRSA_month6 <- unite(PRSA_month6, Date, year, month, day, hour, sep = '-')
PRSA_month6$Date <- ymd_h(PRSA_month6$Date)

PRSA_month7 <- unite(PRSA_month7, Date, year, month, day, hour, sep = '-')
PRSA_month7$Date <- ymd_h(PRSA_month7$Date)

PRSA_month8 <- unite(PRSA_month8, Date, year, month, day, hour, sep = '-')
PRSA_month8$Date <- ymd_h(PRSA_month8$Date)

PRSA_month9 <- unite(PRSA_month9, Date, year, month, day, hour, sep = '-')
PRSA_month9$Date <- ymd_h(PRSA_month9$Date)

PRSA_month10 <- unite(PRSA_month10, Date, year, month, day, hour, sep = '-')
PRSA_month10$Date <- ymd_h(PRSA_month10$Date)

PRSA_month11 <- unite(PRSA_month11, Date, year, month, day, hour, sep = '-')
PRSA_month11$Date <- ymd_h(PRSA_month11$Date)

PRSA_month12 <- unite(PRSA_month12, Date, year, month, day, hour, sep = '-')
PRSA_month12$Date <- ymd_h(PRSA_month12$Date)

PRSA <- unite(PRSA, Date, year, month, day, hour, sep = '-')
PRSA$Date <- ymd_h(PRSA$Date)

PRSA_train <- PRSA %>% filter(Date < as.Date('2014-01-01')) 
PRSA_test <- PRSA %>% filter(Date >= as.Date('2014-01-01'))

## Q1
par(mfrow = c(1, 3))
boxplot(PRSA_train$pm2.5~PRSA_train$Ir)
boxplot(PRSA_train$pm2.5~PRSA_train$Is)
boxplot(PRSA_train$pm2.5~PRSA_train$cbwd)

ggplot(PRSA_train, aes(x = factor(DEWP), y = pm2.5)) + geom_boxplot()
ggplot(PRSA_train, aes(x = TEMP, y = pm2.5)) + geom_point()
ggplot(PRSA_train, aes(x = PRES, y = pm2.5)) + geom_point()
ggplot(PRSA_train, aes(x = Iws, y = pm2.5)) + geom_point()

data <- 0
par(mfrow = c(1, 4))
for(i in (1:20)){
  data[i] <- 24 * i
  plot(data_2010[1:(i*24), ]$pm2.5, ylim = c(0, 1000)) + lines(data_2010[1:(i*24), ]$pm2.5) + 
    abline(v = c(data), col = 'red')
  
  plot(data_2011[1:(i*24), ]$pm2.5, ylim = c(0, 1000)) + lines(data_2011[1:(i*24), ]$pm2.5) + 
    abline(v = c(data), col = 'red')
  
  plot(data_2012[1:(i*24), ]$pm2.5, ylim = c(0, 1000)) + lines(data_2012[1:(i*24), ]$pm2.5) + 
    abline(v = c(data), col = 'red')
  
  plot(data_2013[1:(i*24), ]$pm2.5, ylim = c(0, 1000)) + lines(data_2013[1:(i*24), ]$pm2.5) + 
    abline(v = c(data), col = 'red')
}

par(mfrow = c(2, 3))
plot(PRSA_month1$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "1월") + 
  lines(PRSA_month1$pm2.5) + abline(h = mean(PRSA_month1$pm2.5), v = c(654, 1326, 1996), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month2$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "2월") + 
  lines(PRSA_month2$pm2.5) + abline(h = mean(PRSA_month2$pm2.5), v = c(672, 1344, 2034), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month3$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "3월") + 
  lines(PRSA_month3$pm2.5) + abline(h = mean(PRSA_month3$pm2.5), v = c(710, 1335, 2075), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month10$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "10월") + 
  lines(PRSA_month10$pm2.5) + abline(h = mean(PRSA_month10$pm2.5), v = c(742, 1359, 2099), 
                                     col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month11$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "11월") + 
  lines(PRSA_month11$pm2.5) + abline(h = mean(PRSA_month11$pm2.5), v = c(665, 1380, 2078), 
                                     col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month12$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "12월") + 
  lines(PRSA_month12$pm2.5) + abline(h = mean(PRSA_month12$pm2.5), v = c(745, 1488, 2102), 
                                     col = c('blue', 'red', 'red', 'red'))

par(mfrow = c(2, 3))
plot(PRSA_month4$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "4월") + 
  lines(PRSA_month4$pm2.5) + abline(h = mean(PRSA_month4$pm2.5), v = c(719, 1255, 1974), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month5$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "5월") + 
  lines(PRSA_month5$pm2.5) + abline(h = mean(PRSA_month5$pm2.5), v = c(738, 1450, 2141), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month6$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "6월") + 
  lines(PRSA_month6$pm2.5) + abline(h = mean(PRSA_month6$pm2.5), v = c(566, 1277, 1985), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month7$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "7월") + 
  lines(PRSA_month7$pm2.5) + abline(h = mean(PRSA_month7$pm2.5), v = c(745, 1486, 2174), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month8$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "8월") + 
  lines(PRSA_month8$pm2.5) + abline(h = mean(PRSA_month8$pm2.5), v = c(677, 1247, 1870), 
                                    col = c('blue', 'red', 'red', 'red'))
plot(PRSA_month9$pm2.5, pch = 1, cex = 0.5, ylim = c(0, 980), main = "9월") + 
  lines(PRSA_month9$pm2.5) + abline(h = mean(PRSA_month9$pm2.5), v = c(469, 1188, 1902), 
                                    col = c('blue', 'red', 'red', 'red'))

par(mfrow = c(1, 2))
plot(PRSA_month1$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "1월") + 
  lines(PRSA_month1$pm2.5) + abline(h = mean(PRSA_month1$pm2.5), v = c(654, 1326, 1996), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month1$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month1$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month1$Ir, pch = 1, cex = 0.3) + lines(PRSA_month1$Ir, col = 'pink')


plot(PRSA_month2$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "2월") + 
  lines(PRSA_month2$pm2.5) + abline(h = mean(PRSA_month2$pm2.5), v = c(672, 1344, 2034), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month2$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month2$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month2$Ir, pch = 1, cex = 0.3) + lines(PRSA_month2$Ir, col = 'pink')

plot(PRSA_month3$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "3월") + 
  lines(PRSA_month3$pm2.5) + abline(h = mean(PRSA_month3$pm2.5), v = c(710, 1335, 2075), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month3$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month3$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month3$Ir, pch = 1, cex = 0.3) + lines(PRSA_month3$Ir, col = 'pink')


plot(PRSA_month4$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "4월") + 
  lines(PRSA_month4$pm2.5, ) + abline(h = mean(PRSA_month4$pm2.5), v = c(719, 1255, 1974), 
                                      col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month4$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month4$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month4$Ir, pch = 1, cex = 0.3) + lines(PRSA_month4$Ir, col = 'pink')

plot(PRSA_month5$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "5월") + 
  lines(PRSA_month5$pm2.5) + abline(h = mean(PRSA_month5$pm2.5), v = c(738, 1450, 2141), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month5$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month5$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month5$Ir, pch = 1, cex = 0.3) + lines(PRSA_month5$Ir, col = 'pink')

plot(PRSA_month6$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "6월") + 
  lines(PRSA_month6$pm2.5) + abline(h = mean(PRSA_month6$pm2.5), v = c(566, 1277, 1985), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month6$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month6$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month6$Ir, pch = 1, cex = 0.3) + lines(PRSA_month6$Ir, col = 'pink')

plot(PRSA_month7$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "7월") + 
  lines(PRSA_month7$pm2.5) + abline(h = mean(PRSA_month7$pm2.5), v = c(745, 1486, 2174), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month7$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month7$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month7$Ir, pch = 1, cex = 0.3) + lines(PRSA_month7$Ir, col = 'pink')

plot(PRSA_month8$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "8월") + 
  lines(PRSA_month8$pm2.5) + abline(h = mean(PRSA_month8$pm2.5), v = c(677, 1247, 1870), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month8$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month8$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month8$Ir, pch = 1, cex = 0.3) + lines(PRSA_month8$Ir, col = 'pink')

plot(PRSA_month9$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "9월") + 
  lines(PRSA_month9$pm2.5) + abline(h = mean(PRSA_month9$pm2.5), v = c(469, 1188, 1902), 
                                    col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month9$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month9$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month9$Ir, pch = 1, cex = 0.3) + lines(PRSA_month9$Ir, col = 'pink')

plot(PRSA_month10$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "10월") + 
  lines(PRSA_month10$pm2.5) + abline(h = mean(PRSA_month10$pm2.5), v = c(742, 1359, 2099), 
                                     col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month10$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month10$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month10$Ir, pch = 1, cex = 0.3) + lines(PRSA_month10$Ir, col = 'pink')

plot(PRSA_month11$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "11월") + 
  lines(PRSA_month11$pm2.5) + abline(h = mean(PRSA_month11$pm2.5), v = c(665, 1380, 2078), 
                                     col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month11$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month11$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month11$Ir, pch = 1, cex = 0.3) + lines(PRSA_month11$Ir, col = 'pink')

plot(PRSA_month12$pm2.5, pch = 16, cex = 0.8, ylim = c(0, 980), main = "12월") + 
  lines(PRSA_month12$pm2.5) + abline(h = mean(PRSA_month12$pm2.5), v = c(745, 1488, 2102), 
                                     col = c('blue', 'red', 'red', 'red'))
par(new = TRUE)
plot(PRSA_month12$Iws, pch = 1, cex = 0.3, ylim = c(0, 500)) + lines(PRSA_month12$Iws, col = 'yellow')
par(new = TRUE)
plot(PRSA_month12$Ir, pch = 1, cex = 0.3) + lines(PRSA_month12$Ir, col = 'pink')

# Modeling_train
PRSA_train <- merge(PRSA_train, aggregate(pm2.5~DEWP, PRSA_train, median), by = 'DEWP')

colnames(PRSA_train)[3] <- 'pm2.5'
colnames(PRSA_train)[10] <- 'pm2.5_median'

summary(PRSA_train$pm2.5_median)
plot(PRSA_train$pm2.5_median, cex = 0.3) + lines(PRSA_train$pm2.5_median)

PRSA_train$group <- cut(PRSA_train$pm2.5_median, breaks = c(0, 10, 18, 22, 29, 56, 66, 75, 80, 85, 
                                                            91, 95, 100, 107, 118, 152, 173, 248, Inf), 
                        right = TRUE, labels = c(1:18))

sv_reg_DEWP <- tapply(PRSA_train$pm2.5, PRSA_train$group, mean)
PRSA_train$pred_pm2.5_mean <- sv_reg_DEWP[PRSA_train$group]
PRSA_train$error <- PRSA_train$pm2.5 - PRSA_train$pred_pm2.5_mean
MSE_train_mean <- mean(PRSA_train$error ** 2)
RMSE_train_mean <- sqrt(MSE_train_mean)
RMSE_train_mean
sd(PRSA_train$pm2.5)

RSS_mean <- sum(PRSA_train$error ** 2)
SStot_mean <- sum((PRSA_train$pm2.5 - mean(PRSA_train$pm2.5)) ** 2)
Rsq_mean <- 1 - RSS_mean/SStot_mean
Rsq_mean

## Modeling_test
PRSA_test <- merge(PRSA_test, aggregate(pm2.5~DEWP, PRSA_test, median), by = 'DEWP')

colnames(PRSA_test)[3] <- 'pm2.5'
colnames(PRSA_test)[10] <- 'pm2.5_median'

PRSA_test$group <- cut(PRSA_test$pm2.5_median, breaks = c(0, 10, 18, 22, 29, 56, 66, 75, 80, 85, 
                                                          91, 95, 100, 107, 118, 152, 173, 248, Inf), 
                       right = TRUE, labels = c(1:18))

sv_reg_DEWP <- tapply(PRSA_train$pm2.5, PRSA_train$group, mean)
PRSA_test$pred_pm2.5 <- sv_reg_DEWP[PRSA_test$group]
PRSA_test$error <- PRSA_test$pm2.5 - PRSA_test$pred_pm2.5
MSE_test <- mean(PRSA_test$error ** 2)
RMSE_test <- sqrt(MSE_test)
RMSE_test
sd(PRSA_test$pm2.5)

RSS_test <- sum(PRSA_test$error ** 2)
SStot_test <- sum((PRSA_test$pm2.5 - mean(PRSA_test$pm2.5)) ** 2)
Rsq_test <- 1 - RSS_test/SStot_test
Rsq_test

## Sharing with friends
PRSA_train$pm_delta <- 0
PRSA_train$Iws_delta <- 0

for (i in (1:33095)){
  PRSA_train$pm_delta[i] <- (PRSA_train$pm2.5[(i+1)] - PRSA_train$pm2.5[i]) / PRSA_train$pm2.5[i]
}

for (i in (1:33095)){
  PRSA_train$DEWP_delta[i] <- (PRSA_train$DEWP[(i+1)] - PRSA_train$DEWP[i]) / PRSA_train$DEWP[i]
}

for (i in (1:33095)){
  PRSA_train$Iws_delta[i] <- (PRSA_train$Iws[(i+1)] - PRSA_train$Iws[i]) / PRSA_train$Iws[i]
}

summary(PRSA_train$pm_delta)
summary(PRSA_train$DEWP_delta)
summary(PRSA_train$Iws_delta)

## Q7
par(mfrow = c(1, 2))
plot(PRSA_train$pred_pm2.5_mean, PRSA_train$pm2.5, xlim = c(0, 1000), main = "train data", 
     xlab = "Prediction", ylab = "Actual") + abline(a=0, b=1, col="red", lty=6)

plot(PRSA_train$pm2.5, PRSA_train$pred_pm2.5_mean, ylim = c(0, 1000), main = "train data", 
     xlab = "Actual", ylab = "prediction") + abline(a=0, b=1, col="red", lty=6)

par(mfrow = c(1, 2))
plot(PRSA_test$pred_pm2.5, PRSA_test$pm2.5, xlim = c(0, 1000), main = "test data", 
     xlab = "Prediction", ylab = "Actual") + abline(a=0, b=1, col="red", lty=6)

plot(PRSA_test$pm2.5, PRSA_test$pred_pm2.5, ylim = c(0, 1000), main = "test data", 
     xlab = "Actual", ylab = "Prediction") + abline(a=0, b=1, col="red", lty=6)


### Single Variable Model for Classification
load('bankruptcy.RData')
head(bankruptcy_train)
head(bankruptcy_test)

str(bankruptcy_train)
#bankruptcy_train[, c(2:8)] <- sapply(bankruptcy_train[, c(2:8)], as.factor)

bankruptcy_train$Class <- as.factor(bankruptcy_train$Class)
bankruptcy_train$Competitiveness <- as.factor(bankruptcy_train$Competitiveness)
bankruptcy_train$Credibility <- as.factor(bankruptcy_train$Credibility)
bankruptcy_train$`Financial Flexibility` <- as.factor(bankruptcy_train$`Financial Flexibility`)
bankruptcy_train$`Industrial Risk` <- as.factor(bankruptcy_train$`Industrial Risk`)
bankruptcy_train$`Management Risk` <- as.factor(bankruptcy_train$`Management Risk`)
bankruptcy_train$`Operating Risk` <- as.factor(bankruptcy_train$`Operating Risk`)
summary(bankruptcy_train)

str(bankruptcy_test)
#bankruptcy_test[, c(2:8)] <- sapply(bankruptcy_test[, c(2:8)], as.factor)

bankruptcy_test$Class <- as.factor(bankruptcy_test$Class)
bankruptcy_test$Competitiveness <- as.factor(bankruptcy_test$Competitiveness)
bankruptcy_test$Credibility <- as.factor(bankruptcy_test$Credibility)
bankruptcy_test$`Financial Flexibility` <- as.factor(bankruptcy_test$`Financial Flexibility`)
bankruptcy_test$`Industrial Risk` <- as.factor(bankruptcy_test$`Industrial Risk`)
bankruptcy_test$`Management Risk` <- as.factor(bankruptcy_test$`Management Risk`)
bankruptcy_test$`Operating Risk` <- as.factor(bankruptcy_test$`Operating Risk`)
summary(bankruptcy_test)


## Q1
aggregate(Competitiveness~Class, bankruptcy_train, summary)[2]
aggregate(Credibility~Class, bankruptcy_train, summary)[2]
aggregate(`Financial Flexibility`~Class, bankruptcy_train, summary)[2]
aggregate(`Industrial Risk`~Class, bankruptcy_train, summary)[2]
aggregate(`Management Risk`~Class, bankruptcy_train, summary)[2]
aggregate(`Operating Risk`~Class, bankruptcy_train, summary)[2]

bankruptcy_train[3:8] <- ifelse(bankruptcy_train[3:8] == 'Negative', -1, 
                                ifelse(bankruptcy_train[3:8] == 'Average', 0, 1))
bankruptcy_train$eval <- rowSums(bankruptcy_train[3:8])
aggregate(eval ~ Class, bankruptcy_train, mean)

threshold <- -2
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
head(bankruptcy_train[, c('eval', 'Class', 'prediction')], 10)

conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train <- sum(diag(conf.table)) / sum(conf.table)
accuracy_train

calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}

calAUC(bankruptcy_train$eval, bankruptcy_train$Class)

## Q2
bankruptcy_test[3:8] <- ifelse(bankruptcy_test[3:8] == 'Negative', -1, 
                               ifelse(bankruptcy_test[3:8] == 'Average', 0, 1))
bankruptcy_test$eval <- rowSums(bankruptcy_test[3:8])
aggregate(eval ~ Class, bankruptcy_test, mean)

threshold <- -2
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
head(bankruptcy_test[, c('eval', 'Class', 'prediction')], 10)

conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test <- sum(diag(conf.table_test)) / sum(conf.table_test)
accuracy_test

calAUC(bankruptcy_test$eval, bankruptcy_test$Class)

## Q3
accuracy_train
accuracy_test

## Q4
threshold <- -6
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train1 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train1 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- -5
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train2 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train2 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- -4
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train3 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train3 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- -3
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train4 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train4 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- -2
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train5 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train5 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- -1
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train6 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train6 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- 0
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train7 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train7 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- 1
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train8 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train8 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- 2
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train9 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train9 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- 3
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train10 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train10 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- 4
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train11 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train11 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

threshold <- 5
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table_train <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
precision_train12 <- conf.table_train[2, 2] / sum(conf.table_train[2, ])
recall_train12 <- conf.table_train[2, 2] / sum(conf.table_train[, 2])

precision_recall <- data.frame(precision = c(precision_train1, precision_train2, precision_train3, 
                                             precision_train4, precision_train5, precision_train6, 
                                             precision_train7, precision_train8, precision_train9, 
                                             precision_train10, precision_train11, precision_train12), 
                              recall = c(recall_train1, recall_train2, recall_train3, recall_train4, 
                                         recall_train5, recall_train6, recall_train7, recall_train8, 
                                         recall_train9, recall_train10, recall_train11, recall_train12), 
                              threshold = c(-6:5))

plot(precision_recall$precision, precision_recall$recall) + 
  lines(precision_recall$precision, precision_recall$recall)


threshold <- -6
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test1 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test1 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- -5
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test2 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test2 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- -4
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test3 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test3 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- -3
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test4 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test4 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- -2
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test5 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test5 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- -1
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test6 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test6 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- 0
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test7 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test7 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- 1
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test8 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test8 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- 2
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test9 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test9 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- 3
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test10 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test10 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- 4
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test11 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test11 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

threshold <- 5
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table_test <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
precision_test12 <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
recall_test12 <- conf.table_test[2, 2] / sum(conf.table_test[, 2])

precision_recall_test <- data.frame(precision = c(precision_test1, precision_test2, precision_test3, 
                                                 precision_test4, precision_test5, precision_test6, 
                                                 precision_test7, precision_test8, precision_test9, 
                                                 precision_test10, precision_test11, precision_test12), 
                                   recall = c(recall_test1, recall_test2, recall_test3, recall_test4, 
                                              recall_test5, recall_test6, recall_test7, recall_test8, 
                                              recall_test9, recall_test10, recall_test11, recall_test12), 
                                   threshold = c(-6:5))

plot(precision_recall_test$precision, precision_recall_test$recall) + 
  lines(precision_recall_test$precision, precision_recall_test$recall)

## Q5
precision_recall$f1 <- 2 * ((precision_recall$precision * precision_recall$recall) / 
                              (precision_recall$precision + precision_recall$recall))
precision_recall[order(precision_recall$f1, decreasing = T), ]

precision_recall_test$f1 <- 2 * ((precision_recall_test$precision * precision_recall_test$recall) / 
                                  (precision_recall_test$precision + precision_recall_test$recall))
precision_recall_test[order(precision_recall_test$f1, decreasing = T), ]

## Q6
bankruptcy_train$Class <- ifelse(bankruptcy_train$Class == 'Bankruptcy', 1, 2)
bankruptcy_train$prediction <- ifelse(bankruptcy_train$prediction == 'FALSE', 1, 2)

plot(performance(prediction(bankruptcy_train$prediction, bankruptcy_train$Class), 'tpr', 'fpr')) + 
  abline(a = 0, b = 1, col = 'red', lty = 6)

bankruptcy_test$Class <- ifelse(bankruptcy_test$Class == 'Bankruptcy', 1, 2)
bankruptcy_test$prediction <- ifelse(bankruptcy_test$prediction == 'FALSE', 1, 2)

plot(performance(prediction(bankruptcy_test$prediction, bankruptcy_test$Class), 'tpr', 'fpr')) + 
  abline(a = 0, b = 1, col = 'red', lty = 6)

## Q7
threshold <- -6
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train1 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -5
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train2 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -4
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train3 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -3
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train4 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -2
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train5 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -1
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train6 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 0
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train7 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 1
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train8 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 2
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train9 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 3
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train10 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 4
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train11 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 5
bankruptcy_train$prediction <- bankruptcy_train$eval > threshold
conf.table <- table(pred = bankruptcy_train$prediction, actual = bankruptcy_train$Class)
accuracy_train12 <- sum(diag(conf.table)) / sum(conf.table)

accuracy_train_df <- data.frame(threshold = c(-6:5), 
                                accuracy = c(accuracy_train1, accuracy_train2, accuracy_train3, 
                                             accuracy_train4, accuracy_train5, accuracy_train6, 
                                             accuracy_train7, accuracy_train8, accuracy_train9, 
                                             accuracy_train10, accuracy_train11, accuracy_train12))
accuracy_train_df[order(accuracy_train_df$accuracy, decreasing = T), ]

threshold <- -6
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test1 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -5
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test2 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -4
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test3 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -3
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test4 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -2
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test5 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- -1
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test6 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 0
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test7 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 1
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test8 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 2
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test9 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 3
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test10 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 4
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test11 <- sum(diag(conf.table)) / sum(conf.table)

threshold <- 5
bankruptcy_test$prediction <- bankruptcy_test$eval > threshold
conf.table <- table(pred = bankruptcy_test$prediction, actual = bankruptcy_test$Class)
accuracy_test12 <- sum(diag(conf.table)) / sum(conf.table)

accuracy_test_df <- data.frame(threshold = c(-6:5), 
                               accuracy = c(accuracy_test1, accuracy_test2, accuracy_test3, 
                                            accuracy_test4, accuracy_test5, accuracy_test6, 
                                            accuracy_test7, accuracy_test8, accuracy_test9, 
                                            accuracy_test10, accuracy_test11, accuracy_test12))
accuracy_test_df[order(accuracy_test_df$accuracy, decreasing = T), ]
