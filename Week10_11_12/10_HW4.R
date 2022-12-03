install.packages("stringr")
install.packages("ggplot2")
install.packages("ROCR")
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("class")
install.packages("caret")

library(stringr)
library(ggplot2)
library(ROCR)
library(dplyr)
library(rpart)
library(rpart.plot)
library(class)
library(caret)

### Q1
PRSA <- read.csv("PRSA_data.csv")
PRSA <- PRSA[-1]
PRSA <- na.omit(PRSA)
sum(is.na(PRSA$pm2.5))

PRSA$bad_air <- ifelse(PRSA$pm2.5 > 75, TRUE, FALSE)
table(PRSA$bad_air)

PRSA_train <- subset(PRSA, year < 2014)
table(PRSA_train$bad_air)
PRSA_test <- subset(PRSA, year == 2014)
table(PRSA_test$bad_air)

### Q2
# Data 확인
par(mfrow = c(1, 2))
boxplot(PRSA_train$DEWP ~ PRSA_train$bad_air, main = "DEWP")
boxplot(PRSA_train$TEMP ~ PRSA_train$bad_air, main = "TEMP")

boxplot(PRSA_train$pm2.5 ~ PRSA_train$Ir, main = "Ir")
boxplot(PRSA_train$pm2.5 ~ PRSA_train$Is, main = "Is")

boxplot(PRSA_train$PRES ~ PRSA_train$bad_air, main = "PRES")
boxplot(PRSA_train$pm2.5 ~ PRSA_train$cbwd, main = "cbwd")

# DEWP와 TEMP 변수 이용
par(mfrow = c(1, 1))
PRSA_model <- rpart(bad_air ~ DEWP, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.DEWP <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.DEWP)
mean(PRSA_train$bad_air == PRSA_train$pred.DEWP)

PRSA_model <- rpart(bad_air ~ TEMP, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.TEMP <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.TEMP)
mean(PRSA_train$bad_air == PRSA_train$pred.TEMP)

PRSA_model <- rpart(bad_air ~ DEWP + TEMP, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.DEWP.TEMP <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.DEWP.TEMP)
mean(PRSA_train$bad_air == PRSA_train$pred.DEWP.TEMP)

# Ir와 Is 변수 이용
PRSA_model <- rpart(bad_air ~ Ir, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.Ir <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.Ir)
mean(PRSA_train$bad_air == PRSA_train$pred.Ir)

PRSA_model <- rpart(bad_air ~ Is, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.Is <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.Is)
mean(PRSA_train$bad_air == PRSA_train$pred.Is)

PRSA_model <- rpart(bad_air ~ Ir + Is, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.Ir.Is <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.Ir.Is)
mean(PRSA_train$bad_air == PRSA_train$pred.Ir.Is)

# PRES와 cbwd, Iws를 이용
PRSA_model <- rpart(bad_air ~ PRES, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.PRES <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.PRES)
mean(PRSA_train$bad_air == PRSA_train$pred.PRES)

PRSA_model <- rpart(bad_air ~ cbwd, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.cbwd <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.cbwd)
mean(PRSA_train$bad_air == PRSA_train$pred.cbwd)

PRSA_model <- rpart(bad_air ~ Iws, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
rpart.plot(PRSA_model, type = 3, box.palette = c('red', 'green'), 
           fallen.leaves = TRUE)
PRSA_train$pred.Iws <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.Iws)
mean(PRSA_train$bad_air == PRSA_train$pred.Iws)

PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
PRSA_train$pred.cbwd.Iws.PRES <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.cbwd.Iws.PRES)
mean(PRSA_train$bad_air == PRSA_train$pred.cbwd.Iws.PRES)

# 더 많은 변수 이용
PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP, data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0))
PRSA_train$pred.cbwd.Iws.PRES.TEMP.DEWP <- predict(PRSA_model, PRSA_train, type = 'class')
table(PRSA_train$bad_air, PRSA_train$pred.cbwd.Iws.PRES.TEMP.DEWP)
mean(PRSA_train$bad_air == PRSA_train$pred.cbwd.Iws.PRES.TEMP.DEWP)

PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                    data = PRSA_train, method = 'class', control = rpart.control(cp = 0))
PRSA_train$pred <- predict(PRSA_model, PRSA_train, type = 'class')
conf.table <- 
  table(Actual = PRSA_train$bad_air, Pred = PRSA_train$pred)
mean(PRSA_train$bad_air == PRSA_train$pred)

# 가장 Accuracy가 높은 모델의 Precision, Recall, F1
precision <- conf.table[2, 2] / sum(conf.table[, 2])
recall <- conf.table[2, 2] / sum(conf.table[2, ])
F1 <- (2 * precision * recall) / (precision + recall)

precision
recall
F1

### Q3
PRSA_test$pred <- predict(PRSA_model, PRSA_test, type = 'class')
conf.table_test <- 
  table(Actual = PRSA_test$bad_air, Pred = PRSA_test$pred)
mean(PRSA_test$bad_air == PRSA_test$pred)

precision_test <- conf.table_test[2, 2] / sum(conf.table_test[, 2])
recall_test <- conf.table_test[2, 2] / sum(conf.table_test[2, ])
F1_test <- (2 * precision_test * recall_test) / 
  (precision_test + recall_test)

precision_test
recall_test
F1_test

### Q4
PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                    data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0, maxdepth = 15))
PRSA_train$pred_better <- predict(PRSA_model, PRSA_train, type = 'class')
PRSA_test$pred_better <- predict(PRSA_model, PRSA_test, type = 'class')
mean(PRSA_train$bad_air == PRSA_train$pred_better)
mean(PRSA_test$bad_air == PRSA_test$pred_better)

PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                    data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0, maxdepth = 10))
PRSA_train$pred_better <- predict(PRSA_model, PRSA_train, type = 'class')
PRSA_test$pred_better <- predict(PRSA_model, PRSA_test, type = 'class')
mean(PRSA_train$bad_air == PRSA_train$pred_better)
mean(PRSA_test$bad_air == PRSA_test$pred_better)

PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                    data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0, maxdepth = 5))
PRSA_train$pred_better <- predict(PRSA_model, PRSA_train, type = 'class')
PRSA_test$pred_better <- predict(PRSA_model, PRSA_test, type = 'class')
mean(PRSA_train$bad_air == PRSA_train$pred_better)
mean(PRSA_test$bad_air == PRSA_test$pred_better)

PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                    data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0, maxdepth = 4))
PRSA_train$pred_better <- predict(PRSA_model, PRSA_train, type = 'class')
PRSA_test$pred_better <- predict(PRSA_model, PRSA_test, type = 'class')
mean(PRSA_train$bad_air == PRSA_train$pred_better)
mean(PRSA_test$bad_air == PRSA_test$pred_better)

PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                    data = PRSA_train, method = 'class', 
                    control = rpart.control(cp = 0, maxdepth = 3))
PRSA_train$pred_better <- predict(PRSA_model, PRSA_train, type = 'class')
PRSA_test$pred_better <- predict(PRSA_model, PRSA_test, type = 'class')
mean(PRSA_train$bad_air == PRSA_train$pred_better)
mean(PRSA_test$bad_air == PRSA_test$pred_better)

### Q5
PRSA_model <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                    data = PRSA_train, method = 'class', control = rpart.control(cp = 0))
printcp(PRSA_model)
plotcp(PRSA_model)
ptree <- prune(PRSA_model, 
               cp = PRSA_model$cptable[which.min(PRSA_model$cptable[, "xerror"]), "CP"])
PRSA_test$better_pred <- predict(ptree, PRSA_test, type = 'class')
PRSA_train$better_pred <- predict(ptree, PRSA_train, type = 'class')
mean(PRSA_test$bad_air == PRSA_test$better_pred)
mean(PRSA_train$bad_air == PRSA_train$better_pred)

### Q6
conf.table <- 
  table(Actual = PRSA_test$bad_air, Pred = PRSA_test$better_pred)

precision <- conf.table[2, 2] / sum(conf.table[, 2])
recall <- conf.table[2, 2] / sum(conf.table[2, ])
precision
recall

### Q7
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}

Model_A <- rpart(bad_air ~ cbwd + Iws + PRES + TEMP + DEWP + Ir + Is, 
                 data = PRSA_train, method = 'class', control = rpart.control(cp = 0))
PRSA_train$pred <- predict(Model_A, PRSA_train, type = 'prob')[, 2]
PRSA_test$pred <- predict(Model_A, PRSA_test, type = 'prob')[, 2]

plot(performance(prediction(PRSA_train$pred, PRSA_train$bad_air), 'tpr', 'fpr'), main = "Train data of Model A")
plot(performance(prediction(PRSA_test$pred, PRSA_test$bad_air), 'tpr', 'fpr'), main = "Test data of Model A")
calAUC(PRSA_train$pred, PRSA_train$bad_air)
calAUC(PRSA_test$pred, PRSA_test$bad_air)

Model_B <- ptree
PRSA_train$pred2 <- predict(ptree, PRSA_train, type = 'prob')[, 2]
PRSA_test$pred2 <- predict(ptree, PRSA_test, type = 'prob')[, 2]

plot(performance(prediction(PRSA_train$pred2, PRSA_train$bad_air), 'tpr', 'fpr'), main = "Train data of Model B")
plot(performance(prediction(PRSA_test$pred2, PRSA_test$bad_air), 'tpr', 'fpr'), main = "Test data of Model B")
calAUC(PRSA_train$pred2, PRSA_train$bad_air)
calAUC(PRSA_test$pred2, PRSA_test$bad_air)

### Q8
PRSA <- read.csv("PRSA_data.csv")
PRSA <- PRSA[-1]
PRSA <- na.omit(PRSA)

PRSA$bad_air <- ifelse(PRSA$pm2.5 > 75, TRUE, FALSE)
PRSA_dummy <- predict(dummyVars(~ cbwd, data = PRSA), PRSA)
PRSA <- cbind(PRSA, PRSA_dummy)

PRSA_train <- subset(PRSA, year < 2014)
PRSA_train_dummy <- PRSA_train[, 14:17]
PRSA_test <- subset(PRSA, year == 2014)
PRSA_test_dummy <- PRSA_test[, 14:17]

minmax_norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
z_score <- function(x){
  (x - mean(x)) / var(x)
}

PRSA_train_minmax <- as.data.frame(lapply(PRSA_train[, c(6:8, 10:12)], minmax_norm))
PRSA_train_minmax <- cbind(PRSA_train_minmax, PRSA_train_dummy)
summary(PRSA_train_minmax)
PRSA_test_minmax <- as.data.frame(lapply(PRSA_test[, c(6:8, 10:12)], minmax_norm))
PRSA_test_minmax <- cbind(PRSA_test_minmax, PRSA_test_dummy)
summary(PRSA_test_minmax)

PRSA_train_z <- as.data.frame(lapply(PRSA_train[, c(6:8, 10:12)], z_score))
PRSA_train_z <- cbind(PRSA_train_z, PRSA_train_dummy)
summary(PRSA_train_z)
PRSA_test_z <- as.data.frame(lapply(PRSA_test[, c(6:8, 10:12)], z_score))
PRSA_test_z <- cbind(PRSA_test_z, PRSA_test_dummy)
summary(PRSA_test_z)

### Q9
sqrt(nrow(PRSA_train_minmax))
PRSA_test_minmax_pred <- knn(train = PRSA_train_minmax, test = PRSA_test_minmax, 
                             cl = PRSA_train$bad_air, k = 181)
mean(PRSA_test$bad_air == PRSA_test_minmax_pred)

PRSA_test_z_pred <- knn(train = PRSA_train_z, test = PRSA_test_z, 
                        cl = PRSA_train$bad_air, k = 181)
mean(PRSA_test$bad_air == PRSA_test_z_pred)

accuracy_knn <- mean(PRSA_test$bad_air == PRSA_test_z_pred)
cmat <- table(Actual = PRSA_test$bad_air, Pred = PRSA_test_z_pred)
precision_knn <- cmat[2, 2] / sum(cmat[, 2])
recall_knn <- cmat[2, 2] / sum(cmat[2, ])
F1_knn <- (2 * precision_knn * recall_knn) / (precision_knn + recall_knn)

accuracy_knn
precision_knn
recall_knn
F1_knn

### Q10
test_pred <- 0
k_pred <- as.data.frame(1:8661)
accuracy <- 0
precision <- 0
recall <- 0
F1 <- 0
AUC_by_k <- 0
k_data <- seq(10, 450, 20)

for(i in (1:23)){
  test_pred <- knn(train = PRSA_train_z, test = PRSA_test_z, 
                   cl = PRSA_train$bad_air, k = k_data[i])
  k_pred[i] <- test_pred 
  cmat <- table(Actual = PRSA_test$bad_air, Pred = test_pred)
  accuracy[i] <- mean(PRSA_test$bad_air == test_pred)
  precision[i] <- cmat[2, 2] / sum(cmat[, 2])
  recall[i] <- cmat[2, 2] / sum(cmat[2, ])
  F1[i] <- (2 * precision[i] * recall[i]) / (precision[i] + recall[i])
}

AUC <- as.data.frame(cbind(k_pred[1:23]))
colnames(AUC)[1] <- "V1"
AUC <- ifelse(AUC == TRUE, 1, 0)
AUC <- as.data.frame(AUC)
PRSA_test$bad_air <- ifelse(PRSA_test$bad_air == TRUE, 1, 0)

for(i in (1:23)){
  AUC_by_k[i] <- calAUC(AUC[, i], PRSA_test$bad_air)
}

k_df1 <- as.data.frame(cbind(k = k_data, accuracy = accuracy[1:23], precision = precision[1:23], 
                            recall = recall[1:23], F1 = F1[1:23], AUC = AUC_by_k))
k_df1[order(k_df1$accuracy, decreasing = T), ]

par(mfrow = c(1, 1))
ggplot(k_df1, aes(x = k, y = accuracy)) + geom_point() + geom_line() + 
  ggtitle("variation of 'Accuracy' by the value of 'k'") + 
  theme(plot.title = element_text(hjust = 0.5, size = 18, color = "darkblue"))
plot(x = k_df1$k, y = k_df1$precision, main = "precision")
plot(x = k_df1$k, y = k_df1$F1, main = "F1")
plot(x = k_df1$k, y = k_df1$AUC, main = "AUC")

### Q11
k_df1[order(k_df1$recall, decreasing = T), ]
plot(x = k_df1$k, y = k_df1$recall, main = "recall")

test_pred <- 0
accuracy <- 0
precision <- 0
recall <- 0
F1 <- 0
k_data <- 480:499
PRSA_test$bad_air <- ifelse(PRSA_test$bad_air == 1, TRUE, FALSE)

for(i in (1:20)){
  test_pred <- knn(train = PRSA_train_z, test = PRSA_test_z, 
                   cl = PRSA_train$bad_air, k = k_data[i])
  cmat <- table(Actual = PRSA_test$bad_air, Pred = test_pred)
  accuracy[i] <- mean(PRSA_test$bad_air == test_pred)
  precision[i] <- cmat[2, 2] / sum(cmat[, 2])
  recall[i] <- cmat[2, 2] / sum(cmat[2, ])
  F1[i] <- (2 * precision[i] * recall[i]) / (precision[i] + recall[i])
}

k_df <- as.data.frame(cbind(k = k_data, accuracy = accuracy[1:20], precision = precision[1:20], 
                            recall = recall[1:20], F1 = F1[1:20]))
k_df <- rbind(k_df, k_df1[, 1:5])
k_df[order(k_df$recall, decreasing = T), ]

PRSA_test_pred <- knn(train = PRSA_train_z, test = PRSA_test_z, cl = PRSA_train$bad_air, 
                      k = 496, prob = TRUE)
head(PRSA_test_pred)
head(attributes(PRSA_test_pred)$prob)

PRSA_test_pred_prob <- ifelse(PRSA_test_pred == TRUE, 
                              attributes(PRSA_test_pred)$prob, 
                              1 - attributes(PRSA_test_pred)$prob)
plot(performance(prediction(PRSA_test_pred_prob, PRSA_test$bad_air == TRUE), 'tpr', 'fpr'))
calAUC(PRSA_test_pred_prob, PRSA_test$bad_air == TRUE)

threshold <- 0.3
PRSA_test_pred_new <- ifelse(PRSA_test_pred_prob > threshold, TRUE, FALSE)
cmat <- table(Actual = PRSA_test$bad_air, Pred = PRSA_test_pred_new)
new_precision <- cmat[2, 2] / sum(cmat[, 2])
new_recall <- cmat[2, 2] / sum(cmat[2, ])
new_precision
new_recall

threshold <- 0.2
PRSA_test_pred_new <- ifelse(PRSA_test_pred_prob > threshold, TRUE, FALSE)
cmat <- table(Actual = PRSA_test$bad_air, Pred = PRSA_test_pred_new)
new_precision <- cmat[2, 2] / sum(cmat[, 2])
new_recall <- cmat[2, 2] / sum(cmat[2, ])
new_precision
new_recall

threshold <- 0.1
PRSA_test_pred_new <- ifelse(PRSA_test_pred_prob > threshold, TRUE, FALSE)
cmat <- table(Actual = PRSA_test$bad_air, Pred = PRSA_test_pred_new)
new_precision <- cmat[2, 2] / sum(cmat[, 2])
new_recall <- cmat[2, 2] / sum(cmat[2, ])
new_precision
new_recall
cmat
