load('hw5_student.RData')
library(dplyr)
library(ROCR)
library(MASS)

##### PART 1
### Q1
sum(rowSums(is.na(student.train)))
model1 <- lm(G3 ~., data = student.train)
summary(model1) # R2 = 0.2954

summary(student.train)
sapply(student.train, class)
#student.train[, c(7:8, 13:15, 24:29)] <- sapply(student.train[, c(7:8, 13:15, 24:29)], as.factor)
student.train$failures <- as.factor(student.train$failures)
student.train$famrel <- as.factor(student.train$famrel)
student.train$goout <- as.factor(student.train$goout)
student.train$Dalc <- as.factor(student.train$Dalc)
student.train$Walc <- as.factor(student.train$Walc)
student.train$health <- as.factor(student.train$health)
sapply(student.train, class)

model2 <- lm(G3 ~., data = student.train)
summary(model2) # R2 = 0.3329

### Q2
calcRMSE <- function(label, estimation){
  return(sqrt(mean(label - estimation) ** 2))
}
calcR2 <- function(label, estimation){
  RSS = sum((label - estimation) ** 2)
  SStot = sum((label - mean(label)) ** 2)
  return (1-RSS/SStot)
}

student.train$pred <- predict(model2, newdata = student.train)
calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)
student.train <- student.train[-33]

### Q3
summary(model2)

student.test.nolabel$failures <- as.factor(student.test.nolabel$failures)
student.test.nolabel$famrel <- as.factor(student.test.nolabel$famrel)
student.test.nolabel$goout <- as.factor(student.test.nolabel$goout)
student.test.nolabel$Dalc <- as.factor(student.test.nolabel$Dalc)
student.test.nolabel$Walc <- as.factor(student.test.nolabel$Walc)
student.test.nolabel$health <- as.factor(student.test.nolabel$health)

student.test.nolabel$pred <- predict(model2, newdata = student.test.nolabel)
pred_grade_test <- student.test.nolabel$pred
save(pred_grade_test, file = 'st21500268.Rdata')
student.test.nolabel <- student.test.nolabel[-32]

### Q4
mean(student.train$G3)
table(student.train$G3)
summary(student.train %>% filter(G3 >= 14))

train <- student.train
train$school <- ifelse(train$school == 'GP', 1, 0)
train$address <- ifelse(train$address == 'U', 1, 0)
train$famsize <- ifelse(train$famsize == 'GT3', 1, 0)
train$Pstatus <- ifelse(train$Pstatus == 'T', 1, 0)
train$failures <- ifelse(train$failures == 0, 1, 0)
train$traveltime <- ifelse(as.numeric(train$traveltime) <= 2, 1, 0)
train$schoolsup <- ifelse(train$schoolsup == 'no', 1, 0)
train$paid <- ifelse(train$paid == 'no', 1, 0)
train$nursery <- ifelse(train$nursery == 'yes', 1, 0)
train$higher <- ifelse(train$higher == 'yes', 1, 0)
train$internet <- ifelse(train$internet == 'yes', 1, 0)
train$romantic <- ifelse(train$romantic == 'no', 1, 0)
train$famrel <- ifelse(as.numeric(train$famrel) >= 4, 1, 0)
train$Dalc <- ifelse(as.numeric(train$Dalc) <= 2, 1, 0)
train$class <- ifelse(train$class == 'port', 1, 0)

attach(train)
train <- as.data.frame(cbind(school, address, famsize, Pstatus, failures, traveltime, schoolsup, paid, 
                             nursery, higher, internet, romantic, famrel, Dalc, class))
train$environment <- rowSums(train)
table(train$environment)
detach(train)

student.train <- cbind(student.train, train$environment)
colnames(student.train)[33] <- 'environment'
table(student.train$environment)

model3 <- lm(G3 ~., data = student.train)
summary(model3) # R2 = 0.3532

student.train$environment <- as.factor(student.train$environment)
model3_1 <- lm(G3 ~., data = student.train)
summary(model3_1) # R2 = 0.3648

student.train$pred <- predict(model3_1, newdata = student.train)
calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)
student.train <- student.train[-34]
student.train <- student.train[-33]

## Re-try
for(i in 1:nrow(student.train)){
  student.train$var[i] <- paste0(student.train[i, c('school', 'famsize', 'studytime', 'failures', 
                                                    'schoolsup', 'goout', 'health', 'class')], collapse = "")
}
model4 <- lm(G3~., data = student.train)
summary(model4) # R2 = 0.7896

student.train$pred <- predict(model4, newdata = student.train)
calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)
student.train <- student.train[-34]
student.train <- student.train[-33]

## Re-try (Final)
student.train$age_group <- ifelse(student.train$age <= 17, 'younger', 'older')
model5 <- lm(G3~., data = student.train)
summary(model5)

student.train$pred <- predict(model5, newdata = student.train)
calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)
student.train <- student.train[-34]

### Q5
step(model5, direction = "both")
summary(lm(formula = G3 ~ school + famsize + Medu + Mjob + Fjob + guardian + 
             studytime + failures + schoolsup + famsup + higher + internet + 
             romantic + goout + health + absences + class, data = student.train))

model6 <- lm(formula = G3 ~. + I(absences^2) + I(age^2) + I(traveltime^2) + I(studytime^2) + 
               I(Medu^2)+ I(Fedu^2)+ school*reason*nursery*schoolsup*studytime +
               freetime*romantic*absences*internet + Medu*Mjob + absences*failures*higher + 
               studytime*higher*class + studytime*failures*higher + Medu*higher*paid + address*traveltime +
               failures*higher*guardian*Pstatus + goout*activities + goout*health + schoolsup*famsup*famsize
               , data = student.train)

student.train$pred <- predict(model6, newdata = student.train)
calcRMSE(student.train$G3, student.train$pred)
calcR2(student.train$G3, student.train$pred)

### Q6
load('hw5_student.RData')
student.train$failures <- as.factor(student.train$failures)
student.train$famrel <- as.factor(student.train$famrel)
student.train$goout <- as.factor(student.train$goout)
student.train$Dalc <- as.factor(student.train$Dalc)
student.train$Walc <- as.factor(student.train$Walc)
student.train$health <- as.factor(student.train$health)
student.train$age_group <- ifelse(student.train$age <= 17, 'younger', 'older')

student.test.nolabel$failures <- as.factor(student.test.nolabel$failures)
student.test.nolabel$famrel <- as.factor(student.test.nolabel$famrel)
student.test.nolabel$goout <- as.factor(student.test.nolabel$goout)
student.test.nolabel$Dalc <- as.factor(student.test.nolabel$Dalc)
student.test.nolabel$Walc <- as.factor(student.test.nolabel$Walc)
student.test.nolabel$health <- as.factor(student.test.nolabel$health)
student.test.nolabel$age_group <- ifelse(student.test.nolabel$age <= 17, 'younger', 'older')

student.test.nolabel$pred <- predict(model6, newdata = student.test.nolabel)
pred_grade_test <- student.test.nolabel$pred
save(pred_grade_test, file = 'st21500268.Rdata')




#### PART 2
### Q1
sum(rowSums(is.na(credit_train)))
sapply(credit_train, class)

credit_train$default.payment.next.month <- as.factor(credit_train$default.payment.next.month)
credit_train$SEX <- as.factor(credit_train$SEX)
credit_train$EDUCATION <- as.factor(credit_train$EDUCATION)
credit_train$MARRIAGE <- as.factor(credit_train$MARRIAGE)
summary(credit_train)

model8 <- glm(default.payment.next.month ~., data = credit_train, family = binomial(link = 'logit'))
summary(model8)
credit_train$pred <- predict(model8, credit_train, type = 'response')
credit_train$pred_prob <- ifelse(credit_train$pred >= 0.5, 1, 0)

### Q2
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}

calAUC(as.numeric(credit_train$default.payment.next.month), credit_train$pred_prob)

credit_test$default.payment.next.month <- as.factor(credit_test$default.payment.next.month)
credit_test$SEX <- as.factor(credit_test$SEX)
credit_test$EDUCATION <- as.factor(credit_test$EDUCATION)
credit_test$MARRIAGE <- as.factor(credit_test$MARRIAGE)

credit_test$pred <- predict(model8, credit_test, type = 'response')
credit_test$pred_prob <- ifelse(credit_test$pred >= 0.5, 1, 0)
prob_default_test <- credit_test$pred
save(pred_grade_test, prob_default_test, file = 'st21500268.Rdata')

### Q3
calAccuary <- function(predCol, targetCol){
  mean(predCol == targetCol, na.rm = T)
}
calPrecision <- function(predCol, targetCol){
  conf <- table(predCol, targetCol)
  return(conf[2, 2] / sum(conf[, 2]))
}
calRecall <- function(predCol, targetCol){
  conf <- table(predCol, targetCol)
  return(conf[2, 2] / sum(conf[2, ]))
}

calAccuary(credit_train$default.payment.next.month, credit_train$pred_prob) # Acc = 0.8118
calPrecision(credit_train$default.payment.next.month, credit_train$pred_prob) # Precision = 0.7173
calRecall(credit_train$default.payment.next.month, credit_train$pred_prob) # Recall = 0.2487

credit_train$pred_precision_prob <- ifelse(credit_train$pred >= 0.4, 1, 0)
calAccuary(credit_train$default.payment.next.month, credit_train$pred_precision_prob) # Acc = 0.81748
calPrecision(credit_train$default.payment.next.month, credit_train$pred_precision_prob) # Precision = 0.6476277
calRecall(credit_train$default.payment.next.month, credit_train$pred_precision_prob) # Recall = 0.3868231

credit_train$pred_precision_prob1 <- ifelse(credit_train$pred >= 0.3, 1, 0)
calAccuary(credit_train$default.payment.next.month, credit_train$pred_precision_prob1) # Acc = 0.8048
calPrecision(credit_train$default.payment.next.month, credit_train$pred_precision_prob1) # Precision = 0.5726872
calRecall(credit_train$default.payment.next.month, credit_train$pred_precision_prob1) # Recall = 0.4693141

credit_train$pred_precision_prob2 <- ifelse(credit_train$pred >= 0.2, 1, 0)
calAccuary(credit_train$default.payment.next.month, credit_train$pred_precision_prob2) # Acc = 0.61844
calPrecision(credit_train$default.payment.next.month, credit_train$pred_precision_prob2) # Precision = 0.331052
calRecall(credit_train$default.payment.next.month, credit_train$pred_precision_prob2) # Recall = 0.7072202

credit_train <- credit_train[-25:-29]
credit_test <- credit_test[-24:-25]

new_credit_train <- credit_train
new_credit_test <- credit_test

### Q4
attach(credit_train)
credit_train$rate_month1 <- PAY_AMT5 / BILL_AMT6
credit_train$rate_month2 <- PAY_AMT4 / BILL_AMT5
credit_train$rate_month3 <- PAY_AMT3 / BILL_AMT4
credit_train$rate_month4 <- PAY_AMT2 / BILL_AMT3
credit_train$rate_month5 <- PAY_AMT1 / BILL_AMT2
detach(credit_train)

sum(is.na(credit_train$rate_month1))
credit_train$rate_month1 <- ifelse(is.na(credit_train$rate_month1), 0, credit_train$rate_month1)
credit_train$rate_month2 <- ifelse(is.na(credit_train$rate_month2), 0, credit_train$rate_month2)
credit_train$rate_month3 <- ifelse(is.na(credit_train$rate_month3), 0, credit_train$rate_month3)
credit_train$rate_month4 <- ifelse(is.na(credit_train$rate_month4), 0, credit_train$rate_month4)
credit_train$rate_month5 <- ifelse(is.na(credit_train$rate_month5), 0, credit_train$rate_month5)

table(credit_train$rate_month1 == Inf)
credit_train$rate_month1 <- ifelse(credit_train$rate_month1 == Inf, 0, credit_train$rate_month1)
credit_train$rate_month2 <- ifelse(credit_train$rate_month2 == Inf, 0, credit_train$rate_month2)
credit_train$rate_month3 <- ifelse(credit_train$rate_month3 == Inf, 0, credit_train$rate_month3)
credit_train$rate_month4 <- ifelse(credit_train$rate_month4 == Inf, 0, credit_train$rate_month4)
credit_train$rate_month5 <- ifelse(credit_train$rate_month5 == Inf, 0, credit_train$rate_month5)

model9 <- glm(default.payment.next.month ~., data = credit_train, family = binomial(link = 'logit'))
summary(model9)

credit_train$pred <- predict(model9, credit_train, type = 'response')
credit_train$pred_prob <- ifelse(credit_train$pred >= 0.5, 1, 0)

calAUC(as.numeric(credit_train$default.payment.next.month), credit_train$pred_prob)


credit_test$default.payment.next.month <- as.factor(credit_test$default.payment.next.month)
credit_test$SEX <- as.factor(credit_test$SEX)
credit_test$EDUCATION <- as.factor(credit_test$EDUCATION)
credit_test$MARRIAGE <- as.factor(credit_test$MARRIAGE)

attach(credit_test)
credit_test$rate_month1 <- PAY_AMT5 / BILL_AMT6
credit_test$rate_month2 <- PAY_AMT4 / BILL_AMT5
credit_test$rate_month3 <- PAY_AMT3 / BILL_AMT4
credit_test$rate_month4 <- PAY_AMT2 / BILL_AMT3
credit_test$rate_month5 <- PAY_AMT1 / BILL_AMT2
detach(credit_test)

credit_test$rate_month1 <- ifelse(is.na(credit_test$rate_month1), 0, credit_test$rate_month1)
credit_test$rate_month2 <- ifelse(is.na(credit_test$rate_month2), 0, credit_test$rate_month2)
credit_test$rate_month3 <- ifelse(is.na(credit_test$rate_month3), 0, credit_test$rate_month3)
credit_test$rate_month4 <- ifelse(is.na(credit_test$rate_month4), 0, credit_test$rate_month4)
credit_test$rate_month5 <- ifelse(is.na(credit_test$rate_month5), 0, credit_test$rate_month5)

credit_test$rate_month1 <- ifelse(credit_test$rate_month1 == Inf, 0, credit_test$rate_month1)
credit_test$rate_month2 <- ifelse(credit_test$rate_month2 == Inf, 0, credit_test$rate_month2)
credit_test$rate_month3 <- ifelse(credit_test$rate_month3 == Inf, 0, credit_test$rate_month3)
credit_test$rate_month4 <- ifelse(credit_test$rate_month4 == Inf, 0, credit_test$rate_month4)
credit_test$rate_month5 <- ifelse(credit_test$rate_month5 == Inf, 0, credit_test$rate_month5)

credit_test$pred <- predict(model9, credit_test, type = 'response')
credit_test$pred_prob <- ifelse(credit_test$pred >= 0.5, 1, 0)
prob_default_test <- credit_test$pred
save(pred_grade_test, prob_default_test, file = 'st21500268.Rdata')

credit_train <- credit_train[-25:-31]
credit_test <- credit_test[-24:-30]


credit_train$LIMIT_BAL <- (credit_train$LIMIT_BAL - min(credit_train$LIMIT_BAL)) / 
  (max(credit_train$LIMIT_BAL) - min(credit_train$LIMIT_BAL))

model10 <- glm(default.payment.next.month ~., data = credit_train, family = binomial(link = 'logit'))
summary(model10)

credit_train$pred <- predict(model10, credit_train, type = 'response')
credit_train$pred_prob <- ifelse(credit_train$pred >= 0.5, 1, 0)

calAUC(as.numeric(credit_train$default.payment.next.month), credit_train$pred_prob)

credit_test$LIMIT_BAL <- (credit_test$LIMIT_BAL - min(credit_test$LIMIT_BAL)) / 
  (max(credit_test$LIMIT_BAL) - min(credit_test$LIMIT_BAL))

credit_test$pred <- predict(model10, credit_test, type = 'response')
credit_test$pred_prob <- ifelse(credit_test$pred >= 0.5, 1, 0)
prob_default_test <- credit_test$pred
save(pred_grade_test, prob_default_test, file = 'st21500268.Rdata')

credit_train <- credit_train[-25:-26]
credit_test <- credit_test[-24:-25]

str(new_credit_train)
#LIMIT_BAL + SEX + 
#EDUCATION + MARRIAGE + AGE + PAY_1 + PAY_2 + PAY_3 + PAY_4 + 
  #BILL_AMT1 + BILL_AMT3 + PAY_AMT1 + PAY_AMT2 + PAY_AMT4 + 
  #PAY_AMT5 + PAY_1*PAY_AMT1 + PAY_2*PAY_AMT2 + + PAY_3*PAY_AMT3 + 
  #PAY_4*PAY_AMT4 + PAY_5*PAY_AMT5 + PAY_6*PAY_AMT6 + I(AGE^2) + 
  #I(PAY_AMT1^2) + I(PAY_AMT2^2) + I(PAY_AMT3^2) + I(PAY_AMT4^2) +
  #I(PAY_AMT5^2) + I(PAY_AMT6^2)

str(credit_train)
model11 <- glm(formula = default.payment.next.month ~ . +
                 LIMIT_BAL + SEX + 
                 EDUCATION + MARRIAGE + AGE + PAY_1 + PAY_2 + PAY_3 + PAY_4 + 
                 BILL_AMT1 + BILL_AMT3 + PAY_AMT1 + PAY_AMT2 + PAY_AMT4 + 
                 PAY_AMT5 + PAY_1*PAY_AMT1 + PAY_2*PAY_AMT2 + + PAY_3*PAY_AMT3 + 
                 PAY_4*PAY_AMT4 + PAY_5*PAY_AMT5 + PAY_6*PAY_AMT6 + I(AGE^2) + 
                 I(PAY_AMT1^2) + I(PAY_AMT2^2) + I(PAY_AMT3^2) + I(PAY_AMT4^2) +
                 I(PAY_AMT5^2) + I(PAY_AMT6^2), 
                 family = binomial(link = "logit"), data = new_credit_train)

new_credit_train$pred <- predict(model11, new_credit_train, type = 'response')
new_credit_train$pred_prob <- ifelse(new_credit_train$pred >= 0.5, 1, 0)
calAUC(as.numeric(new_credit_train$default.payment.next.month), new_credit_train$pred_prob)

new_credit_test$pred <- predict(model11, new_credit_test, type = 'response')
new_credit_test$pred_prob <- ifelse(new_credit_test$pred >= 0.5, 1, 0)
prob_default_test <- new_credit_test$pred
save(pred_grade_test, prob_default_test, file = 'st21500268.Rdata')

### Q5
new_credit_test$pred_prob <- ifelse(new_credit_test$pred >= 0.3, TRUE, FALSE)
pred_default_test <- new_credit_test$pred_prob
save(pred_grade_test, prob_default_test, pred_default_test, file = 'st21500268.Rdata')

new_credit_test$pred_prob <- ifelse(new_credit_test$pred >= 0.4, TRUE, FALSE)
pred_default_test <- new_credit_test$pred_prob
save(pred_grade_test, prob_default_test, pred_default_test, file = 'st21500268.Rdata')

new_credit_test$pred_prob <- ifelse(new_credit_test$pred >= 0.5, TRUE, FALSE)
pred_default_test <- new_credit_test$pred_prob
save(pred_grade_test, prob_default_test, pred_default_test, file = 'st21500268.Rdata')

new_credit_test$pred_prob <- ifelse(new_credit_test$pred >= 0.6, TRUE, FALSE)
pred_default_test <- new_credit_test$pred_prob
save(pred_grade_test, prob_default_test, pred_default_test, file = 'st21500268.Rdata')
