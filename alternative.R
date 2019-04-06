bankCSV <- read.csv('bank-additional-full.csv')
str(bankCSV)
summary(bankCSV)

# Encoding the categorical variables as factors
bankCSV$job = as.numeric(factor(bankCSV$job,
                                      levels = c('admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown'),
                                      labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))

bankCSV$marital = as.numeric(factor(bankCSV$marital,
                                levels = c('divorced','married','single','unknown'),
                                labels = c(1, 2, 3, 4)))

bankCSV$education = as.numeric(factor(bankCSV$education,
                                    levels = c('basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown'),
                                    labels = c(1, 2, 3, 4, 5, 6, 7, 8)))

bankCSV$default = as.numeric(factor(bankCSV$default,
                                      levels = c('no','yes','unknown'),
                                      labels = c(1, 2, 3)))

bankCSV$housing = as.numeric(factor(bankCSV$housing,
                                    levels = c('no','yes','unknown'),
                                    labels = c(1, 2, 3)))

bankCSV$loan = as.numeric(factor(bankCSV$loan,
                                 levels = c('no','yes','unknown'),
                                 labels = c(1, 2, 3)))

bankCSV$contact = as.numeric(factor(bankCSV$contact,
                                 levels = c('cellular','telephone'),
                                 labels = c(1, 2)))

bankCSV$month = as.numeric(factor(bankCSV$month,
                                levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'),
                                labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))

bankCSV$day_of_week = as.numeric(factor(bankCSV$day_of_week,
                                  levels = c('mon','tue','wed','thu','fri'),
                                  labels = c(1, 2, 3, 4, 5)))

bankCSV$poutcome = as.numeric(factor(bankCSV$poutcome,
                                        levels = c('failure','nonexistent','success'),
                                        labels = c(1, 2, 3)))

# bankCSV$y = as.numeric(factor(bankCSV$y,
#                                      levels = c('yes','no'),
#                                      labels = c(1, 2)))

bankDF <- bankCSV[order(runif(nrow(bankCSV))),]
tail(bankDF,47)

set.seed(123456)
split = sample.split(bankDF$y, SplitRatio = 0.8)
bankDF_train = subset(bankDF, split ==TRUE)
bankDF_test = subset(bankDF, split ==FALSE)

# bankDF_train <- bankDF[1:41000,]
# bankDF_test <- bankDF[41001:41188,]

# nrow(bankDF_train)
# nrow(bankDF_test)

require(C50)

#C5.0 Train 

# C50_model <- C5.0(bankDF_train[,-21],bankDF_train[,21])

C50_model <- C5.0(bankDF_train[,-21],bankDF_train[,21])
summary(C50_model)

plot(C50_model)


#C5.0 Predict

C50_predict <- predict(C50_model,bankDF_test)
C50_predict

#Comfusion Matrix

library(caret)
confusion <- confusionMatrix(C50_predict,bankDF_test[,21])
confusion


#C5.0 Train improve performance

C50_model <- C5.0(bankDF_train[,-21],bankDF_train[,21],trials=100)
summary(C50_model)

#C5.0 Predict Class
C50_predict_class <-predict(C50_model,bankDF_test, method = "class")
# C50_predict_class

#Confusion Matrix

confusion_improved <- confusionMatrix(C50_predict_class, bankDF_test[,21])
confusion_improved