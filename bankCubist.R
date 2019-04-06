# Importing the dataset
bankCSV = read.csv('bank-additional-full.csv')
# dataset = dataset[4:14]





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
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123456)
split = sample.split(bankDF$y, SplitRatio = 0.8)
bankDF_train = subset(bankDF, split ==TRUE)
bankDF_test = subset(bankDF, split ==FALSE)


train_bdata <- createDataPartition(y=bankDF$y,p=0.75,list = FALSE)
trainpred <- bankDF[train_bdata,-21]
testpred <- bankDF[-train_bdata,-21]
trainresp <- bankDF$y[train_bdata]
testresp <- bankDF$y[-train_bdata]

library(Cubist)
library(mlbench)
Model1 <- cubist(x = trainpred, y= trainresp)
summary(Model1)
Model1
Pred1 <- predict(Model1,testpred)
Pred1
summary(Pred1)
sqrt(2)
sqrt(mean((Pred1-testresp)^2))
cor(Pred1,testpred)^2
cor(Pred1,testresp)^2