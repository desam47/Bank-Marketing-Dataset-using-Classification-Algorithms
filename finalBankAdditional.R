#Required libraries

library(ggplot2)
library(ggthemes)
library(DMwR)
library(caret)
library(randomForest)
library(xgboost)
library(VIM)
library(C50)
library(car)
library(caTools)

#Importing the dataset
dataset <- read.csv('bank-additional-full.csv')

#Initial look at the data
names(dataset)
summary(dataset)
str(dataset)

#DATA CLEANING
#Checking for the missing values
sum(is.na(dataset)) 
sapply(dataset, FUN=function(x) sum(is.na(x)))  

#dataset - no missing value found

#Generating plots to visually get some insights about the variables

#Theme Setting
theme_set(theme_fivethirtyeight())
barfill <- "#4271AE"
barlines <- "#1F3552"    

#Missing Vales Plot
aggr_plot <- aggr(dataset, col=c('slateblue','red'), numbers=TRUE, prop=FALSE,
                  sortVars=TRUE, labels=names(dataset), cex.axis=0.7, gap=1,
                  varheight = FALSE,combined = FALSE,cex.numbers =0.5, 
                  ylab=c("Histogram of missing data","Pattern"))

#Age Variable
p1 <- ggplot(dataset, aes(x = age)) +
  geom_histogram(aes(fill = ..count..), binwidth = 1,
                 colour = barlines, fill = barfill)+
  scale_x_continuous(name = "Age",
                     breaks = seq(15,100,10),
                     limits = c(15,100)) +
  xlab("Age")+ylab("Count")+
  ggtitle("Frequency histogram of Age") +
  theme(plot.title = element_text(hjust = 0.5))
p1

p2 <- ggplot(dataset, aes(x = y, y =age)) +
  geom_boxplot(fill = "coral2", colour="firebrick4")+
  scale_y_continuous(name = "Age") +
  scale_x_discrete(name="Subscribed") +
  ggtitle("Boxplot of Age by Subscription") +
  theme(plot.title = element_text(hjust = 0.5))

p2

#Marital Vairable
p3 <- ggplot() + geom_bar(aes(y = (..count..), x = marital, fill = y), data = dataset,
                          stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Marital Status Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p3

#Job Vairable
p4 <- ggplot() + geom_bar(aes(y = (..count..), x = job, fill = y), data = dataset,
                          stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Job Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1))
p4

#Education Vairable
p5 <- ggplot() + geom_bar(aes(y = (..count..), x = education, fill = y), data = dataset,
                          stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Education Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1))
p5

#Contact Variable
p6 <- ggplot() + geom_bar(aes(y = (..count..), x = contact, fill = y), data = dataset,
                          stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Contact Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p6

#Default Variable
p7 <- ggplot() + geom_bar(aes(y = (..count..), x = default, fill = y), data = dataset,
                          stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Default Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p7

#Housing Variable
p8 <- ggplot() + geom_bar(aes(y = (..count..), x = housing, fill = y), data = dataset,
                          stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Housing Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p8

#Loan Variable
p9 <- ggplot() + geom_bar(aes(y = (..count..), x = loan, fill = y), data = dataset,
                          stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Loan Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p9

#Campaign
p11 <- ggplot() + geom_histogram(aes(y = (..count..), x = campaign, fill = y ), data = dataset,
                                 binwidth=5, stat="count") +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  scale_x_continuous(breaks = seq(1,15,1),limits=c(0,15))+
  ggtitle("No.of Contacts Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))

p11

#Month Vairable
p12 <- ggplot() + geom_bar(aes(y = (..count..), x = month, fill = y), data = dataset,
                           stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Month Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p12


#Days of Week Vairable
p13 <- ggplot() + geom_bar(aes(y = (..count..), x = day_of_week, fill = y), data = dataset,
                           stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Day Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p13


#Duration Vairable
p14 <- ggplot() + geom_bar(aes(y = (..count..), x = duration, fill = y), data = dataset,
                           stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Duration Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p14

#ln_duration <- log(dataset$duration)
ln_duration <- log(1+(dataset$duration))
p24 <- ggplot() + geom_histogram(aes(x = ln_duration, fill = y), data = dataset,
                                 binwidth = 0.1 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Log Duration Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p24

#pdays Vairable
p16 <- ggplot() + geom_histogram(aes( x = pdays, fill = y), data = dataset,
                                 binwidth = 50 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("PCampaign Days Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p16
# table(dataset$pdays)


#Previous
p17 <- ggplot() + geom_histogram(aes( x = previous, fill = y), data = dataset,
                                 binwidth = 1 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("PContact Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p17

#POutcome
p18 <- ggplot() + geom_bar(aes(y = (..count..), x = poutcome, fill = y), data = dataset,
                           stat="count" ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("POutcome Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p18

#Employment variation Rate
p19 <- ggplot() + geom_histogram(aes( x = emp.var.rate, fill = y), data = dataset,
                                 binwidth = 1 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Emp.Var.Rate Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p19

#Consumer Price Index
p20 <- ggplot() + geom_histogram(aes( x = cons.price.idx, fill = y), data = dataset,
                                 binwidth = 1 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Cons.Price.Index Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p20

#Consumer Confidence Index
p21 <- ggplot() + geom_histogram(aes( x = cons.conf.idx, fill = y), data = dataset,
                                 binwidth =5 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Cons.Conf.Index Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p21

#Euro Interbank Offered Rates
p22 <- ggplot() + geom_histogram(aes( x = euribor3m, fill = y), data = dataset,
                                 binwidth =0.01 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("Euro.Interbank.Rate Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p22

#No. of Employees
p23 <- ggplot() + geom_histogram(aes( x = nr.employed, fill = y), data = dataset,
                                 binwidth =100 ) +
  labs(fill="Subscription") + 
  scale_y_continuous(name = "Count")+
  ggtitle("No.of Employees Vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p23

#FEATURE ENGINEERING

#Feature Binning - Age Variable
dataset$age <- cut(dataset$age, c(0,19,35,60,100), labels = c("Teens","Young Adults", "Adults", "Senior Citizens"))


#Feature Selection

#1
#Checking the predictor variables that are highly correlated with each other
#Two metrics are used - Correlation factor and VIF

#Correlation Factor
bank_cor <- subset(dataset, select=-c(y))
for(i in 1:ncol(bank_cor)){bank_cor[,i]<- as.integer(bank_cor[,i])} #Changing the variables into integer
correlationMatrix <- cor(bank_cor) #Correlation matrix
#Finding attributes that are highly correlated (>0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose = TRUE)
print(highlyCorrelated) #Result -> 'euribor3m' and 'emp.var.rate'

#VIF Factor
#Defining a custom VIF Function
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

#Checking the variables that have VIF value greater than 10
vif_func(in_frame=bank_cor,thresh=10,trace=T) #Result - 'euribor3m'

#From the above 2 metrics- it is decided to remove the 'euribor3m' variable
dataset <- subset(dataset, select = -c(euribor3m))


#PRE-PROCESSING:

#datasetRan <- dataset[order(runif(nrow(dataset))),]

#Splitting the dataseting data into train and validation set

i = sample.split(dataset$y, SplitRatio = 0.8)
new_train_pre = subset(dataset, i ==TRUE)
new_test = subset(dataset, i ==FALSE)



#Dealing with Imbalanced Data
#Synthetic Data generation method is used to balance the classes
#Specifically, SMOTE Technique is used

new_train <-SMOTE(y~.,new_train_pre,perc.over = 400, perc.under = 150,k=5)
train_smote <- SMOTE(y~.,dataset, perc.over = 400, perc.under = 150, k=5)


#checking the proportion of classes before and after SMOTE
prop.table(table(new_train_pre$y)) # No-89%, Yes-11%
prop.table(table(new_train$y)) #No-55%, Yes-45%

prop.table(table(dataset$y)) # No-89%, Yes-11%
prop.table(table(train_smote$y)) #No-55%, Yes-45%


#MODELLING - VARIOUS ALGORITHMS are USED BELOW
set.seed(123)

#1 - C5.0 Decision Tree

#Model Execution

C50_model <- C5.0(y ~.,data = new_train)

#Summary Statistics
summary(C50_model)


#C5.0 Prediction
pred_C50<- predict(C50_model,new_test, type="class")

#Confusion Matrix
confusion_C50 <- confusionMatrix(data= pred_C50,reference = new_test$y)
confusion_C50


#C5.0 Train improve performance (boosting)

C50_model_imp <- C5.0(y ~.,data = new_train,trials=100)
summary(C50_model_imp)

#C5.0 improved Predict
predict_C50_imp <-predict(C50_model_imp,new_test, method = "class")

#Confusion Matrix

confusion_improved <- confusionMatrix(predict_C50_imp, new_test$y)
confusion_improved



#2 - Random Forest

#Model Execution
rf_model<-randomForest(y ~.,data = new_train, importance=TRUE, ntree=100)

plot(rf_model)

#Summary Statistics
summary(rf_model)

#Variable Importance
varImpPlot(rf_model)

#Prediction
pred_rf<- predict(rf_model,new_test, type="response")
#pred_rf <- pred_rf[,2]

#Confusion Matrix
confusion_rf <- confusionMatrix(data= pred_rf,reference = new_test$y)
confusion_rf



#3 - eXtreme Gradient Boosting (XGBoost)

#Using one hot encoding 
labels <- new_train['y'] 

new_train$y<- recode(new_train$y,"'yes'=1; 'no'=0")

#prepare training data
trainm = model.matrix(y ~., data = new_train)
train_label = as.numeric(as.character(new_train[,"y"]))
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label = train_label)

#prepare testing/validating data
new_test$y<- recode(new_test$y,"'yes'=1; 'no'=0")
#prepare testy data
testm = model.matrix(y ~., data = new_test)
test_label = as.numeric(as.character(new_test[,"y"]))
test_matrix = xgb.DMatrix(data = as.matrix(testm), label = test_label)


#parameters
xgb_params = list(booster = "gbtree",
                  objective   = "binary:logistic",
                  eval_metric = "error",
                  max_depth   = 3,
                  eta         = 0.01,
                  gammma      = 1,
                  colsample_bytree = 0.5,
                  min_child_weight = 1)
#model
bst_model = xgb.train(params = xgb_params, 
                      data = train_matrix,
                      watchlist = list(train=train_matrix, test=test_matrix),
                      nrounds = 75,
                      nfold = 6)


#feature importance
imp = xgb.importance(colnames(train_matrix), model = bst_model)
imp = xgb.importance(colnames(trainm), model = bst_model)
xgb.plot.importance(imp)
#prediction
p = predict(bst_model, newdata = test_matrix)
pred.resp <- ifelse(p > 0.55,'yes','no')

new_test$y<- recode(new_test$y,"1='yes'; 0='no'")

pred.resp<- factor(pred.resp)

# confusion matrix

confusionMatrix(pred.resp, new_test$y)
