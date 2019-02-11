#remove all objects
rm(list = ls())

#Setting the working directory

getwd()
setwd("E:/ABHI/COURSES/EDWISOR/PROJECTS/CHURN REDUCTION")

#Importing the libraries
library(ggplot2)
library(gridExtra)
library(DMwR)
library(corrgram)
library(outliers)


#importing the test and train data set
test_data = read.csv("Test_data.csv", header = T)
train_data = read.csv("Train_data.csv",header = T)

#Combining both the datasets
combined_data = rbind(train_data, test_data)

summary(combined_data)
names(combined_data)
str(combined_data)

#CATEGORICAL VARIABLES
cat_variables = c('state','area.code','international.plan',
                  'number.customer.service.calls','voice.mail.plan', 'Churn')

#CONTINUOUS VARIABLES
cont_variables=c('account.length', 'number.vmail.messages',
                 'total.day.minutes', 'total.day.calls', 'total.day.charge',
                 'total.eve.minutes', 'total.eve.calls', 'total.eve.charge',
                 'total.night.minutes', 'total.night.calls', 'total.night.charge',
                 'total.intl.minutes', 'total.intl.calls', 'total.intl.charge')


#####   MISSING VALUE ANALYSIS

#Creating another dataframe for missing values
missing_data=data.frame(apply(combined_data,2,function(x){sum(is.na(x))}))

#Renaming the column of missing_data
names(missing_data)[1] =  "missing values"

#Saving the dataset into our system
write.csv(missing_data,"Missing Values.csv")

#We can see that there are no missing values in our dataset

#########Converting the categorical variables in factors

for(i in cat_variables)
{
        combined_data[,i] = as.factor(combined_data[,i])
}

####### OUTLIER ANALYSIS

for (i in 1:length(cont_variables))
{
        assign(paste0("gn",i), ggplot(aes_string(y = (cont_variables[i]), x = "Churn"), data = combined_data)+ 
                       stat_boxplot(geom = "errorbar", width = 0.5) +
                       geom_boxplot(outlier.colour="blue", fill = "yellow" ,outlier.shape=12,
                                    outlier.size=2, notch=FALSE) +
                       theme(legend.position="bottom")+
                       labs(y=cont_variables[i],x="CHURN")+
                       ggtitle(paste("Box plot for",cont_variables[i])))
}

## Grid Plot
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,gn11,gn12,ncol=2)
gridExtra::grid.arrange(gn13,gn14,ncol=2)


#Replacing Outlier Values by NA
for(i in cont_variables)
{
        outlier_val = combined_data[,i][combined_data[,i] %in% boxplot.stats(combined_data[,i])$out]
        combined_data[,i][combined_data[,i] %in% outlier_val] = NA
}

#Imputation of Outlier values

#Checking which method is better for missing values imputation since we have replaced all outliers by NA
#Voluntarily deleting 2nd observation  of total night minutes
#combined_data[2,14]
#Actual value [2,20] = 254.4
#Value form mean method = 200.422
#Value from median method = 200.4
#Value from Knn method = 237.62 (with k=5)

#combined_data[2,14] = NA
#Mean method of imputation
#combined_data$total.night.minutes[is.na(combined_data$total.night.minutes)] = mean(combined_data$total.night.minutes,na.rm=T) #na.rm=T will remove all NA values and calculate the mean
#combined_data[2,14]

#combined_data[2,14] = NA
#Median method of imputation
#combined_data$total.night.minutes[is.na(combined_data$total.night.minutes)] = median(combined_data$total.night.minutes,na.rm=T)  
#combined_data[2,14]

#combined_data[2,14] = NA
#Knn method of imputation
combined_data = knnImputation(combined_data,k=5)
#combined_data[2,14]

#Confirming if all the missing values are imputed 
sum(is.na(combined_data))

for(i in cat_variables)
{
        combined_data[,i] = as.factor(combined_data[,i])
}

#Dropping the variable phone number as it is not very relevant 
combined_data=subset(combined_data,select=-c(phone.number))

######CREATING NEW VARIABLES
#Total Minutes
combined_data$total.minutes = with(combined_data, combined_data$total.day.minutes + combined_data$total.eve.minutes + combined_data$total.night.minutes + combined_data$total.intl.minutes)

#Total Calls
combined_data$total.calls = with(combined_data, combined_data$total.day.calls + combined_data$total.eve.calls + combined_data$total.night.calls + combined_data$total.intl.calls)

#Total Charge
combined_data$total.charge = with(combined_data, combined_data$total.day.charge + combined_data$total.eve.charge + combined_data$total.night.charge + combined_data$total.intl.charge)

#Total average minutes per call
combined_data<-transform(combined_data, total.avgmin.percall=total.minutes/total.charge)

###Converting the newly created variables to categories

combined_data$charge_category[combined_data$total.charge>25 & combined_data$total.charge <=50] = 'Low'
combined_data$charge_category[combined_data$total.charge>50 & combined_data$total.charge <=75] = 'Medium'
combined_data$charge_category[combined_data$total.charge >75 ] = 'High'
combined_data$charge_category = as.factor(combined_data$charge_category)

combined_data$minutes_category[combined_data$total.minutes>200 & combined_data$total.minutes <=400] = 'Low'
combined_data$minutes_category[combined_data$total.minutes>400 & combined_data$total.minutes <=600] = 'Medium'
combined_data$minutes_category[combined_data$total.minutes >600 ] = 'High'
combined_data$minutes_category = as.factor(combined_data$minutes_category)

combined_data$call_category[combined_data$total.calls>150 & combined_data$total.calls <=250] = 'Low'
combined_data$call_category[combined_data$total.calls>250 & combined_data$total.calls <=350] = 'Medium'
combined_data$call_category[combined_data$total.calls >350 ] = 'High'
combined_data$call_category = as.factor(combined_data$call_category)


#Dropping the newly created variables as we have created their categorical equivalent
combined_data=subset(combined_data,select=-c(total.calls,total.charge,total.minutes))

newcont_variables=c('account.length', 'number.vmail.messages',
                    'total.day.minutes', 'total.day.calls', 'total.day.charge',
                    'total.eve.minutes', 'total.eve.calls', 'total.eve.charge',
                    'total.night.minutes', 'total.night.calls', 'total.night.charge',
                    'total.intl.minutes', 'total.intl.calls', 'total.intl.charge','total.avgmin.percall')

###CORRELATION PLOT
corrgram(combined_data[,newcont_variables], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#CCONSOLDATING NUMBER OF CUSTOMER CALLS BY ASSIGNING 3 or more customer service calls the same category
combined_data$num.customer.service.calls[combined_data$number.customer.service.calls==0]=0
combined_data$num.customer.service.calls[combined_data$number.customer.service.calls==1]=1
combined_data$num.customer.service.calls[combined_data$number.customer.service.calls==2]=2
combined_data$num.customer.service.calls[combined_data$number.customer.service.calls==3]=3
combined_data$num.customer.service.calls[is.na(combined_data$num.customer.service.calls)]<-3
combined_data$num.customer.service.calls = as.factor(combined_data$num.customer.service.calls)

combined_data=subset(combined_data,select=-c(number.customer.service.calls))


###CHI-SQUARE TEST
factor_index = sapply(combined_data,is.factor)

chisq_testdata = combined_data[,factor_index]

chisq_testdata=subset(chisq_testdata, select=-c(Churn))

for (i in 1:length(colnames(chisq_testdata)))
{
        print(names(chisq_testdata)[i])
        print(chisq.test(table(combined_data$Churn, chisq_testdata[,i])))
}

###Dropping variables
#Some of them can be removed after conducting chi-sq test & correlation plots
#Some of the variables are are not required after the creation of new variables
combined_data=subset(combined_data,select=-c(total.day.minutes, total.day.calls,total.day.charge,
                                             total.eve.minutes, total.eve.calls, total.eve.charge,
                                             total.night.minutes,total.night.calls,total.night.charge,
                                             total.intl.minutes, total.intl.calls, total.intl.charge,call_category))

#Rearranging the positions of all the columns
combined_data = combined_data[,c(1:6,8:11,7)]
colnames(combined_data)[8]<-'Total_charge'
colnames(combined_data)[9]<-'Total_minutes'


####FEATURE SCALING

histogram(combined_data$account.length)
histogram(combined_data$number.vmail.messages)
histogram(combined_data$total.avgmin.percall)


##Applying Normalization technique for scaling as the data is non-uniform
normal_index = sapply(combined_data, is.numeric)
normal_data = combined_data[,normal_index]

for (i in colnames(normal_data))
{
        print(i)
        combined_data[,i] = (combined_data[,i] - min(combined_data[,i]))/
                (max(combined_data[,i]) - min(combined_data[,i]))
}

######DATA VISUALIZATION
#COUNT PLOTS

#CountPlot_churn wrt number customer service calls
gc1 = ggplot(combined_data, aes(x = num.customer.service.calls, fill=Churn)) + geom_bar(stat = 'count')  +
        geom_label(stat='count',aes(label=..count..), size=5) 
gc1

#CountPlot_churn wrt area code
gc2 = ggplot(combined_data, aes(x = area.code, fill=Churn)) + geom_bar(stat = 'count')  +
        geom_label(stat='count',aes(label=..count..), size=5) 
gc2

#CountPlot_area wise customer service calls
gc3 = ggplot(combined_data, aes(x = area.code, fill=num.customer.service.calls)) + geom_bar(stat = 'count')  +
        geom_label(stat='count',aes(label=..count..), size=5) 
gc3


#CountPlot_churn wrt intl plan
gc4 = ggplot(combined_data, aes(x = international.plan, fill=Churn)) + geom_bar(stat = 'count')  +
        geom_label(stat='count',aes(label=..count..), size=5) 
gc4

#CountPlot_churn wrt voice mail plan
gc5 = ggplot(combined_data, aes(x = voice.mail.plan, fill=Churn)) + geom_bar(stat = 'count')  +
        geom_label(stat='count',aes(label=..count..), size=5) 
gc5


#CountPlot_churn wrt Total charge
gc6 = ggplot(combined_data, aes(x = Total_charge, fill=Churn)) + geom_bar(stat = 'count')  +
        geom_label(stat='count',aes(label=..count..), size=5) 
gc6


#CountPlot_number of customer calls wrt Total charge
gc7 = ggplot(combined_data, aes(x = Total_charge, fill=num.customer.service.calls)) + geom_bar(stat = 'count')  +
        geom_label(stat='count',aes(label=..count..), size=5) 
gc7


######DATA MODELLING
##LOGISTIC REGRESSION
#Dividing the data into train & test
train_index = sample(1:nrow(combined_data),0.8*nrow(combined_data))
train_data=combined_data[train_index,]
test_data=combined_data[-train_index,]

logitmodel=glm(Churn~.,data=train_data,family = "binomial")

summary(logitmodel)

#predict using logistic regression
predictlogit=predict(logitmodel,newdata = test_data,type='response')
#convert probabilities into class
predictlogit=ifelse(predictlogit>0.5,1,0)

##Evaluate the performance
confmatrix_lr = table(test_data$Churn,predictlogit)
confmatrix_lr


##DECISION TREE
library(rpart)
library(C50)
c50model = C5.0(Churn~., train_data, trials=100, rules=TRUE)
summary(c50model)
predict_dt = predict(c50model,newdata = test_data,type='class')
##Evaluate the performance
confmatrix_dt = table(test_data$Churn,predict_dt)
confmatrix_dt


##RANDOM FOREST
library(randomForest)
library(inTrees)
rfmodel = randomForest(Churn~.,train_data,importance=TRUE,ntree=300)
predicted_rf = predict(rfmodel,test_data[,-11])
##Evaluate the performance
confmatrix_rf = table(test_data$Churn,predicted_rf)
confmatrix_rf



##NAIVE BAYS
library(e1071)
nbmodel=naiveBayes(Churn~.,data=train_data)
nbpredict = predict(nbmodel,test_data[,-11],type='class')

##Evaluate the performance
confmatrix_nb = table(test_data$Churn,nbpredict)
confmatrix_nb
