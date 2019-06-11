############################################ EMPLOYEE ABSENTEEISM ###########################################


#remove all the objects stored
rm(list=ls())


#Getting Current working directory
getwd()

# Seting the working directory
setwd("D:/project2")
getwd()


##################################  Loading Data Test  ##################################################

library(xlsx)
emp_data = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1 ,header = TRUE)

##Getting the column names of the dataset
colnames(emp_data)

###Getting the structure of the dataset
str(emp_data)

##Getting the number of variables and obervation in the datasets
dim(emp_data)

#getting summary of the data
summary(emp_data)


#Separating Continuous and Fcatorial Variable name

fact_var = c("ID","Reason.for.absence" , "Month.of.absence" ,"Day.of.the.week" ,"Seasons", "Disciplinary.failure" , "Education"
        ,"Social.drinker","Social.smoker","Son","Pet" )
#cont_var = c( "Transportation.expense", "Distance.from.Residence.to.Work","Service.time","Age" ,"Work.load.Average.day."
#              ,"Hit.target","Weight",  "Height" ,"Body.mass.index", "Absenteeism.time.in.hours"  )

#Converting factorial Variable to Factor datatype

for( i in fact_var){
  emp_data[,i]=as.factor(emp_data[,i])
}

#Getting Structure of the data after datatype conversion

str(emp_data)

################################## Missing Values Analysis###############################################
#getting numbers of missing values of the each variable
missing_val = data.frame(apply(emp_data,2,function(x){sum(is.na(x))}))#Missing_value

missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(emp_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
#percentage of the missing values variable wise
write.csv(missing_val, "Miising_perc.csv", row.names = F)

#plotting the graph of variable wise missing values
library(ggplot2)
ggplot(data = missing_val[1:21,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
    geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
     ggtitle("Missing data percentage (employee)") + theme_bw()
## Try and error method to impute missing value
emp_data[4,"Body.mass.index"]
#actual value = 24
#Creating missing value by substituting Na at 4th row of "Body.mass.index" variable
emp_data[4,"Body.mass.index"]= NA

# imputation using Mean Method
#emp_data$Body.mass.index[is.na(emp_data$Body.mass.index)] = mean(emp_data$Body.mass.index, na.rm = T)
# 26.68785


#imputation using Median  Method
#emp_data$Body.mass.index[is.na(emp_data$Body.mass.index)] = median(emp_data$Body.mass.index, na.rm = T)
# 25

#imputation using kNN Imputation method

library(DMwR)

emp_data = knnImputation(emp_data, k = 3)
#24.32342

#looking for sum of missing values after imputation
sum(is.na(emp_data))

# Actual Value = 24

#mean method = 26.68785
#median Method = 25
# knn = 24.32342

## Here KNN imputation method is performing well ,we locked it for the missing value imputation  

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check

numeric_index = sapply(emp_data,is.numeric) #selecting only numeric

numeric_data = emp_data[,numeric_index]


cnames = colnames(numeric_data)
 
 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(emp_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Absenteeism.time.in.hours")+
            ggtitle(paste("Box plot of Absenteeism.time.in.hours for",cnames[i])))
 }
 
 ## Plotting plots together
 gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
 gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
 gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
 gridExtra::grid.arrange(gn10)

 
 
 #Replace all outliers with NA and imputing them using KNNimutation
 #create NA on outliers
 for(i in cnames){
   val = emp_data[,i][emp_data[,i] %in% boxplot.stats(emp_data[,i])$out]
   print(length(val))
   emp_data[,i][emp_data[,i] %in% val] = NA
 }
 sum(is.na(emp_data))
 
 ##KNNimputation
 
 emp_data = knnImputation(emp_data, k = 5)
 sum(is.na(emp_data))
 
 ########################################## FEATURE SELECTION ########################################################
 library(corrgram)
 library(usdm)
 corrgram(emp_data[,numeric_index], order = F,
         upper.panel=panel.cor, text.panel=panel.txt, main = "Correlation Plot")
 vif(numeric_data[,-10])
 vifcor(numeric_data)
 
 #removing "weight" variable from the dataset 
 reduced_emp_data=subset(emp_data,select= -c(Weight))
 
 library(stats)
 summary(aov(formula = Absenteeism.time.in.hours~ID,data = emp_data)) 
 summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Education,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Son,data = emp_data))
 summary(aov(formula = Absenteeism.time.in.hours~Pet,data = emp_data))
 str(emp_data)
 finaldata=reduced_emp_data
 
 str(finaldata)

 #########################################  FEATURE SCALING ############################################################
#Plotting histogram of continuous variable to have a insight of data distribution 
 par(mfrow=c(2,2))

hist(reduced_emp_data$Transportation.expense,xlab = "Transportation.expense",main = "Histogram of Transportation.expense ",col = "grey")
hist(reduced_emp_data$Distance.from.Residence.to.Work,xlab = "Distance.from.Residence.to.Work",main = "Histogram of Distance.from.Residence.to.Work",col = "grey")
hist(reduced_emp_data$Service.time,xlab = "Service.time",main = "Histogram of Service.time",col = "grey")

hist(reduced_emp_data$Age,xlab = "Age",main = "Histogram of Age",col = "grey")
hist(reduced_emp_data$Work.load.Average.day.,xlab = "Work.load.Average.day.",main = "Histogram of Work.load.Average.day.",col = "grey")
hist(reduced_emp_data$Hit.target,xlab = "Hit.target",main = "Histogram of Hit.target",col = "grey")
hist(reduced_emp_data$Son,xlab = "Son",main = "Histogram of Son",col = "grey")

hist(reduced_emp_data$Pet,xlab = "Pet",main = "Histogram of Pet",col = "grey")

hist(reduced_emp_data$Height,xlab = "Height",main = "Histogram of Height",col = "grey")
hist(reduced_emp_data$Body.mass.index,xlab = "Body.mass.index",main = "Histogram of Body.mass.index",col = "grey")
par(mfrow=c(1,1))
hist(numeric_data$Absenteeism.time.in.hours,xlab = "Absenteeism.time.in.hours",main = "Histogram of Absenteeism.time.in.hours",col = "grey")

##Applying Standardisation method to scale the data
reduced_numeric_index = sapply(reduced_emp_data,is.numeric) #selecting only numeric
reduced_numeric_data = reduced_emp_data[,reduced_numeric_index]
reduced_cnames = colnames(reduced_numeric_data)
View(reduced_cnames)
for(i in reduced_cnames)
{
  print(i)
  reduced_emp_data[,i] = (reduced_emp_data[,i] - mean(reduced_emp_data[,i]))/
    sd(reduced_emp_data[,i])
}
## Creating Dummies variable for categorical variables
library(dummies)
reduced_emp_data = dummy.data.frame(reduced_emp_data, fact_var)
colnames(reduced_emp_data)
str(reduced_emp_data)
dim(reduced_emp_data)
 
#############################################  SAMPLING ###############################################################
#Divide the data into train and test
library(rpart)
library(utils)
library(caret)
library(RDocumentation)
set.seed(123)
# Dividing data into train and test into ration of 80% : 20%
train_index = sample(1:nrow(reduced_emp_data), 0.8 * nrow(reduced_emp_data))
train = reduced_emp_data[train_index,]
test = reduced_emp_data[-train_index,]
###########################decision tree for regression#################################

# ##rpart for regression
fit_DT = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")

#Predict for train data 
pred_DT_train = predict(fit_DT, train[,-116])
#predict for the test data
pred_DT_test = predict(fit_DT, test[,-116])


library(DMwR)
# Error metrics for train data
regr.eval(train[,116],pred_DT_train,stats = c("rmse","mape"))
# Error metrics for test data
regr.eval(test[,116],pred_DT_test,stats = c("rmse","mape"))
summary(fit_DT)


######################## Random Forest ###############################################
library(randomForest)
library(RRF)
library(inTrees)

fit_RF = randomForest(Absenteeism.time.in.hours~., data = train)
summary(fit_RF)

#Lets predict for train data
pred_RF_train = predict(fit_RF, train[,names(test) != "Absenteeism.time.in.hours"])

#Lets predict for test data
pred_RF_test = predict(fit_RF,test[,names(test) != "Absenteeism.time.in.hours"])

#Error metrics For training data
regr.eval(train[,116],pred_RF_train,stats = c("rmse","mape"))
#Error metrics For testing data 
regr.eval(test[,116],pred_RF_test,stats = c("rmse","mape"))

################### Linear Regression model############################################

#run regression model
lm_model = lm(Absenteeism.time.in.hours ~., data = train)

#Summary of the model
summary(lm_model)

#Predict for the train data
pred_LR_train = predict(lm_model, train[,1:115])
#Predict for the test data
pred_LR_test = predict(lm_model, test[,1:115])
#error metrics for train data
regr.eval(train[,116],pred_LR_train,stats = c("rmse","mape"))

#Error metrics test data
regr.eval(test[,116],pred_LR_test,stats = c("rmse","mape"))

#####################  Principal Component Analysis ####################################
# Applying PCA on train data
pca_var = prcomp(train)

#compute standard deviation of each principal component
pca_std_dev = pca_var$sdev

#compute variance
pca_variance = pca_std_dev^2

#proportion of variance explained
prop_varex = pca_variance/sum(pca_variance)

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#training set with principal components after pca
train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, pca_var$x)

# From the above plot selecting 50 components since it explains almost 95+ % data variance
train.data =train.data[,1:50]

#forming test data after PCA
test.data = predict(pca_var, newdata = test)
test.data = as.data.frame(test.data)

#selecting the first 50 components
test.data=test.data[,1:50]
############################### Modeling after PCA########################################

########################### Decision tree for Regression after PCA #################################

#Develop Model on training data
pca_dt = rpart(Absenteeism.time.in.hours ~., data = train.data, method = "anova")


#Lets predict for training data
pca_pred_DT_train = predict(pca_dt, train.data)

#Lets predict for training data
pca_pred_DT_test = predict(pca_dt,test.data)


#Error metrics For training data 

regr.eval(train$Absenteeism.time.in.hours,pca_pred_DT_train,stats = c("rmse","mape"))
#Error metrics For testing data 
regr.eval(test$Absenteeism.time.in.hours,pca_pred_DT_test,stats = c("rmse","mape"))


###############################Random Forest after PCA #################################

#Develop Model on training data
pca_fit_RF = randomForest(Absenteeism.time.in.hours~., data = train.data)

#Lets predict for training data
pca_pred_RF_train = predict(pca_fit_RF, train.data)

#Lets predict for testing data
pca_pred_RF_test = predict(pca_fit_RF,test.data)

#Error metrics For training data 
regr.eval(train$Absenteeism.time.in.hours,pca_pred_RF_train,stats = c("rmse","mape"))


#Error metrics For testing data 
regr.eval(test$Absenteeism.time.in.hours,pca_pred_RF_test,stats = c("rmse","mape"))


################################-Linear Regression after PCA#############################

#Develop Model on training data
pca_fit_LR = lm(Absenteeism.time.in.hours ~ ., data = train.data)

#Lets predict for training data
pca_pred_LR_train = predict(pca_fit_LR, train.data)

#Lets predict for testing data
pca_pred_LR_test = predict(pca_fit_LR,test.data)

#Error metrics For training data 
regr.eval(train$Absenteeism.time.in.hours,pca_pred_LR_train,stats = c("rmse","mape"))

# For testing data 
regr.eval(test$Absenteeism.time.in.hours,pca_pred_LR_test,stats = c("rmse","mape"))


