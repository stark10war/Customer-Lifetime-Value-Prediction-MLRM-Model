.#----------------------------------Multiple Linear Regression Modelling--------------------------------#

#-----------------------------Customer Lifetime Value Prediction By Shashank Tanwar------------------------


#Problem Statement: 

#To examine which are the factors which influene,  Customer Lifetime Value (CLV) of Insurance Premium Company, based on the given attributes of the customer
#Business Context: What are the  attributes of customer, who have a higher Customer Lifetime Value




#------------------------------Preparing the environment for MLRM---------------------------------------#

list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"C:/Users/Stark/Documents/ivy files/R/IVY--R PREDICTIVE ANALYTICS -- RESOURCES-20180618T082011Z-001/IVY--R PREDICTIVE ANALYTICS -- RESOURCES/01DATASETS"
setwd(Path)
getwd()

data=read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
data1=data #To create a backup of original data


#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)


#Renaming the Dependent var
colnames(data1)[which(names(data)=="Customer.Lifetime.Value")]="clv"




#-------------->Outlier Treatment through quantile method


quantile(data1$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data2=data1[data1$clv <36000,]#creating a new data frame with a cap on outliers

nrow(data1)

nrow(data2)

nrow(data1)-nrow(data2)

quantile(data2$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))



data3=data2[data2$clv <14722, ]

nrow(data3)

quantile(data3$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))



nrow(data1)-nrow(data3)
str(data3)

#-------------->Treatment of missing values:Example with substituting with mean of the series

#-------------->Droping the rendundant variables from the data frame
as.data.frame(colnames(data3))
data4=select(data3,-c(Customer,State,Effective.To.Date))

str(data4)#final dataset for modelling


#Exporting the treated data into csv file

write.csv(data4,"MLdata.csv")



#--------------------------Splitting the data into training and test data set------------------------#

set.seed(123)
spl = sample.split(data4$clv, 0.7)#Splits the overall data into train and test data in 70:30 ratio

original.data = subset(data4, spl == TRUE)
str(original.data)
dim(original.data)

test.data = subset(data4, spl == FALSE)
str(test.data)
dim(test.data)


#------------------------------------------Fitting the model---------------------------------------#
#Iteration.1
LinearModel0=lm(clv~.,data=original.data)
summary(LinearModel0)

#Removing the first two insignificant variables: policy and policy types
#Iteration.2.
LinearModel1=lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Sales.Channel+	Total.Claim.Amount+	Vehicle.Class+	Vehicle.Size, data=original.data)
summary(LinearModel1)


#Removing the insignificant variables: Total Claim, Sales Channels, Policy Special, Months Since Policy Inception, Marital  Status, Location, Income
#Iteration.3.
LinearModel2=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Vehicle.Class, data=original.data)
summary(LinearModel2)



#Removing the insignificant classes in dummy variables
#Iteration.4.
LinearModel3=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV")+ I(Vehicle.Size == "Small"), data=original.data)
summary(LinearModel3)



#Iteration.5.
LinearModel4=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)
summary(LinearModel4)


#Iteration.6.
LinearModel5=lm(clv~	Coverage+	I(Education=="College")+I(Education =="High School or Below")+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)
summary(LinearModel5)


##---------------------------------------------------Final Model

FinalModel=lm(clv~	Coverage+	I(EmploymentStatus=="Medical Leave")+	 Monthly.Premium.Auto + Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)

summary(FinalModel)
#Months.Since.Last.Claim
#+I(EmploymentStatus =="Retired")

#Checking Multicollinearity in the model

## Get the predicted or fitted values
vif(FinalModel)


## Get the predicted or fitted values
fitted(FinalModel)

par(mfrow=c(2,2))
plot(FinalModel)



## MAPE
original.data$pred <- fitted(FinalModel)
write.csv(original.data,"mape.csv")


#Calculating MAPE
attach(original.data)
MAPE<-print((sum((abs(clv-pred))/clv))/nrow(original.data))

############ Residual Analysis ############################################################################

res <- original.data

res$stu_res <- studres(FinalModel) ##Studentized residuals
res$stud.del.resids <- rstudent(FinalModel) ##studentized deleted residuals
res$leverage <- hatvalues(FinalModel) ## leverage values (hi)
res$cooks_dis <- cooks.distance(FinalModel) ## Cook's distance
res$dffits <- dffits(FinalModel) ## Dffit
res$dfbetas <- dfbetas(FinalModel) ## Dfbetas
res$cov_ratio <- covratio(FinalModel) ## Covariance Ratio

write.csv(res,"res.csv")

##################################### Checking of Assumption ############################################

# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

durbinWatsonTest(FinalModel)
dwt(FinalModel)
#Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation)

# Checking multicollinearity
vif(FinalModel) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(FinalModel)  # Null hypothesis -> error is non-homogenious (p value should be more than 0.05)


#Cook-Weisberg test
# hypothesis of constant error variance against the alternative that the error variance changes with the level of the  response 
# p value should be more than 0.05
ncvTest(lm(clv~ 	Coverage +	I(Education=="College") 
           + I(EmploymentStatus=="Medical Leave")+ I(EmploymentStatus=="Retired")+ I(EmploymentStatus=="Unemployed")
           +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
           +	I(Vehicle.Class=="SUV")  , data=original.data))


## Normality testing Null hypothesis is data is normal.

resids <- FinalModel$residuals


ad.test(resids) #get Anderson-Darling test for normality 
cvm.test(resids) #get Cramer-von Mises test for normaility 
lillie.test(resids) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids) #get Pearson chi-square test for normaility 
sf.test(resids) #get Shapiro-Francia test for normaility 

qqnorm(resids)


###########################################################################################################################
############## Testing the model on test data ############################################################################
###########################################################################################################################


##Final model 
fit1<- lm(clv~ 	Coverage +	I(Education=="College") 
          + I(EmploymentStatus=="Medical Leave")+ I(EmploymentStatus=="Retired")+ I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
          +	I(Vehicle.Class=="SUV")  , data=test.data)
summary(fit1)


##Remove I(EmploymentStatus=="Medical Leave"), I(EmploymentStatus=="Retired"),I(Education=="College") 
fit1<- lm(clv~ 	Coverage +  I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
          +	I(Vehicle.Class=="SUV")  , data=test.data)
summary(fit1)

##Remove Coverage Extended
fit1<- lm(clv~ 	I(Coverage=="Premium")  +  I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
          +	I(Vehicle.Class=="SUV")  , data=test.data)
summary(fit1)

##Final Model
fit1<- lm(clv~ 	I(Coverage=="Premium")  +  I(EmploymentStatus=="Unemployed")
          +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
          +	I(Vehicle.Class=="SUV")  , data=test.data)
summary(fit1)

#Check Vif, vif>2 means presence of multicollinearity
vif(fit1)


## Get the predicted or fitted values
fitted(fit1)

## MAPE
test.data$pred <- fitted(fit1)
#write.csv(ori_data,"mape.csv")

#Calculating MAPE
attach(test.data)
(sum((abs(clv-pred))/clv))/nrow(test.data)



############ Residual Analysis ############################################################################

res1 <- test.data

res1$stu_res <- studres(fit1) ##Studentized residuals
res1$stud.del.resids <- rstudent(fit1) ##studentized deleted residuals
res1$leverage <- hatvalues(fit1) ## leverage values (hi)
res1$cooks_dis <- cooks.distance(fit1) ## Cook's distance
res1$dffits <- dffits(fit1) ## Dffit
res1$dfbetas <- dfbetas(fit1) ## Dfbetas
res1$cov_ratio <- covratio(fit1) ## Covariance Ratio

#write.csv(res1,"res1.csv")

##################################### Checking of Assumption ############################################

# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

durbinWatsonTest(fit1)
dwt(fit1)

# Checking multicollinearity
vif(fit1) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(fit1)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)


#Cook-Weisberg test
# hypothesis of constant error variance against the alternative that the error variance changes with the level of the  response 
# p value should be more than 0.05
ncvTest(lm(clv~ 	I(Coverage=="Premium")  +  I(EmploymentStatus=="Unemployed")
           +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
           +	I(Vehicle.Class=="SUV")  , data=test.data))


## Normality testing Null hypothesis is data is normal.

resids1 <- fit1$residuals


ad.test(resids1) #get Anderson-Darling test for normality 
cvm.test(resids1) #get Cramer-von Mises test for normaility 
lillie.test(resids1) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids1) #get Pearson chi-square test for normaility 
sf.test(resids1) #get Shapiro-Francia test for normaility 

qqnorm(resids1)


