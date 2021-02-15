setwd("C:/Users/SHYAM KRISHNAN K/Desktop/PJ-3/regression")
getwd()
library("readxl") 
library(psych)
library(car)

mydata <- read_excel("Household Data.xlsx")

########## exploratory data analysis
dim(mydata)
names(mydata)
str(mydata)
head(mydata)
tail(mydata)
colSums(is.na(mydata)) 
colnames(mydata)<-c("annual_income","monthly_income","member_no","expenditure","EMI")
############# descriptive data analysis.

attach(mydata)
str(mydata)
summary(mydata)

range(annual_income)
range(monthly_income)
range(member_no)
range(EMI)
range(expenditure)

sd(annual_income)
sd(monthly_income)
sd(member_no)
sd(EMI)
sd(expenditure)

var(annual_income)
var(monthly_income)
var(member_no)
var(EMI)
var(expenditure)


########## data transformation
colnames(mydata)<-c("annual_income","monthly_income","member_no","expenditure","EMI")
mydata<- transform(mydata, exp1000 = expenditure / 1000)
mydata$expenditure<- NULL
attach(mydata)


################### data visualization 

plot(mydata, pch=1, col="brown", main="Scatterplot of complete variables")

library(lattice)
plot(density(expenditure),col='green', main="density plot - expenditure")

hist(expenditure,main = 'Monthly Expenditure',xlab = 'expenditure',ylab = 'Frequency',col = 'orange')

par(mfrow=c(2,2))
hist(annual_income,main = 'Annual Income',xlab = 'annual_income',ylab = 'Frequency',col = 'green')
hist(member_no,main = 'Member Number',xlab = 'member_no',ylab = 'Frequency',col = 'pink')
hist(monthly_income,main = 'Monthly Income',xlab = 'monthly_income',ylab = 'Frequency',col = 'brown')
hist(EMI,main = 'EMI',xlab = 'EMI',ylab = 'Frequency',col = 'blue')
dev.off()
boxplot(EMI, main="EMI", col = 'green',xlab = 'emi')

############# multivariate regression ###########
result1<-lm(exp1000~annual_income+monthly_income+member_no+EMI, data=mydata)
result1
aov(result1)
summary(result1)

result1a<-lm(exp1000~monthly_income+annual_income+member_no+EMI, data=mydata)
result1a
aov(result1a)
summary(result1a)

result2<-lm(exp1000~monthly_income+member_no+EMI, data=mydata)
result2
aov(result2)
summary(result2)

result6<-lm(exp1000~member_no+EMI, data=mydata)
result6
aov(result6)
summary(result6)

result5<-lm(exp1000~monthly_income+EMI, data=mydata)
result5
aov(result5)
summary(result5)



result3<-lm(exp1000~monthly_income+member_no, data=mydata)
result3
aov(result3)
summary(result3)
#################### test of assumptions #################

####Mean of the residuals is zero 
mean(result1$residuals)

#######Homoscedasticity of residuals 
par(mfrow=c(2,2))
plot(result1,col="blue")
dev.off()

############Errors and Expalantory Variables are uncorrelated
cor.test(annual_income,result1$residuals) 
cor.test(monthly_income,result1$residuals)
cor.test(member_no,result1$residuals)
cor.test(EMI,result1$residuals)

############multi-colinearity 
library(car)
vif(result1)
alias( lm(exp1000~annual_income+monthly_income+member_no+EMI, data=mydata))
vif(lm(exp1000~annual_income+monthly_income+member_no+EMI, data=mydata))
vif(lm(exp1000~monthly_income+member_no+EMI, data=mydata))
vif(lm(exp1000~annual_income+member_no, data=mydata))
#################### testing variables correlation ########
round(cor(cbind(annual_income,monthly_income,member_no,EMI,exp1000)),2)

############ Robust Regression #############
library(foreign)
library(MASS)

r_result1<-rlm(exp1000~annual_income+monthly_income+member_no+EMI, data=mydata)
r_result1
aov(r_result1)
summary(r_result1)

r_result2<-rlm(exp1000~monthly_income+member_no+EMI, data=mydata)
r_result2
aov(r_result2)
summary(r_result2)

r_result3<-rlm(exp1000~monthly_income+member_no, data=mydata)
r_result3
aov(r_result3)
summary(r_result3)

########log-log model###########
mydata$lna_income<-log(mydata$annual_income)
mydata$lnm_income<-log(mydata$monthly_income)
mydata$lnmembe<-log(mydata$member_no)
mydata$lnEMI<-log(mydata$EMI)
mydata$lnexp1000<-log(mydata$exp1000)

newlogdf <- c("lna_income", "lnm_income", "lnmembe","lnEMI","lnexp1000")
newdata <- mydata[newlogdf]
summary(newdata)
plot(newdata)

res_log<-lm(mydata$lnexp1000~mydata$lna_income+mydata$lnm_income+mydata$lnmembe+mydata$lnEMI)
res_log
aov(res_log)
summary(res_log)

res_log1<-lm(mydata$lnexp1000~mydata$lna_income+mydata$lnmembe)
res_log1
aov(res_log1)
summary(res_log1)

res_log2<-lm(mydata$lnexp1000~mydata$lna_income+mydata$lnmembe+mydata$lnEMI)
res_log2
aov(res_log2)
summary(res_log2)

res_log3<-lm(mydata$lnexp1000~mydata$lna_income+mydata$lnEMI)
res_log3
aov(res_log3)
summary(res_log3)

############## parsimony
######### stepwise regression
library(leaps)

Null<-lm(exp1000~1, data=mydata)
Full<-lm(exp1000~annual_income+monthly_income+member_no+EMI, data=mydata) 
step(Null, scope = list(upper=Full), data=mydata, direction="both") 


#########prediction
finalmodel<-lm(formula = exp1000 ~ member_no + monthly_income, data = mydata)
finalmodel
anova(finalmodel)
summary(finalmodel)

predict(finalmodel,data.frame(member_no=3, monthly_income=4.5),interval="confidence")
predict(finalmodel,data.frame(member_no=3, monthly_income=4.5),interval="prediction")
