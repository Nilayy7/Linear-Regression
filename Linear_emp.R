#Build a prediction model for Churn_out_rate 
View(emp_data)

# Scatter Diagram (Plot x,y)
plot(emp_data$Salary_hike,emp_data$Churn_out_rate)

# Other Exploratory data analysis and Plots
boxplot(emp_data)

#Histogram
hist(emp_data$Salary_hike)
hist(emp_data$Churn_out_rate)

CR <- emp_data$Churn_out_rate
SH <- emp_data$Salary_hike
#Correlation 
cor(CR,SH)
# This has a strong negative Correlation 

# Simple model without using any transformation
reg<-lm(CR~SH)
p <- predict(reg,interval = "predict")
plot(reg)
summary(reg)
#The multiple-R-Squared Value is 0.8312 and Adjusted R-Squared Value is 0.8101

# Applying transformations
# Logarthmic transformation
reg1<-lm(CR~log(SH))  # Regression using logarthmic transformation
p1 <- predict(reg1,interval = "predict")
plot(reg1)
summary(reg1)
#The multiple-R-Squared Value is 0.8486 and Adjusted R-Squared Value is 0.8297

#### Applying different transformations
# Exponential model 
reg2<-lm(log(CR)~SH)
p2 <- predict(reg2,interval = "predict")
plot(reg2)
summary(reg2)
#The multiple-R-Squared Value is 0.8735 and Adjusted R-Squared Value is 0.8577

# Higher the R-sqaured value - Better chances of getting good model 
#Quad Model
reg3 <- lm(CR~SH+I(SH^2),data=emp_data)
p3 <- predict(reg3, interval="predict")
plot(reg3)
summary(reg3)
#The multiple-R-Squared Value is 0.9737 and Adjusted R-Squared Value is 0.9662

#Cubic Model
reg4 <- lm(CR~SH+I(SH^2)+I(SH^3),data=emp_data)
p4 <- predict(reg3, interval="predict")
plot(reg4)
summary(reg4)
#The multiple-R-Squared Value is 0.9893 and Adjusted R-Squared Value is 0.9842

rmse<-sqrt(mean((p4-CR)^2))
rmse
