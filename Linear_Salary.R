#Build a prediction model to predict the salary hike based on years of experience.
View(Salary_Data)

#Scatter Plot
plot(Salary_Data$YearsExperience,Salary_Data$Salary)

#BoxPlot
boxplot(Salary_Data)

#Histogram
hist(Salary_Data$YearsExperience)
hist(Salary_Data$Salary)
summary(Salary_Data)

#Correlation
YE <- Salary_Data$YearsExperience
SH <- Salary_Data$Salary
cor(SH,YE)
# This has a strong Positive Correlation

#Simple Regression model 
reg <- lm(SH~YE)
p <- predict(reg,interval = "predict")
plot(reg)
summary(reg)
#Multi R-Squared error is 0.957, Adjusted R-squared method is 0.9554

#Exponential Transformation
reg1 <- lm(SH~log(YE))
p1 <- predict(reg1,interval = "predict")
plot(reg1)
summary(reg1)
#Multi R-Squared error is 0.8539, Adjusted R-squared method is 0.8487

#cubic model
reg2 <- lm(SH~YE+I(YE^2)+I(YE^3),data = Salary_Data)
p2 <-predict(reg2,interval = "predict")
plot(reg2)
summary(reg2)
#Multi R-Squared error is 0.9636, Adjusted R-squared method is 0.9594

Final <- cbind(YearsofExp=Salary_Data$YearsExperience,Sal_Hike = Salary_Data$Salary,Pred_sal_hike=p2)
View(Final)

rmse<-sqrt(mean((p2-SH)^2))
rmse
