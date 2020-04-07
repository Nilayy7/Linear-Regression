#Predict delivery time using sorting time 
View(delivery_time)

# Scatter Diagram (Plot x,y)
plot(delivery_time$Sorting.Time,delivery_time$Delivery.Time)

# Other Exploratory data analysis and Plots
boxplot(delivery_time)

#Plot Histogram
hist(delivery_time$Sorting.Time)
hist(delivery_time$Delivery.Time)

summary(delivery_time)

# Correlation coefficient value for Delivery Time and Sorting Time
dt<- delivery_time$Delivery.Time
st <- delivery_time$Sorting.Time
cor(st,dt)
#We have a strong correlation

# Simple model without using any transformation
reg<-lm(dt~st)
summary(reg)
plot(reg)
p <- predict(reg,interval  = "prediction")
# Adjusted R-squared value for the above model is 0.6655 

# Applying transformations
# Logarthmic transformation
reg1<-lm(dt~log(st))  # Regression using logarthmic transformation
summary(reg1)
# Adjusted R-squared:  0.6794 
plot(reg1)
p1 <- predict(reg1,interval = "prediction")

# Exponential model 
reg2<-lm(log(dt)~st) # regression using Exponential model
summary(reg2)
plot(reg2)
p2 <- predict(reg2,interval = "prediction")
# R-squared value is 0.7109
# Adjusted R SQuare Value is 0.6957

# Cubic model
reg3 <- lm(dt~st+I(st^2)+I(st^3),data=delivery_time)
summary(reg3) 
plot(reg3)
p3 <- predict(reg3,interval = "prediction")
rmse<-sqrt(mean((p3-dt)^2))
rmse
