setwd("C:/Users/HP/OneDrive/Desktop/MA2142-Regression/Presentation-2/")
library(psych)
library(car)
library(lmtest)


# Load the data
dframe = read.csv("NFLX.csv")
#View(dframe)

# Head of the data
head(dframe)

# Checking if NA values are present in dataset 
colSums(is.na(dframe))

# Remove Unnecessary features
dframe$Date<-NULL

# Correlation Matrix
corrMat<-cor(dframe)
corrMat

# summary of the data
summary(dframe)


# Factor Analysis
fa.parallel(corrMat,fa = "both",  n.iter = 100)
fa <- fa(corrMat, nfactors = 2, rotate = "none" , fm = "pa")
fa

fa <- fa(corrMat, nfactors = 2, rotate = "varimax" , fm = "pa")
fa

# linear regression model
fit1 <- lm(Close ~ Open + High + Low 
                 + Volume, data = dframe)

summary(fit1)

# 1) test for linearity
plot(fit1, 1)

# 2) test for normality
hist(fit1$residuals, xlab = "Close", col = "red", border = "black")
shapiro.test((fit1$residuals))

# test for outliers
plot(fit1, 4)

cooksd <- cooks.distance(fit1)
extreme <- as.numeric(names(cooksd)[(cooksd > (4/nrow(dframe)))])
extreme
dframe <- dframe[-extreme, ]


# Updated linear model
fit2 <- lm(Close ~ Open + High + Low + 
             + Volume, data = dframe)

summary(fit2)
plot(fit2, 4)


# normality of updated model
hist(fit2$residuals, xlab = "Closing Price", col = "red", border = "black")
shapiro.test((fit2$residuals))

# 3) test for multicollinearity
vif(fit2)

# Model re-specification
fit3 <- lm(Close ~ Open + Volume, data = dframe)
vif(fit3)

# 4) test for homoscedasticity
gqtest(fit3, data = dframe)

# "scale-location" plot
plot(fit3, 3)


# 5) test for autocorrelation
durbinWatsonTest(fit3)
