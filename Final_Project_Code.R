data <- read.csv("Life_Expectancy_Data.csv")
attach(data)
library("car")
library("tree")
library("stargazer")
library("MASS")

# Slice data into variables you only want to regress life expectancy with
# 17 and 18 are thiness which wasn't a variable we said we would use
# omit nan for the step to work
fitall <- data[,3:23]
fitall <- fitall[,-c(17,18)]
fitall <- na.omit(fitall)
# fitall.1 <- fitall[,1:18]

stargazer(fitall,type="",out="final.html")
par(mfrow=c(1,1))

#Our two boxplots for descriptive statistsics
boxplot(Life.expectancy~Status,
        data=fitall,
        col=(c("blue","green")),
        main="Boxplot for Life Exp vs Status")

boxplot(Life.expectancy~Continent,
        data=fitall,
        col=(c("blue","green","orange","yellow","red","purple")),
        main="Boxplot for Life Exp vs Continent",
        las=2)

##### Fit Two Linear Models one with Contenient and one with status
## First one is with status ----------------------------------------
fitall.status <- fitall[,1:18]
lm.mod1 <- lm(Life.expectancy~.,data=fitall.status)
summary(lm.mod1)
vif(lm.mod1)

# Remove infant deaths
fitall.status <- fitall.status[,-c(4)]
lm.mod1 <- lm(Life.expectancy~.,data=fitall.status)
summary(lm.mod1)
vif(lm.mod1)

# Take out GDP
fitall.status <- fitall.status[,-c(14)]
lm.mod1 <- lm(Life.expectancy~.,data=fitall.status)
summary(lm.mod1)

# Plot this model
par(mfrow=c(2,2))
plot(lm.mod1)

## Do the same thing but with continent --------------------------
fitall.cont <- fitall[,2:19]
lm.mod2 <- lm(Life.expectancy~.,data=fitall.cont)
summary(lm.mod2)
vif(lm.mod2)

# Remove infant.deaths for high VIF
fitall.cont <- fitall.cont[,-c(3)]
lm.mod2 <- lm(Life.expectancy~.,data=fitall.cont)
summary(lm.mod2)
vif(lm.mod2)

# Remove GDP for high VIF
fitall.cont <- fitall.cont[,-c(13)]
lm.mod2 <- lm(Life.expectancy~.,data=fitall.cont)
summary(lm.mod2)
vif(lm.mod2)

# Remove Total Expenditure for high p-value
fitall.cont <- fitall.cont[,-c(10)]
lm.mod2 <- lm(Life.expectancy~.,data=fitall.cont)
summary(lm.mod2)

# Remove Hepatitis B for high p-value
fitall.cont <- fitall.cont[,-c(5)]
lm.mod2 <- lm(Life.expectancy~.,data=fitall.cont)
summary(lm.mod2)

# Remove Population for high p-value
fitall.cont <- fitall.cont[,-c(11)]
lm.mod2 <- lm(Life.expectancy~.,data=fitall.cont)
summary(lm.mod2)

# Plot to check lm assumptions
plot(lm.mod2)

# Compare two plots
stargazer(lm.mod1,lm.mod2,type="html",out="3.htm")

#Start of cross validation
#Cross Validation for the one with status -------------------------------
#Set seed because there will be randomization from validation set split
set.seed(1)
#Split it half
train <- sample(1:nrow(fitall),nrow(fitall)/2)
attach(fitall)

#Using model containing status
lm.mod3 <- lm(`Life.expectancy`~.,data=fitall.status,subset=train)
summary(lm.mod3)

#calculating MSE for model containing status which was split by validation set. 
mean((`Life.expectancy`-predict(lm.mod3,fitall))[-train]^2)

#Cross Validation for the one with continent  -------------------------------
set.seed(2)
train2 <- sample(1:nrow(fitall.cont),nrow(fitall.cont)/2)
names(fitall.cont)
dim(fitall.cont[train2,])

#Using model containing Continent
lm.mod4 <- lm(Life.expectancy~.,data=fitall.cont,subset=train2)
summary(lm.mod4)

mean((`Life.expectancy`-predict(lm.mod4,fitall.cont))[-train2]^2)

##### Start of Decision Trees -------------------------------------------
## Do one with all the variables without status
# For randomization
set.seed(1)
fitall.tree <- fitall[,2:19] # Take out status
train3 <- sample(1:nrow(fitall.tree), nrow(fitall.tree)/2)
fitall.tree$Continent=as.factor(fitall.tree$Continent)

tree.lmfit <- tree(Life.expectancy~., data= fitall.tree, subset = train3)
summary(tree.lmfit)

# Producing tree visually
par(mfrow=c(1,1))
plot(tree.lmfit)
text(tree.lmfit, pretty=0)

# Cross validation to determine the best tree, the best tree size is 10, so we do not need to prune the tree
cv.Continents <- cv.tree(tree.lmfit)
cv.Continents
plot(cv.Continents$size,cv.Continents$dev,type='b')

# Now we want to generate a value for MSE so that we can compare with previous models
yhat=predict(tree.lmfit,newdata=fitall.tree[-train3,])
Life_Expectancy.test=fitall.tree[-train3,"Life.expectancy"]

# We also made a plot to show how the true y compares to the predicted y. The plot shows that our model's predictions follows a similar trend to the real values.
par(mfrow=c(1,1))
plot(yhat,Life_Expectancy.test)
abline(0,1)

#MSE (Mean of Squared Errors)
mean((yhat-Life_Expectancy.test)^2)

## Do one with all the variables with status
# For randomization
set.seed(1)
fitall.tree2 <- fitall
train4 <- sample(1:nrow(fitall.tree2), nrow(fitall.tree2)/2)
fitall.tree2$Continent=as.factor(fitall.tree2$Continent)
fitall.tree2$Status=as.factor(fitall.tree2$Status)

tree.lmfit2 <- tree(Life.expectancy~., data= fitall.tree2, subset = train4)
summary(tree.lmfit2)

# Producing tree visually
par(mfrow=c(1,1))
plot(tree.lmfit2)
text(tree.lmfit2, pretty=0)

# Cross validation to determine the best tree, the best tree size is 10, so we do not need to prune the tree
cv.Continents2 <- cv.tree(tree.lmfit2)
cv.Continents2
plot(cv.Continents2$size,cv.Continents2$dev,type='b')

# Now we want to generate a value for MSE so that we can compare with previous models
yhat2 <- predict(tree.lmfit2,newdata=fitall.tree2[-train4,])
Life_Expectancy.test2=fitall.tree2[-train4,"Life.expectancy"]

# We also made a plot to show how the true y compares to the predicted y. The plot shows that our model's predictions follows a similar trend to the real values.
par(mfrow=c(1,1))
plot(yhat2,Life_Expectancy.test2)
abline(0,1)

#MSE (Mean of Squared Errors)
mean((yhat2-Life_Expectancy.test2)^2)


