#Chapter #4 Q11
#Part A
getwd()
require(ISLR)
summary(Auto)
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg>median(mpg)] = 1
Auto = data.frame(Auto, mpg01)
#From this moment on, do not use mpg and mpg01 at the same time
#Something bad might happen?
summary(Auto)

#Part B
mpg01
pairs(Auto)
pairs(mpg01~displacement+horsepower+weight+acceleration)
hcylinders = as.factor(cylinders)
origin = as.factor(origin)
plot(cylinders, mpg01)
plot(origin, mpg01)
?Auto

#Part C
#We could use the maximum year 82 as our test data.
#As such we should use everything except for 82 as training data.
train = year < 82
test = !train
glm.fit = glm(mpg01~horsepower+weight+acceleration, data = Auto, family = binomial, subset = train)
summary(glm.fit)

glm.probs = predict(glm.fit, newdata = Auto[test,], type = "response")
glm.predict = ifelse(glm.probs > 0.5, "Good", "Bad")

DirectionTest = Auto$mpg01[test]


#Part D
#LDA Fit
require(MASS)
lda.fit = lda(mpg01~horsepower+weight+acceleration, data = Auto, subset = train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit,newdata = Auto[test,])
data.frame(lda.pred)[1:5,]
mean(lda.pred$class == DirectionTest)
mean(lda.pred$class != DirectionTest)

#Part E
#QDA Fit
qda.fit = qda(mpg01~horsepower+weight+acceleration, data = Auto, subset = train)
qda.fit
plot(qda.fit)
qda.pred = predict(qda.fit,newdata = Auto[test,])
data.frame(qda.pred)[1:5,]
mean(qda.pred$class == DirectionTest)
mean(qda.pred$class != DirectionTest)

#Part F
#Logistic Regression
glm.fit = glm(mpg01~horsepower+weight+acceleration, data = Auto, subset = train)
glm.fit
plot(glm.fit)
glm.pred = predict(glm.fit,newdata = Auto[test,])
data.frame(glm.pred)[1:5,]
mean(glm.pred == DirectionTest)
mean(glm.pred != DirectionTest)


#Part G
#KNN
library(class)
attach(Auto)
Xpred = cbind(horsepower, weight, acceleration)
knn.pred = knn(Xpred[train,],Xpred[test,],Direction[train],k=1, prob = FALSE)
mean(knn.pred == DirectionTest)
mean(knn.pred != DirectionTest)

attach(Auto)
Xlag=cbind(horsepower, weight, acceleration)
train=Year<82
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
mean(knn.pred!=Direction[!train])

#Some error is coming out but I can't fix it...