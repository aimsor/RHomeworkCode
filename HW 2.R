getwd()
#Question 3 (Salary is in Thousands of Dollars)
Function3 = function(x1, x2, x3){
  Salary = 50 + 20 * x1 + 0.07 * x2 + 35 * x3 + 0.01 * (x1 * x2) - 10 * (x1 * x3)
  Salary
}

#Part a
Function3(10,3,1)
Function3(10,3,0)

Function3(4,110,1)
Function3(4,110,0)

Function3(3,110,1)
Function3(3,110,0)

Function3(2,110,1)
Function3(2,110,0)

Function3(1,110,1)
Function3(1,110,0)

Function3(3.5,110,1)
Function3(3.5,110,0)

Function3(0,110,1)
Function3(0,110,0)

#Part b
Function3(4,110,1)

#Part c
Function3(4,110,1)
Function3(4,109,1)


#Question 9
Auto = read.csv("Auto.csv",header = TRUE, na.string="?")
Auto = na.omit(AutoData)
#Part B
?cor
cor(subset(Auto, select = -name))

#Part C
Fit1 = lm(mpg~.-name, data = Auto)
summary(Fit1)
plot(Fit1)
?lm

names(Auto)
#Part E
Fit2 = lm(mpg~.+mpg*cylinders-name)

Fit2 = lm(mpg~.+mpg:cylinders+mpg:displacement+mpg:horsepower+mpg:weight+
            mpg:acceleration+mpg:year+mpg:origin+cylinders:displacement+
            cylinders:horsepower+cylinders:weight+cylinders:acceleration+
            cylinders:year+cylinders:origin+displacement:horsepower+displacement:weight+
            displacement:acceleration+displacement:year+displacement:origin+
            horsepower:weight+horsepower:acceleration+horsepower:year+horsepower:origin+
            weight:acceleration+weight:year+weight:origin+acceleration:year+
            acceleration:origin+year:origin-name, data = Auto)
summary(Fit2)

#Part F
pairs(Auto)
pairs(Auto)
attach(Auto)
pairs(mpg~horsepower+weight)

FitF1 = lm(mpg~I(displacement^2), data = Auto)
summary(FitF1)

FitF2 = lm(mpg~I(displacement^.5), data = Auto)
summary(FitF2)

FitF3 = lm(mpg~I(log(displacement)), data = Auto)
summary(FitF3)


FitF1a = lm(mpg~I(horsepower^2), data = Auto)
summary(FitF1)

FitF2a = lm(mpg~I(horsepower^.5), data = Auto)
summary(FitF2)

FitF3a = lm(mpg~I(log(horsepower)), data = Auto)
summary(FitF3)


FitF1b = lm(mpg~I(weight^2), data = Auto)
summary(FitF1)

FitF2b = lm(mpg~I(weightt^.5), data = Auto)
summary(FitF2)

FitF3b = lm(mpg~I(log(weight)), data = Auto)
summary(FitF3)


FitF1c = lm(horsepower~I(acceleration^2), data = Auto)
summary(FitF1)

FitF2c = lm(horsepower~I(acceleration^.5), data = Auto)
summary(FitF2)

FitF3c = lm(horsepower~I(log(acceleration)), data = Auto)
summary(FitF3)
