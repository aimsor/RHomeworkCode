#Question 8
#A
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x-2*x^2+rnorm(100)

summary (x,y)

#B
plot(x,y)

#C
require(ISLR)
require(boot)
glm.fit = glm(y~x)
summary(glm.fit)
cv.glm(glm.fit)$delta

loocv = function(fit){
  h = lm.influence(fit)$h
  mean(((residuals(fit)/(1-h))^2))
}

cv.error = rep(0,4)
degree = 1:4
for(d in degree){
  glm.fit = glm(y~poly(x,d))
  cv.error[d] = loocv(glm.fit)
}

#F

#Question 10

