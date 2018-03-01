f = function(n) {
  result = c()
  s = 1
  for (i in n) {
    s = s + 1/factorial(i)
    result = c(result, s)
  }
  return(result)
}
n = 1:100
plot(n, f(n), type = 'p')
# lines(n, rep(exp(1), length(n)), col='red')
abline(h=exp(1),col='red')

############################################################
############################################################

N = 1:1000
d1 = 1:6
d2 = 1:6
count = c()

for (i in N) {
  s1 = sample(d1, i, replace=TRUE)
  s2 = sample(d2, i, replace=TRUE)
  count = c(count, length(which(s1+s2==6))/i)
}
plot(N, count, type = 'l')

############################################################
############################################################

n = 1:1000
bar_Z = c()
for (i in n) {
  Z = rnorm(i,0,1)
  bar_Z = c(bar_Z, sum(Z)/i)
}
plot(n, bar_Z, type = 'l')

median_Z = c()
mean_Z = c()
for (i in n) {
  Z = rnorm(100,0,1)
  median_Z = c(median_Z, median(Z))
  mean_Z = c(mean_Z, mean(Z))
}
mean(median_Z)

var(mean_Z)
var(median_Z)
# var(mean_Z) < var(median_Z)

############################################################
############################################################

x = c(3,4,5,6,8,10,12)
y = c(16,12,9.6,7.9,6,4.7,4)
n = length(x)

bar_x = mean(x)
bar_y = mean(y)
sxy = sum((x-bar_x)*(y-bar_y))/(n-1)
sxx = sum((x-bar_x)^2)/(n-1)

hat_beta1 = sxy / sxx
hat_beta0 = bar_y - hat_beta1*bar_x

rss = sum((y-hat_beta0-hat_beta1*x)^2)

se_beta0 = sqrt((rss/(n-2)) * (1/n + bar_x^2/((n-1)*sxx)))
se_beta1 = sqrt((rss/(n-2)) * (1/((n-1)*sxx)))

mu = qt(.975,n-2)

b0_lower = hat_beta0 - se_beta0 * mu
b0_upper = hat_beta0 + se_beta0 * mu
b1_lower = hat_beta1 - se_beta1 * mu
b1_upper = hat_beta1 + se_beta1 * mu

y_x15_lower = b0_lower + b1_lower * 15
y_x15_upper = b0_upper + b1_upper * 15

############################################################
############################################################

library(ISLR)
library(MASS)
library(class)

train = cbind(c(5,4.7,4.4,5.12,4.3,5.44))
test = cbind(4.9)
cl = factor(c('male','female','female','female','male','male'))

z = qda(train, cl)
predict(z,test)$class

knn(train,test,cl,k=3)
