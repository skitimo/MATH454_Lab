######################################################
######################################################

q1_cv = function(x,y) {
  n = dim(x)[1]
  hat_beta = solve(t(x) %*% x) %*% t(x) %*% y
  hat_y = x %*% hat_beta
  S = x %*% solve(t(x) %*% x) %*% t(x)
  return( 1/n * sum( ((y-hat_y)/(1-diag(S)))^2 ) )
}

odor_data0 = read.table("/Users/skitimoon/Dropbox/2018_1SP/MATH454/Lab02/odor.txt",header=T)
odor_data = data.matrix(odor_data0)
ones = rep(1,length(odor_data[,1]))
x1 = odor_data[,2]
x2 = odor_data[,3]
x3 = odor_data[,4]
x_model1 = c(ones, x1, x2, x3, x1^2, x2^2, x3^2)
x_model1 = matrix(x_model1, nrow = length(x1), ncol = 7, byrow = FALSE)
x_model2 = c(ones, x2, x3, x1^2, x2^2)
x_model2 = matrix(x_model2, nrow = length(x1), ncol = 5, byrow = FALSE)
y = odor_data[,1]

summary(lm(y~x_model1))
summary(lm(y~x_model2))

MSE1 = q1_cv(x_model1,y)
MSE2 = q1_cv(x_model2,y)
MSE1
MSE2

######################################################
######################################################

library(MASS)
set.seed(1)
n = dim(Boston)[1]
B = 1000

# answer for (a): full-value property-tax rate per \$10,000.

Boston_median_tax = median(Boston$tax)

Se = function(X, B) {
  return( median(X) )
}

result = c()
for (i in 1:1000) {
  X = sample(Boston$tax,n,replace = TRUE)
  result = c(result, Se(X, B))
}
se_mu = sqrt( 1/(B-1) * sum( (result - mean(result))^2 ) )

######################################################
######################################################

Crab = read.table("/Users/skitimoon/Dropbox/2018_1SP/MATH454/Lab02/crab.txt",header=T)
colnames(Crab) = c("Obs","C","S","W","Wt","Sa")
Expectation = glm(Crab$Sa~1+Crab$W+Crab$Wt, family=poisson(link=log))
summary(Expectation)
Expectation

######################################################
######################################################

Sales = read.table("/Users/skitimoon/Dropbox/2018_1SP/MATH454/Lab02/ToyotaSales.txt",header=T)
Camry = Sales$CamrySales
Cruiser = Sales$FJCruiserSales
Cruiser = Cruiser[-c(1,2,3,4)]
Cruiser = as.numeric(paste(Cruiser))


T79=1:15
result = nlsLM(Camry ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /(1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=sum(Camry)*1000, P=0.5, Q=0.65))
summary(result)

T79=1:11
result = nls(Cruiser ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /(1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=sum(Cruiser), P=0.5, Q=0.65))
summary(result)



