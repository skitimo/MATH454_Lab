
CompanyBill=read.table("/Users/skitimoon/Dropbox/2018_1SP/MATH454/Lab/Lab03/CompanyBill.txt",header = TRUE)

dim(CompanyBill)
CompanyBill = CompanyBill[-which(CompanyBill==0),]
dim(CompanyBill)

colnames(CompanyBill) = c("V1","V2","V3","V4","V5","V6","V7")

#################################################################################
#################################################################################

regfit.full=regsubsets(V1~.,CompanyBill)
reg.summary=summary(regfit.full)

par(mfrow =c(2,2))

plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS", type="l")
a1 = which.min(reg.summary$rss)
points(a1, reg.summary$rss[a1], col ="red",cex =2, pch =20)

plot(reg.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")
a2 = which.max(reg.summary$adjr2)
points(a2, reg.summary$adjr2[a2], col ="red",cex =2, pch =20)

plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp", type="l")
a3 = which.min(reg.summary$cp)
points(a3, reg.summary$cp[a3], col ="red",cex =2, pch =20)

plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l")
a4 = which.min(reg.summary$bic)
points(a4, reg.summary$bic[a4], col ="red",cex=2,pch =20)

coef(regfit.full,a1)

#################################################################################
#################################################################################

regfit.fwd=regsubsets(V1~.,CompanyBill,method="forward")
reg.summary=summary(regfit.fwd)

par(mfrow =c(2,2))

plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS", type="l")
a1 = which.min(reg.summary$rss)
points(a1, reg.summary$rss[a1], col ="red",cex =2, pch =20)

plot(reg.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")
a2 = which.max(reg.summary$adjr2)
points(a2, reg.summary$adjr2[a2], col ="red",cex =2, pch =20)

plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp", type="l")
a3 = which.min(reg.summary$cp)
points(a3, reg.summary$cp[a3], col ="red",cex =2, pch =20)

plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l")
a4 = which.min(reg.summary$bic)
points(a4, reg.summary$bic[a4], col ="red",cex=2,pch =20)

coef(regfit.full,a1)

#################################################################################
#################################################################################

par(mfrow =c(1,1))
set.seed(1)
train=sample(1:nrow(CompanyBill), nrow(CompanyBill)/2)
test=(-train)
x=model.matrix(V1~.,CompanyBill)[,-1]
summary(x)
y=CompanyBill$V1

cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min

ridge.mod=glmnet(x,y,alpha=0,lambda=bestlam)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y[test])^2)

#################################################################################
#################################################################################

par(mfrow =c(1,1))
set.seed(1)
train=sample(1:nrow(CompanyBill), nrow(CompanyBill)/2)
test=(-train)
x=model.matrix(V1~.,CompanyBill)[,-1]
summary(x)
y=CompanyBill$V1

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

lasso.mod=glmnet(x,y,alpha=1,lambda=bestlam)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y[test])^2)

#################################################################################
#################################################################################




