#Lab2_fa1
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores
#Lab2_fa2
install.packages("Hmisc")
library(Hmisc)
AthleticsData <- spss.get("AthleticsData.sav")
attach(AthleticsData)
#
names(AthleticsData)

cor(AthleticsData)
prcomp(AthleticsData)

fit.2 <- factanal(AthleticsData,factors=2,rotation="varimax")
print(fit.2)

fit.3 <- factanal(AthleticsData,factors=3,rotation="varimax")
print(fit.3)
print(fit.3, digits = 2, cutoff = .2, sort = TRUE)

install.packages("GPArotation")
library(GPArotation)

fit <- principal(AthleticsData, nfactors=3, rotate=â€varimaxâ€)
fit # print results


# do not go past here unless you can find fa.promax.R



fit.3.promax <- update(fit.3,rotation="promax") 
colnames(fit.3.promax$loadings)<-c("Endurance","Strength","Hand-Eye") 
print(loadings(fit.3.promax), digits = 2, cutoff = .2, sort = TRUE)
AssignFactorNames <- function(fit.object,names)
{
  colnames(fit.object$promax.loadings)<-names
  colnames(fit.object$varimax.loadings)<-names
  rownames(fit.object$corr.factors)<-names
  colnames(fit.object$corr.factors)<-names
}
fit.3.Enzmann <- fa.promax(AthleticsData,factors=3, digits=2, sort=TRUE) AssignFactorNames(fit.3.Enzmann,factor.names)
fit.3.Enzmann

#Lab2_fa4
data(epi)
epi.keys <- make.keys(epi,list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,
                                     -29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                               N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,
                                   43, 45, 47, 50, 52, 55, 57),
                               L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                               I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                               S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))
scores <- scoreItems(epi.keys,epi)
N <- epi[abs(epi.keys[,"N"]) >0]
E <- epi[abs(epi.keys[,"E"]) >0]
fa.lookup(epi.keys[,1:3],epi.dictionary) #show the items and keying information
#lab1_svm1 
n <- 150 # number of data points
p <- 2 # dimension
sigma <- 1 # variance of the distribution
meanpos <- 0 # centre of the distribution of positive examples
meanneg <- 3 # centre of the distribution of negative examples
npos <- round(n/2) # number of positive examples
nneg <- n-npos # number of negative examples
# Generate the positive and negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)
# Generate the labels
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
# Visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))
#
ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1
# Visualize
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))
#lab1_svm2 
library(e1071)
library(rpart)

index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)

#lab1_svm3 
data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## try regression mode on two dimensions

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x, y)
new <- predict(m, x)

# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

## density-estimation

# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

# traditional way:
m <- svm(X, gamma = 0.1)

# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

# weights: (example not particularly sensible)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)
#lab1_svm4 
## example using the promotergene data set
data(promotergene)

## create test and training set
ind <- sample(1:dim(promotergene)[1],20)
genetrain <- promotergene[-ind, ]
genetest <- promotergene[ind, ]

## train a support vector machine
gene <-  ksvm(Class~.,data=genetrain,kernel="rbfdot",\
              kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)

## predict gene type probabilities on the test set
genetype <- predict(gene,genetest,type="probabilities")
#lab1_svm5 
library(e1071) 
m1 <- matrix( c( 
  0,    0,    0,    1,    1,    2,     1, 2,    3,    2,    3, 3, 0, 1,2,3, 
  0, 1, 2, 3, 
  1,    2,    3,    2,    3,    3,     0, 0,    0,    1, 1, 2, 4, 4,4,4,    0, 
  1, 2, 3, 
  1,    1,    1,    1,    1,    1,    -1,-1,  -1,-1,-1,-1, 1 ,1,1,1,     1, 
  1,-1,-1 
), ncol = 3 ) 

Y = m1[,3] 
X = m1[,1:2] 

df = data.frame( X , Y ) 

par(mfcol=c(4,2)) 
for( cost in c( 1e-3 ,1e-2 ,1e-1, 1e0,  1e+1, 1e+2 ,1e+3)) { 
  #cost <- 1 
  model.svm <- svm( Y ~ . , data = df ,  type = "C-classification" , kernel = 
                      "linear", cost = cost, 
                    scale =FALSE ) 
  #print(model.svm$SV) 
  
  plot(x=0,ylim=c(0,5), xlim=c(0,3),main= paste( "cost: ",cost, "#SV: ", 
                                                 nrow(model.svm$SV) )) 
  points(m1[m1[,3]>0,1], m1[m1[,3]>0,2], pch=3, col="green") 
  points(m1[m1[,3]<0,1], m1[m1[,3]<0,2], pch=4, col="blue") 
  points(model.svm$SV[,1],model.svm$SV[,2], pch=18 , col = "red") 
}
#lab1_svm6 
data(spam)

## create test and training set
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]

## train a support vector machine
filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)
filter

## predict mail type on the test set
mailtype <- predict(filter,spamtest[,-58])

## Check results
table(mailtype,spamtest[,58])
#lab1_svm7 
## Another example with the famous iris data
data(iris)

## Create a kernel function using the build in rbfdot function
rbf <- rbfdot(sigma=0.1)
rbf

## train a bound constraint support vector machine
irismodel <- ksvm(Species~.,data=iris,type="C-bsvc",
                  kernel=rbf,C=10,prob.model=TRUE)

irismodel

## get fitted values
fitted(irismodel)

## Test on the training set with probabilities as output
predict(irismodel, iris[,-5], type="probabilities")


#lab1_svm8 
## Demo of the plot function
x <- rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
y <- matrix(c(rep(1,60),rep(-1,60)))

svp <- ksvm(x,y,type="C-svc")
plot(svp,data=x)


### Use kernelMatrix
K <- as.kernelMatrix(crossprod(t(x)))

svp2 <- ksvm(K, y, type="C-svc")

svp2

#lab1_svm9 # test data
xtest <- rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=3),,2))
# test kernel matrix i.e. inner/kernel product of test data with
# Support Vectors

Ktest <- as.kernelMatrix(crossprod(t(xtest),t(x[SVindex(svp2), ])))

predict(svp2, Ktest)


#### Use custom kernel 

k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)

## train svm using custom kernel
gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)

gene


#### Use text with string kernels
data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv


## regression
# create data
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)

# train support vector machine
regm <- ksvm(x,y,epsilon=0.01,kpar=list(sigma=16),cross=3)
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")
