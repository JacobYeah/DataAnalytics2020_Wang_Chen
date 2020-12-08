wine <- read.csv(file.choose())
head(wine)
wine$fixed.acidity
hist(wine$fixed.acidity)
require(ggplot2)

wine$poor <- wine$quality <= 4
wine$okay <- wine$quality == 5 |wine$quality == 6
wine$good <- wine$quality >= 7

summary(wine)


col(wine)
library(tidyr)
ggplot(gather(wine, cols, value), aes(x = value)) + 
  geom_bar() + facet_grid(.~cols)

ggplot(gather(wine), aes(value)) + 
  geom_bar() + 
  facet_wrap(~key, scales = 'free_x')

summary(wine$quality)
table(wine$quality)

bank <- read.csv(file.choose(),sep = ";")
head(bank)


linear_quality = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar
                    +chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+
                      alcohol, data=wine)
summary(linear_quality)


linear_quality_1 = lm(quality ~ alcohol, data =wine)
summary(linear_quality_1)

linear_quality_4 = lm(quality ~ alcohol + volatile.acidity + citric.acid + sulphates, data = wine)
summary(linear_quality_4)



plot(wine$alcohol,wine$quality)
points(wine$alcohol,predict(linear_quality_4,redwine),col="blue")
points(wine$alcohol,predict(linear_quality_1,redwine),col="red")





knn10 = knn.reg(train=wine[,1:11], test=wine[,1:11], y=wine$quality, k =10) 
knn20 = knn.reg(train=wine[,1:11],test=wine[,1:11], y = wine$quality, k=20)
plot(wine$alcohol,wine$quality)
points(wine$alcohol,knn10$pred,col="red")
points(wine$alcohol,knn20$pred,col="blue")


library(rpart) 
tree <- rpart(good ~ alcohol + sulphates, data = wine, method="class")
summary(tree)
install.packages("rpart")
library(rpart.plot) 
install.packages("caret")
library(caret)
rpart.plot(tree)
pred <- predict(tree,newdata=wine,type="class")

class_knn10 = knn(train=wine[,1:11], test=wine[,1:11], cl=wine$good, k =10) 
class_knn20 = knn(train=wine[,1:11],test=wine[,1:11], cl = wine$good, k=20)
table(wine$good,class_knn10)
table(wine$good,class_knn20)

library(readr)
install.packages("gmodels")
library(gmodels)
bank <- read.csv(file.choose(),sep = ";")
head(bank)

names(bank)
bank <- as.data.frame(bank)
summary(bank)

ggplot(gather(bank), aes(value)) + 
  geom_bar() + 
  facet_wrap(~key, scales = 'free_x')
summary(bank$y)
CrossTable(bank$y)


bank[,21]=gsub("yes",1,bank[,21])
bank[,21]=gsub("no",0,bank[,21])
lm1 <- lm(bank$y ~ bank$age +bank$duration  +bank$pdays +bank$previous  +bank$emp.var.rate
          +bank$cons.price.idx +bank$cons.conf.idx)
summary(lm1)


sr <- 0.5
bank1 <- dim(bank)[1]
test.labels <- ceiling((1-sr)*bank1)
set.seed(2019)
training <- sample(1:bank1,sr*bank1,replace=F)
trains <- subset(bank[training,],select = c(age,duration,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,y))
train <- subset(bank[training,],select = c(age,duration,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,y))
testing <- sample(1:bank1,test.labels,replace = F)
test <- subset(bank[testing,],select = c(age,duration,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,y))
lm2 <- lm(trains$y ~ trains$age + trains$duration + trains$pdays + trains$previous + trains$emp.var.rate + trains$cons.price.idx)
summary(lm2)



accurate <- predict(lm2,test,interval="prediction")
head(accurate)
coef(lm2)

accurate.test <- as.data.frame(bank$y[testing])
accurate <- cbind(accurate.test,round(accurate))
names(accurate) <- c("trueaccurate","fit","lwrBound","uprBound")
head(accurate)

test.accuracy.rate <- count(accurate[which(accurate$trueaccurate == accurate$fit),])/dim(accurate)
test.accuracy.rate*100



install.packages('kknn')
install.packages('igraph')
library(kknn)
library(class)
sprt <- 0.5
test.labels <- (1-sprt)*bank1
set.seed(2222)
training <- sample(1:bank1,sprt*bank1,replace = F)
train <- subset(bank[training,],select = c(age,duration,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,y))
testing <- sample(1:bank1,test.labels,replace = F)
test <- subset(bank[testing,],select = c(age,duration,pdays,previous,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,y))
cg <- bank$y[training]
true.label <- bank$y[testing]
solution <- knn(train,test,cg,k=4)

solution <- as.numeric(as.character(solution))
answer <- as.data.frame(cbind(bank$y[testing],solution))
names(answer) <- c("true","knn")
head(answer)

accuracy <- count(answer[which(answer$true == answer$knn),])/dim(answer)[1]
accuracy*100
View(answer)
class(answer$true)
class(answer$knn)


names(bank)
mbank <- bank[,c(1,11,13,14,16,17,18,19,21)]
str(mbank)
View(bank)
View(mbank)
View(mbank[training,])

library(rpart)
library(rpart.plot)
bankModel1 <- rpart(y ~ .,data=mbank[training,],method="class")
bankModel1

rpart.plot(bankModel1,type = 3,fallen.leaves=T)

fit <- predict(bankModel1,mbank[testing,-21],type = "class")
tree.result <- as.numeric(as.character(fit))

test <- as.data.frame(test)

true.result <- as.numeric(as.character(test[,9]))
tree.out <- as.data.frame(cbind(true.result,tree.result))


accuracy_ <- (count(tree.out[which(tree.out$true.result == tree.out$tree.result),]))/dim(tree.out)[1]
accuracy_*100

