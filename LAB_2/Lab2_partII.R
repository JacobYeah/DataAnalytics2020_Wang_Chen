# Regression 
r_data <- read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_2/dataset_multipleRegression.csv')
r_data
attach(r_data)
#UNEM=7% and HGRAD=90,000.

lmROLL <- lm(ROLL~UNEM+HGRAD)
predict(lmROLL,data.frame(UNEM=0.07,HGRAD=90000))

#INC=$25,000
lmROll2 <- lm(ROLL~UNEM+HGRAD+INC)
predict(lmROll2,data.frame(UNEM=0.07,HGRAD=90000,INC = 25000))



#Classification
abalone_data <- read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_2/abalone.csv')
abalone_data
colnames(abalone_data) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
summary(abalone_data)

abalone_data$rings
abalone_data$rings <- as.numeric(abalone_data$rings)
abalone_data$rings <- cut(abalone_data$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone_data$rings <- as.factor(abalone_data$rings)
summary(abalone_data$rings)
aba <- abalone_data
aba$sex <- NULL
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7],normalize))
summary(aba$shucked_wieght)
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2981)
k <- 55
library(class)
KNNpred <- knn(train=KNNtrain[1:7],test=KNNtest[1:7],cl=KNNtrain$rings,k)
table(KNNpred)
KNNpred



#cluster
library(ggplot2) # we will use ggplot2 to visualize the data.
head(iris) # first 6 rows of the 
str(iris) 
summary(iris) 
iris_new <- iris[,1:4]
help("sapply")
sapply(iris[,-5], var)
summary(iris)
set.seed(300)
k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(iris_new,k,nstart = 20,iter.max = 1000)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris_new,3,nstart = 20)
table(iris[,5],icluster$cluster)

## Dplyr Exercise 

EPI_data <- read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_2/2010EPI_data.csv', skip = 1)
EPI_data
attach(EPI_data)
install.packages('dplyr')
library(dplyr)
attach(EPI_data)
new_data <- data.frame(EPI_data$EPI,EPI_data$DALY)
new_data
help("sample_n")
sample_n(new_data, 5, replace = TRUE)
sample_frac(new_data,0.1)
new_decs_EPI <- arrange(data.frame(EPI_data$EPI),desc(EPI_data$EPI))
new_decs_DALY <- arrange(data.frame(EPI_data$DALY),desc(EPI_data$DALY))
double_EPI <- EPI_data$EPI *2 
EPI = mutate(EPI_data,double_EPI)
double_DALY <- EPI$DALY *2
mutate(EPI,double_DALY)
summarise(EPI, avg_EPI = mean(EPI, na.rm = TRUE))
summarise(EPI, avg_DALY = mean(DALY, na.rm = TRUE))


aaa <- EPI$EPI
mean(aaa,na.rm = TRUE)
