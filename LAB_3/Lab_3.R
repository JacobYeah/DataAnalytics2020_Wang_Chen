nyt25 <-read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_3/nyt25.csv',header = T)
nyt26 <-read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_3/nyt26.csv',header = T)
nyt27 <-read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_3/nyt27.csv',header = T)
nyt28 <-read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_3/nyt28.csv',header = T)
nyt29 <-read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_3/nyt29.csv',header = T)

boxplot(nyt25$Age,nyt26$Age,nyt27$Age,nyt28$Age,nyt29$Age)
boxplot(nyt25$Gender,nyt26$Gender,nyt27$Gender,nyt28$Gender,nyt29$Gender)
par(mfrow=c(3,2))
hist(nyt25$Age, xlab = "Age",breaks=seq(0,130,5))
hist(nyt26$Age, xlab = "Age",breaks=seq(0,130,5))
hist(nyt27$Age, xlab = "Age",breaks=seq(0,130,5))
hist(nyt28$Age, xlab = "Age",breaks=seq(0,130,5))
hist(nyt29$Age, xlab = "Age",breaks=seq(0,130,5))

hist(nyt25$Impressions, xlab = "Impressions",breaks=seq(0,130,5))
hist(nyt26$Impressions, xlab = "Impressions",breaks=seq(0,130,5))
hist(nyt27$Impressions, xlab = "Impressions",breaks=seq(0,130,5))
hist(nyt28$Impressions, xlab = "Impressions",breaks=seq(0,130,5))
hist(nyt29$Impressions, xlab = "Impressions",breaks=seq(0,130,5))


par(mfrow=c(3,2))
plot(ecdf(nyt25$Age),xlab="Age",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt26$Age),xlab="Age",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt27$Age),xlab="Age",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt28$Age),xlab="Age",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt29$Age),xlab="Age",do.points=FALSE, verticals=TRUE)

par(mfrow=c(3,2))
plot(ecdf(nyt25$Impressions),xlab="Impressions",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt26$Impressions),xlab="Impressions",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt27$Impressions),xlab="Impressions",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt28$Impressions),xlab="Impressions",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt29$Impressions),xlab="Impressions",do.points=FALSE, verticals=TRUE)

qqplot(nyt25$Age,nyt25$Impressions)
qqplot(nyt26$Age,nyt26$Impressions)
qqplot(nyt27$Age,nyt27$Impressions)
qqplot(nyt28$Age,nyt28$Impressions)
qqplot(nyt29$Age,nyt29$Impressions)

t.test(nyt27$Age,nyt28$Age)

nyt25_q2<- subset(nyt25, Gender<1&Age>5)
nyt26_q2<- subset(nyt26, Gender<1&Age>5)
par(mfrow=c(1,2))
hist(nyt25_q2$Age,xlab="Age", breaks=seq(0,130,5))
hist(nyt26_q2$Age,xlab="Age", breaks=seq(0,130,5))

par(mfrow=c(1,2))
hist(nyt26_q2$Impressions,xlab="Impressions",breaks=seq(0,19,1))
hist(nyt26_q2$Impressions,xlab="Impressions",breaks=seq(0,19,1))

par(mfrow=c(2,1))

plot(ecdf(nyt25_q2$Age),xlab="Age",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt26_q2$Age),xlab="Age",do.points=FALSE, verticals=TRUE)


par(mfrow=c(2,1))
plot(ecdf(nyt25_q2$Impressions),xlab="Impressions",do.points=FALSE, verticals=TRUE)
plot(ecdf(nyt26_q2$Impressions),xlab="Impressions",do.points=FALSE, verticals=TRUE)

par(mfrow=c(2,1))
qqplot(nyt25_q2$Age,nyt25_q2$Impressions)
qqplot(nyt26_q2$Age,nyt26_q2$Impressions)


t.test(nyt25_q2$Impressions,nyt26_q2$Impressions)
