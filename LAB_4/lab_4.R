install.packages("readxl")
library("readxl")
queens <-read_excel("/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_4/rollingsales_queens.xls")
names(queens) <- as.matrix(queens[4, ])
queens <- queens[-1, ]
queens[] <- lapply(queens, function(x) type.convert(as.character(x)))
colnames(queens)<-
  c("borough","neighborhood","buildingclasscategory","taxclassatpresent","block","lot","easement","buildingclassatpresent","address","apratmentnumber","zipcode","residentialunits","commercialunits","totalunits","landsquarefeet","grosssquarefeet","yearbuilt","taxclassattimeofsale","buildingclassattimeofsale","saleprice","saledate")
summary(queens)
queensA=data.frame(queens$totalunits,queens$grosssquarefeet,queens$landsquarefeet,queens$yearbuilt,queens$taxclassattimeofsale,queens$saleprice)
boxplot(as.numeric(queensA$queens.grosssquarefeet),as.numeric(queensA$queens.landsquarefeet) )

queensA = queensA[which(as.numeric(queensA$queens.saleprice)>1),]
as.numeric(queensA$queens.saleprice)
library(ggplot2)
ggplot(data=queensA)+geom_histogram(mapping = aes(x=queens.saleprice),bins = 10,stat="count")
summary(queensA$queens.saleprice)
queensB = queensA[which(as.numeric(queensA$queens.saleprice)<1000000),]
boxplot(as.numeric(queensB$queens.saleprice))
hist(as.numeric(queensB$queens.saleprice))
summary(queens)
queensA$queens.saleprice<-as.numeric(queensA$queens.saleprice)
queensA<- na.omit(queensA)
queensA = queensA[which(queensA$queens.saleprice>1),]
queensB = queensA[which(queensA$queens.saleprice<1000000),]

queensC = queens[which(as.numeric(queens$saleprice)>1&as.numeric(queens$totalunits)<5),]
queensC_1<-dim(queensC)[1]

sampling.rate=0.8
num.test.set.labels=queensC_1*(1.-sampling.rate)
training <-sample(1:queensC_1,sampling.rate*queensC_1, replace=FALSE)
summary(queensC)
train<-subset(queensC[training,],select=c(totalunits,block,lot))
testing<-setdiff(1:queensC_1,training)
test<-subset(queensC[testing,],select=c(totalunits,block,lot))
cg<-queensC$taxclassattimeofsale[training]
true.labels<-queensC$taxclassattimeofsale[testing]

library(class)
classif<-knn(train,test,cg,k=5)
help(knn)
classif
attributes(.Last.value)

table(classif,true.labels)
correct = sum(as.numeric(true.labels)==as.numeric(classif))/nrow(test)
correct
help(table)

attach(queensB)
lmsaleprice<-lm(queens.saleprice~queens.totalunits+queens.grosssquarefeet+queens.landsquarefeet)
lmsaleprice
summary(lmsaleprice)
csaleprice<-coef(lmsaleprice)
csaleprice
