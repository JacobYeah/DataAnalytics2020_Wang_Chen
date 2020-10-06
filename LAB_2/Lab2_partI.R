EPI_data <- read.csv('/Users/jacob/Documents/RPI/20Fall_Final_Semester/Data_Analytics/DataAnalytics2020_Wang_Chen/LAB_2/2010EPI_data.csv', skip = 1)
EPI_data
attach(EPI_data)


summary(EPI)
summary(DALY)

help("fivenum")

fivenum(EPI, na.rm = TRUE)
fivenum(DALY, na.rm = TRUE)

#Histogram
hist(EPI)
hist(DALY)
#Boxplot
boxplot(EPI)

boxplot(DALY)
#qqplot
qqplot(EPI, DALY)

ENVHEALTH
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
help(lm)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
cENVH
help(c)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval='prediction')
cENV<- predict(lmENVH,NEW,interval='confidence')

#AIR_E CLIMATE

boxplot(AIR_E,CLIMATE)
lmENVH_AC <- lm(ENVHEALTH~AIR_E+CLIMATE)
lmENVH_AC
summary(lmENVH_AC)
cENVH2 <- coef(lmENVH2)

AIR_ENEW <- c(seq(5,95,5))
CLIMATENEW <- c(seq(5,95,5))
NEW2 <- data.frame(AIR_ENEW,CLIMATENEW)
pENV2 <- predict(lmENVH2,NEW2,interval = 'prediction')
cENV2 <- predict(lmENVH2,NEW2,interval = 'confidence')

cENV2



