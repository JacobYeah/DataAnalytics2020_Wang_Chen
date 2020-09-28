EPI_data <- read.csv(file.choose(), skip = 1)
attach(EPI_data)

EPI
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")

qqline(EPI)
boxplot(EPI_data$EPI,EPI_data$DALY)

multivariate <-read.csv(file.choose())
View(multivariate)
head(multivariate)
attach(multivariate)
help(lm)
mm <-lm(Homeowners~Immigrant)
mm
summary(mm)
plot(Homeowners~Immigrant)
help("abline")
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrant = c(0,  20))

plot(mtcars$wt, mtcars$mpg)
