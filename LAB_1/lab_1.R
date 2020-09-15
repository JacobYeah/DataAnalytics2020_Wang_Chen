EPI_data <- read.csv(file.choose(), skip = 1)
View(EPI_data)

attach(EPI_data)
fix(EPI_data)

EPI
tf<-is.na(EPI)
tf
E<-EPI[!tf]
E
summary(EPI)
fivenum(EPI, na.rm = TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)

lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI); 
qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
DALY
boxplot(EPI,DALY)



help("distributions")
EPILand<-EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

