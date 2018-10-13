#Read the data from the location
ReadData <- read.csv("C:/Users/Sony/Desktop/Data_Science/GyanVriksh_Course-master/Assignment/Assignment/MBA Starting Salaries Data.csv")

#Attach the file to R
attach(ReadData)

hist(age) #Histogram of age

mean.age=mean(age)
mean.age
std.age=sqrt(var(age))
std.age

x=dnorm(age,mean.age,std.age)
png(file="dnorm1.png")
plot(age,x)
dev.off() # It is not a normal distribution

gmat.tot.mean=mean(gmat_tot) #Mean of gmat total
gmat.tot.median=median(gmat_tot) #Median of gmat total

std.gmat.tot=sqrt(var(gmat_tot)) #Standard deviation of gmat total

summary(gmat_tot) #Summary of gmat total

#Function for finding out Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(gmat_tot) # Mode of gmat total
x=unique(gmat_tot)
x
match(gmat_tot,x)
tabulate(match(gmat_tot,x))
max(tabulate(match(gmat_tot,x)))
which.max(tabulate(match(gmat_tot,x)))
x=unique(which.max(tabulate(match(gmat_tot,x))))
x

# Ploting a normalized graph 
y=dnorm(gmat_tot,gmat.tot.mean,std.gmat.tot)
png(file="dnorm2.png")
plot(gmat_tot,y)
dev.off()
#Mean=619.5  Median=620.0  Mode=630. Since Median ,Median,Mode is almost same . It is a normal distribution

#View the data
View(ReadData)

mba.salary<-(ReadData$salary) # Read the salary from the data set
View(mba.salary) # View the salary
mba.salary.new<-ReadData$salary[!(ReadData$salary %in% c(999,998,0))] #Removing 999,998,0 from salary
View(mba.salary.new) # View salary after filtering the data


mean.salary=mean(mba.salary.new)#Mean of Salary
std.salary=sqrt(var(mba.salary.new)) #Standard Deviation of salary


y1=dnorm(mba.salary.new,mean.salary,std.salary)
png(file="dnorm3.png")
plot(mba.salary.new,y1)
dev.off()

hist(mba.salary.new)
  # It is bimodal right tailed extreme


satisfaction.salary<-data.frame(sal=c(ReadData$salary),stat=c(ReadData$satis))
View(satisfaction.salary)
satisfaction.salary.new<-subset(satisfaction.salary,sal!=999 & sal!=998 &sal!=0 & stat!=999 & stat!=998)

View(satisfaction.salary.new)

png(file="line_chart.jpg")
plot(satisfaction.salary.new$sal,satisfaction.salary.new$stat,type="p",col="red",xlab="Salary",ylab="Satisfaction",main="satisfaction vs salary")
dev.off()
 #High Salary doesnt lead to high satisfaction.




