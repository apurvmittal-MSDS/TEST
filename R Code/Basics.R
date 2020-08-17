install.packages("tidyverse")
library(tidyverse)
age = c(22,21,24,19,20,23)
age
age[2]

years_math_ed= c(4,5,2,5,3,5)

#data frames
df1 = data.frame(Age=age, Years = years_math_ed)
df1
df1[4,2] #Data in 4th row and 2nd column
df1[1,] #Data in first row and all the columns
df1[,2] #Data in Second columns
df1$Years #Data in Years Column

str(df1) #Datatype and other details about the data frame

a = c("Mary","Martha","Kim", "Kristen", "Amy", "Sam")
b = c("English", "Math", "Sociology", "Math", "Music","Dance")

df2 = data.frame(Name =a, Major=b)
df2

#Another Data Frame
df3 = data.frame(Age=age, Years =years_math_ed, Name=a, Major=b)
df3

#Cbind = Combine columns

df4 = cbind(df1,df2)
df4

#str and Class tell about the variables

str(df4)
class(df4$Name)
summary(df4)

#Define a new row/student
#putting number in quotes makes it a charqcter
d=c("19","4","John", "Math")

#rbind = add a new row

df5=rbind(df4,d)

#changing in to character

dfcopy = df4
dfcopy$Name = as.character(df4$Name)

str(df5)

#setting the data type

df5$Age = as.numeric(df5$Age)
df5$Years = as.numeric(df5$Years)
df5$Name = as.factor(df5$Name)
df5$Major = as.factor(df5$Major)

#Filtering Data
df5[df5$Age>21, ]

#Data Import

Example1 = read.csv("/Users/apurv/Documents/SMU/6306 - Doing Data Science/Unit - 1/Data sets/BusinessSales.csv", header = TRUE)
Example1

Example2 = read.csv(file.choose(), header = TRUE)
Example2

#Scatterplots
#plot(x,y,col,pch,type, ylab, xlab,main)
plot(Example1$ad_tv,Example1$sales,pch = 15, ylab = "Sales", xlab = "TV Advertising",main = "Sales vs Advertising")

abline(h=55, col="red", lwd = 5) # h is for height from where line starts and 'lwd' is the line width

# Using Iris dataset

summary (iris)
Irisvir = iris[iris$Species=="virginica", ]
Irisvir

plot(Irisvir$Sepal.Length, Irisvir$Petal.Length, col="blue", ylab = "Petal Length", xlab ="Sepal Length", ylim= c(0,7), xlim = c(4,8))
Irisvers = iris[iris$Species=="versicolor", ]
# Add points to existing plot and NOT create a new plot.
points(Irisvers$Sepal.Length, Irisvers$Petal.Length, col="red")

Iriset = iris[iris$Species=="setosa", ]
points(Iriset$Sepal.Length, Iriset$Petal.Length, col="green")

# Turn off the plot

dev.off()

#Histogram
hist(iris$Sepal.Length)

# Getting HELP
?hist

#BOXPLOTS

boxplot(Sepal.Length ~ Species, data=iris, main ="Boxplot test")

# Dividing the Plot # c(num rowa, num columns)

par(mfrow = c(1,2))

hist(iris$Sepal.Length)
boxplot(Sepal.Length ~ Species, data=iris, main ="Boxplot test")

#Bar Charts

barplot(df3$Years,  names.arg = df3$Name)

#MTCARS Dataset

#Scatterplot Matrix
pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Simple Scatterplot Matrix")
dev.off()
#draw a sample of mean and SD. # rnorm(<Sample>,<Mean>,<SD>)
Sample1 = rnorm(1000,0,1)
Sample1
hist(Sample1)
mean(Sample1)
sd(Sample1)

#Another method
Population = rnorm(1000000,0,1)
Sample1 = sample(Population,100,FALSE)
hist(Sample1)
mean(Sample1) # X bar
sd(Sample1) # S

#Make a Function to pass Sample Size, SD, Mean

#Define these outside the function so that it can be available globally

xBarVec = c() # Global vector to hold the sample mean
Population = rnorm(10000000,0,1) #Simulating the population

#####################################################################
#Function: xbarGenerator
#Samplesize: the size of the sample that each sample mean is based on
#number_of_samples:
#####################################################################

xbarGenerator = function(sampleSize=30, number_of_samples = 100)
{
  for (i in 1:number_of_samples)
  {
    theSample = sample(Population,sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec,xbar)
  }
  return(xBarVec)
}

xbars = xbarGenerator(30,1000)
length(xbars)
hist(xbars)


#####################################################################
####DIFFERENT METHOD####
#Function: xbarGenerator
#Samplesize: the size of the sample that each sample mean is based on
#number_of_samples:
#####################################################################

xbarGenerator2 = function(sampleSize=30, number_of_samples = 100, mean = 0, sd = 1)
{
  for (i in 1:number_of_samples)
  {
    theSample = rnorm(sampleSize, mean,sd)
    xbar = mean(theSample)
    xBarVec = c(xBarVec,xbar)
  }
  return(xBarVec)
}

xbars = xbarGenerator2(50,10000, 60,10)
length(xbars)
hist(xbars)
