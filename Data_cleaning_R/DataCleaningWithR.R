##missing data
age <- c(23, 16, NA)
mean(age)
mean(age, na.rm = TRUE)

setwd("https://raw.githubusercontent.com/ShailyBarjatya/R-Scripts/master/Data_cleaning_R")
person<-read.delim("person",header=TRUE)
complete.cases(person)

person_complete<-na.omit(person)

install.packages("Hmisc")
library(Hmisc)
x <- 1:5  
x[2] <- NA  
x1 <- impute(x, mean)
x2 <- impute(x,median)
is.imputed(x1)


data(iris)
iris$Sepal.Length[1:10]<-NA
model<-lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
I <- is.na(iris$Sepal.Length)

install.packages("VIM")
library(VIM)
data(iris)
iris$Sepal.Length[1:10]<-NA
iris2<-kNN(iris)

iris2

data(iris)
n <- nrow(iris)
# provide some empty values (10 in each column, randomly)
for (i in 1:ncol(iris)) {
  iris[sample(1:n, 10, replace = FALSE), i] <- NA
}
iris3 <- kNN(iris)

install.packages("deducorrect")
library(deducorrect)

E <- editmatrix(expression(
  staff + cleaning + housing == total,
  staff    >= 0,
  housing  >= 0,
  cleaning >= 0
))
dat <- data.frame(
  staff = c(100,100,100),
  housing = c(NA,50,NA),
  cleaning = c(NA,NA,NA),
  total = c(100,180,NA)
)
dat
cor <- deduImpute(E,dat) 
cor$corrected

E <- editarray(expression(
  age %in% c("adult","under-aged"),
  driverslicense %in% c(TRUE, FALSE),
  if ( age == "under-aged" ) !driverslicense
))
dat <- data.frame(
  age = NA,
  driverslicense = TRUE
)
dat
cor <- deduImpute(E,dat) 
cor$corrected

##noise

#install.packages("deducorrect")
#library(deducorrect)
e <- editmatrix("x + y == z")
d <- data.frame(x = 100, y = 101, z = 200) 
cor <- correctRounding(e, d) 
cor$corrected
cor$corrections

e <- editmatrix("x + y == z")
d <- data.frame(x = 100, y = -100, z = 200) 
cor <- correctSigns(e, d)
cor$corrected
cor$corrections

e <- editmatrix("x + y == z")
d <- data.frame(x = 123, y = 132, z = 246) 
cor <- correctTypos(e, d)
cor$corrected
cor$corrections


##inconsistencies
people<-read.csv("people.txt")
install.packages("editrules")
library(editrules)
E <- editset(c("age >=0", "age <= 150"))
violatedEdits(E,people)

E<-editfile("edits.txt")

ve<-violatedEdits(E,people)
summary(ve)
plot(ve)
plot(E)

id <- c(2, 5)
people[id, ]
violatedEdits(E, people[id, ])

le <- localizeErrors(E, people[id, ], method = "mip")

people[2, "status"] <- "single"
people[5, "height"] <- 7
people[5, "agegroup"] <- "adult"
summary(violatedEdits(E, people[id, ]))

##outliers

x <- c(1:10, 20, 30)
boxplot.stats(x)$out
boxplot.stats(x,coef=2)$out




