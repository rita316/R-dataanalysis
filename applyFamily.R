
# Using the "apply" family of functions

#Q1 (5 pts)
# Given a function below,
myfunc <- function(z) return(c(z,z^2,(z^2+z)%/%2))
#(a) explain in words what myfunc is doing.
myfunc1 <- function(z) (c(z,z^2,(z^2+z)%/%2))
myfunc(2)
myfunc1(2)
#(b) Examine the following code, and briefly explain what it is doing.
y = 1:8
myfunc(y)
matrix(myfunc(y),ncol=3)

### Your explanation here



#(c) Simplify the code in (b) using one of the "apply" functions and save the result as m.
###code & result

m = t(sapply(y, myfunc))


#(d) Find the column product of m.
###code & result
apply(m, 2, prod)



#(e) Find the row sum of m in two ways.
###code & result
rowSums(m)
apply(m,1,sum)
sapply( (x = 1:8), function(x){
  sum(m[x, ])}
  )
#(f) Multiple all the values by 2 in two different ways:
### 1. code & result
m *2

### 2. code & result
m2 = matrix(2,8,3)
m*m2

apply(m, 1:2, function(x) x*2)
#Q2 (10 pts)
#Create a list l with 2 elements as follows:
l <- list(a = 1:10, b = 21:30)

#(a) What is the sum of the values in each element?
###code & result
lapply(l,sum)

#(b) What is the (sample) variance of the values in each element?
###code & result
lapply(l,var)

#(c) Use the help() function to check what type of output object will you expect if you use sapply and lapply. 
# Show your R code that finds these answers and briefly explain if the results agree with your expectation.

###code
class(lapply(l,sum)) #"list"
class(sapply(l,sum)) #"integer"
###written explanation




#(d) Change one of them to make the two statement return the same results (type of object):
###code & result
class(lapply(l,sum)) #"list"

class(sapply(l,sum,simplify=FALSE)) "list"

# Now create the following list:
l.2 <- list(c = c(11:20), d = c(31:40))
#(e) What is the sum of the corresponding elements of l and l.2, using one function call? Your result should be a 
# single vector with length 10.
###code & result
mapply(sum, l$a,l$b,l.2$c,l.2$d)

#(f) Take the log of each element in the list l:
###code & result

lapply(l, log)

#(g) First change l and l.2 into matrixes, make each element in the list as column,
###code & result

l = matrix(unlist(l), ncol = 2)
l.2 = matrix(unlist(l.2), ncol = 2)

#Then, form a list named mylist using l,l.2 and m (from Q1) (in that order).
###code & result
mylist = list(l,l.2,m)
class(mylist)
mylist
#Then, select the second column of each elements in mylist in one function call (hint '[' is the select operator).
###code & result

lapply(mylist, `[`,, 2)


#Q3 (3 pts)
# Let's load the family data again.
load(url("http://courseweb.lis.illinois.edu/~jguo24/family.rda"))
#(a) Find the mean bmi by gender in one function call.
###code & result

tapply(family$bmi, family$gender, mean)
aggregate(family$bmi, list(family$gender),mean)

#(b) Could you get a vector of what the type of variables the dataset is made of？
###code & result
sapply(family, function(x)class(x))


#(c) Could you sort the firstName in bmi descending order?
###code & result
family$firstName[order(-family$bmi)]

#Q4 (2 pts)
# There is a famous dataset in R called "iris." It should already be loaded
# in R for you. If you type in ?iris you can see some documentation. Familiarize 
# yourself with this dataset.
#(a) Find the mean petal width by species.
### code & result

tapply(iris$Petal.Width,iris$Species, mean)

#(b) Now obtain the mean of the first 4 variables, by species, but using only one function call.
### code & result

aggregate(iris[1:4], by = list(iris$Species), mean)
by(iris[,1:4],iris$Species,colMeans)

#Q5. (5 pts) Now with the "iris" data, fit a simple linear regression model using lm() to predict 
# Petal length from Petal width. Place your code and output (the model) below. 


# How do you interpret this model?


# Create a scatterplot of Petal length vs Petal width. Add the linear regression line you found above.
# Provide an interpretation for your plot.
### code & result
