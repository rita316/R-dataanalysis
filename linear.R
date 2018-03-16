
## Linear Regression & the Bootstrap
## dataset - cats

################## Part 1: Linear Regression Concepts #######################

## These questions do not require coding but will explore some important concepts.

## "Regression" refers to the simple linear regression equation:
##    y = b0 + b1*x
## This homework will not discuss other models.

## 1. (1 pt)
## What is the interpretation of the coefficient B1? 
## (What meaning does it represent?)

## Your answer here

# The lope of the linear regression, the change of y for per unit change of x.

## 2. (1 pt)
## Outliers are problems for many statistical methods, but are particularly problematic
## for linear regression. Why is that? It may help to define what outlier means in this case.
## (Hint: Think of how residuals are calculated)

## Your answer here

# Outliers are extreme values in the data. They are problematic for linear regression because 
# with outliers, the Residual sum of square will increase greatly. My linear regression line will skew
# toward the outlier to minimize the impact of that squared residual.
# 

## 3. (1 pt)
## How could you deal with outliers in order to improve the accuracy of your model?

## Your answer here
# delete them if they're not important to data.


################## Part 2: Sampling and Point Estimation #####################

## The following problems will use the cats dataset and explore
## the average body weight of female cats.

## Load the data by running the following code


library(MASS)
data(cats)

## 4. (2 pts)
## Subset the data frame to ONLY include female cats.

## Your answer here

newcats =  subset(cats, cats$Sex == "F")

## Use the sample function to generate a vector of 1s and 2s that is the same
## length as the subsetted data frame you just created. Use this vector to split
## the 'Bwt' variable into two vectors, Bwt1 and Bwt2.

## IMPORTANT: Make sure to run the following seed function before you run your sample
## function. Run them back to back each time you want to run the sample function to ensure 
## the same seed is used every time.

## Check: If you did this properly, you will have 24 elements in Bwt1 and 23 elements
## in Bwt2.

set.seed(676)
## Your answer here
sample1 = sample(x = c(1,2), size = nrow(newcats), replace = TRUE)

Bwt1 = newcats$Bwt[sample1 == 1]
Bwt2 = newcats$Bwt[sample1 == 2]

## 5. (3 pts)
## Calculate the mean and the standard deviation for each of the two
## vectors, Bwt1 and Bwt2. Use this information to create a 95% 
## confidence interval for your sample means (you can use the following formula 
## for a confidence interval: mean +/- 2 * standard deviation). 
## Compare the confidence intervals -- do they seem to agree or disagree?

## Your answer here
z = 1.96
CIBwt1 = c(mean(Bwt1) - z * sd(Bwt1)/sqrt(length(Bwt1)), mean(Bwt1) + z * sd(Bwt1)/sqrt(length(Bwt1)))
CIBwt2 = c(mean(Bwt2) - z * sd(Bwt2)/sqrt(length(Bwt2)), mean(Bwt2) + z * sd(Bwt2)/sqrt(length(Bwt2)))




## 6. (4 pts)
## Draw 1000 observations from a standard normal distribution. Calculate the sample mean.
## Repeat this 500 times, storing each sample mean in a vector called mean_dist.
## Plot a histogram of mean_dist to display the distribution of your sample mean.
## How closely does your histogram resemble this normal distribution? Explain.

## Your answer here

mean_dist = replicate(500, mean(rnorm(1000)))
hist(mean_dist, xlab = "Mean", ylab = "Frequency")
# Centerd at 0. slightly skewed to left, almost symmetric.

## 7. (3 pts)
## Write a function that implements Q5.
reps = rnorm
n = numeric()
reps = numeric()
HW.Bootstrap=function(distn,n,reps){
  set.seed(666) 
  
  ### Your answer here
  (replicate(reps, mean(distn(n))))
  
}
hist(HW.Bootstrap(rnorm, 1000, 500))

## Use the function you write to repeat the experiment in Q5 but instead of the
## normal distribution as we used above, use an exponential distribution with mean 1.
## Check your histogram and write out your findings.
## (Hint: HW.Bootstrap(rexp,n,reps))

## Your answer here
HW.Bootstrap(rexp, 1000, 500)
hist(HW.Bootstrap(rexp, 1000, 500))



################### Part 3: More Linear Regression ######################

## This problem will use the Prestige dataset.
## Load the data by running code below

library(car)
data(Prestige)

## We will focus on this two variables:
## income: Average income of incumbents, dollars, in 1971. 
## education: Average education of occupational incumbents, years, in 1971

## Before starting this problem, we will declare a null hypthosesis that
## education has no effect on income .
## That is: H0: B1 = 0
##          HA: B1 != 0
## We will attempt to reject this hypothesis by using a linear regression

## 8. (2 pt)
## Fit a linear regression using of Prestige data using education to predict
## income, using lm(). Examine the model diagnostics using plot(). Would you 
## consider this a good model or not? Explain.

## Your answer here
lmedu_inc = lm(Prestige$income~Prestige$education)
plot(lmedu_inc)

## 9. (2 pts)
## Using the information from summary() on your model (the output from the lm() command), create a 
## 95% confidence interval for the coefficient of education variable 

## Your answer here
summary(lmedu_inc)

ci = c(898.8 - 1.96*127, 898.8 + 1.96*127 )
ci
confint(lmedu_inc)
## 10. (2 pts)
## Based on the result from question 9, would you reject the null hypothesis or not?
## (Assume a significance level of 0.05). Explain.

## Your answer here

# Because we have 95% confident that the b1 fall between 649.88 and 1147.72, we reject the null hypothesis.



## 11. (1 pt)
## Assuming that the null hypothesis is true. 
## Based on your decision in the previous question, would you be committing a decision error? 
## If so, which type of error?

## Your answer here
# If null is true and we rejected it, we're comitting a Type 1 error.



## 12. (1 pt)
## Discuss what your regression results mean in the context of the data.
## (Hint: Think back to Question 1)

## Your answer here
# When education is 0, income is approximately at -2853.6. For per unit change of education,
# income change 898.6.







