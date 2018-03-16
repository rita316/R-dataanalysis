# Simulation & Regex
################################### Part 1: Simulation in R (15 pts) ##############################################

## We will use the simulation techniques (Monte Carlo) introduced in class to generate confidence intervals for our estimates of distribution mean


#(a). As we will generate random numbers please set the seed using your classID. This will help with reproducibility. (1 pts)
set.seed(35)
#(b)  For this simulation problem, we will sample data from the binomial distribution with parameters n and p. 

# First, we will estimate an individual experiment.

##(1) Generate m = 100 observations of test data, with n = 10 and p = 0.8 and name it test_sample. (1 pts)

test_sample = rbinom(100, size = 10, prob = .8)

##(2) What is your estimate mean X_bar? (1 pts)
X_bar = mean(test_sample)
X_bar
##(3) What is the confidence interval for X_bar? (Recall HW4 Q5) (1 pts)
X_se =  sd(test_sample)/sqrt(100)
c(lower = X_bar - 1.96 * X_se, upper = X_bar + 1.96 * X_se)

#(c) Now use the simulation to estimate the distribution of X_bar and create confidence intervals for it using that distribution.

##(1) Form a set of X_bar by repeating B = 1000 times the individual experiment. You may want to create a matrix to save those values.(1 pts)
B = 1000
X_bar_1k = t(replicate(B, (rbinom(100, 10, .8))))
##(2) Get a estimate for mean X_bar for each experiment in (c)(1) and save it to a vector X_bar_estimate(length B vector).(1 pts)
X_bar_estimate = rowMeans(X_bar_1k)
X_bar_estimate
##(3) Now use X_bar_estimate to create a sampling distribution for X_bar, using the hist function to show the distribution.(Recall HW5 graphing techniques)
## Does the distribution look normal? (2 pts)
hist(X_bar_estimate)
##(4) Now as we have a simulated sampling distribution of X_bar, we could calculate the standard error using the X_bar_estimate. 
## What is your 95% confidence interval?(2 pts)
se_X_bar_estimate =  sd(X_bar_estimate)
CI_X_bar_estimate = c(lower =  mean(X_bar_estimate) - se_X_bar_estimate, upper = mean(X_bar_estimate) + se_X_bar_estimate)
CI_X_bar_estimate
#(d) We made some decisions when we used the simulation above that we can now question. 
# Repeat the above creation of a confidence interval in (c) for a range of settings values
# (we had our sample size fixed at 100) and a range of bootstrap values (we had B 
# fixed at 1000). Suppose the sample size varies (100, 200, 300, .... , 1000) and 
# B varies (1000, 2000, ... , 10000). You will likely find it useful to write
# functions to carry out these calculations. Your final output should be 
# upper and lower pairs for the confidence intervals produced using the bootstrap
# method for each value of sample size and B.

# generalize (c) into a function, and vary inputs of sample size and B as we did above. (2 pts)

simulation =  function(sample_size, B){
  X_bar_B = t(replicate(B, (rbinom(sample_size, 10, .8))))
  X_bar_estimate = rowMeans(X_bar_B)
}
MC_sample =  function(sample_size, B){
  mc_s =  numeric(B)
  for(i in 1:B){
    mc_s[i] =  mean(rbinom(sample_size, 10, 0.8))
  }
  return(mc_s)
}
  
#code here
sample_size = seq(100, 1000, 100)
B = seq(1000, 10000, 1000)
sample_1 = lapply(B, function(l)sapply(sample_size, simulation, l))
sample_2 =  lapply(B, function(l)sapply(sample_size, MC_sample,l))
sample_2[[1]]
str(sample_2)
#(e).Plot your CI limits to compare the effect of changing the sample size and 
# changing the number of simulation replications B (2 plots). What do you conclude? (3 pts)

################################### Part 2: Regular Expression(Regular Expression or R) (22 pts) ##############################################

#(a) Write down a general regular expression to match the following:(General Regular Expression)
string =c('abc','def', 'sas', 'star', 'ears', 'xxx@illinois.edu', 'hi@gmail.com', 'no@il.gov',
               '<a>hi</a>','<b>xxxxojioehrjkjkfbana</b>')
##(1) Words/tokens only have 's' as start or end. For example, stats, specifies, start, ends etc.(1 pts)
grep('^s',string, value =TRUE)
##(2) A string with the format <a>text</a>, <b>xxx</b> etc.(1 pts)
grep('<.>.*</.>', string, value = TRUE)
##(3) An email address that ends with .com, .edu, .net, .org, or .gov(1 pts)
grep('.+@.+\\.(edu|net|org)', string, value = TRUE)
#(b) Carry out the following exercises on the State of the Union Speeches dataset(available in moodle, stateoftheunion1790-2012.txt). (R)
# (Suggestion: check the .txt data before coding the solutions and also lapply could be really helpful)

##(1) Use readLines() to read in the speeches where the return value is: character vector with one element/character string per line in the file save as su_data (1 pts)

##(2) Use regular expressions and R to return the number of speeches in the dataset, and the number of presidents that gave speeches.(2 pts)

##(3) Use regular expressions to identify the date of the speech (save as su_date), extract the name of the speaker (save as su_speaker)
## extract the year (save as su_year) and the month of the date (save as su_month) (4 pts)

##(4) Merge the lines up into a list named su_speeches. Each element of the list is a character vector containing one speech. 
## The length of su_speeches should be the number of speeches in the data. Check: does the length of your list match your answer above? (3 pts)

##(5) Eliminate apostrophes, numbers, and the phrase: (Applause.) and make all the characters lowercase for each element in su_speeches. (3 pts)

##(6) Split the speeches in su_speeches by blanks, punctuations. Drop any empty words that resulted from this split. 
## Save the result to another list su_tokens.Each element in the su_tokens should be a vector of words in the speeches.(2 pts)

##(7) Based on su_tokens, create a list called su_frequency to calculate the token frequency for each token in each speech.(1 pts)

##(8) Carry out some exploratory analysis of the data and term frequencies. For example, find the number of sentences, extract the
## long words, and the political party. Plot and interpret the term frequencies. What are your observations? (3 pts)
