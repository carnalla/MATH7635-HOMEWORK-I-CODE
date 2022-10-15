## CHAPTER 02: QUESTION 8 ##

# (a) Use the read.csv() function to read the data into R. Call the loaded data college. 
# Make sure that you have the directory set to the correct location for the data.

college = read.csv(url('https://www.statlearning.com/s/College.csv'))

# (b) Look at the data using the View() function. You should notice that the first column is just the name of each university.We don’t
# really want R to treat this as data. However, it may be handy to have these names for later. Try the following commands:

rownames(college) = college[ , 1]
View(college)

# You should see that there is now a row.names column with the name of each university recorded. This means that R has given
# each row a name corresponding to the appropriate university. R will not try to perform calculations on the row names. However,
# we still need to eliminate the first column in the data where the names are stored. Try

college = college[ , -1]
View(college)

# Now you should see that the first data column is Private. Note that another column labeled row.names now appears before the Private column. 
# However, this is not a data column but rather the name that R is giving to each row.
# (c) i. Use the summary() function to produce a numerical summary of the variables in the data set. 
# (c) ii. Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. 
# Recall that you can reference the first ten columns of a matrix A using A[,1:10].

summary(college)
college[ , 1] = factor(college[, 1])
pairs(college[, 1:10])

# iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.

boxplot(Outstate ~ Private, data = college, main = 'Outstate vs. Private', xlab = 'Private (Y/N)', ylab = 'Outstate')

# iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. We are going to divide universities
# into two groups based on whether or not the proportion of students coming from the top 10% of their high school classes exceeds 50 %.

Elite = rep('No', nrow(college))
Elite[college$Top10perc > 50] = 'Yes'
Elite = as.factor(Elite)
college = data.frame(college, Elite)

# Use the summary() function to see how many elite universities there are. 

summary(Elite)

# Now use the plot() function to produce side-by-side boxplots of Outstate versus Elite.

boxplot(Outstate ~ Elite, data = college, main = 'Outstate vs. Elite', xlab = 'Elite (Y/N', y = 'Outstate')

# v. Use the hist() function to produce some histograms with differing numbers of bins for a few of the quantitative variables.
# You may find the command par(mfrow = c(2, 2)) useful: it will divide the print window into four regions so that four plots can be made simultaneously. Modifying the
# arguments to this function will divide the screen in other ways.

summary(college$Private)
private = college[college$Private == 'Yes', ]
public = college[college$Private == 'No', ]

par(mfrow = c(2, 2))
hist(private$Outstate, breaks = 20, col = 'red', main = 'Private: Out-of-State Tuition', xlab = 'Tuition ($1000/Bin)', ylab = 'Number of Schools')
hist(private$PhD, breaks = 40, col = 'red', main = 'Private: Percent of Faculty with PhD', xlab = 'Percent (2%/Bin)', ylab= 'Number of Schools')
hist(public$Outstate, breaks = 20, col = 'blue', main = 'Public: Out-of-State Tuition', xlab = 'Tuition ($5000/Bin)', ylab = 'Number of Schools')
hist(public$PhD, breaks = 40, col = 'blue', main = 'Public: Percent of Faculty with PhD', xlab = 'Percent (2%/Bin)', ylab = 'Number of Schools')

# vi. Continue exploring the data, and provide a brief summary of what you discover.

par(mfrow = c(1, 1))
linMod = lm(perc.alumni ~ S.F.Ratio, data = college)
plot(perc.alumni ~ S.F.Ratio, data = college, main = 'Percent of Alumni Who Donate x Student-Faculty Ratio', xlab = 'Student-to-Faculty Ratio', ylab = 'Percent of Alumni who Donate')
abline(linMod, col = 'red')

##     CHAPTER 02: QUESTION 10     ##

# 10. This exercise involves the Boston housing data set.
# (a) To begin, load in the Boston data set. The Boston data set is part of the ISLR2 library.

#install.packages('ISLR2')
library(ISLR2)

# Now the data set is contained in the object Boston.

Boston

# Read about the data set:

?Boston

# How many rows are in this data set? How many columns? What do the rows and columns represent?

#(b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.

pairs(Boston)

# (c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.

round(cor(Boston, method = 'pearson'), digits = 2)

# comparison of crime rates for Charles River variable.

median(Boston[Boston$chas == 1, ]$crim) / median(Boston[Boston$chas == 0, ]$crim)

# (d) Do any of the census tracts of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.

par(mfrow = c(1, 3))
boxplot(Boston$crim, main = 'Boston Crime Rates', outcol = 'red', outlwd = 3)
boxplot(Boston$tax, main = 'Boston Full-Value Property Tax Rates', outcol = 'red', outlwd = 3)
boxplot(Boston$ptratio, main = 'Boston Pupil-Teacher Ratio', outcol = 'red', outlwd = 3)

summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)

# (e) How many of the census tracts in this data set bound the Charles river?
  
table(Boston$chas)
   
# (f) What is the median pupil-teacher ratio among the towns in this data set?

median(Boston$ptratio)
   
# (g) Which census tract of Boston has lowest median value of owner-occupied homes? 
# What are the values of the other predictors for that census tract, and how do those values compare to the overall ranges for those predictors? 
# Comment on your findings.

Boston[order(Boston$medv, decreasing = FALSE), ]
lowValueMeans = colMeans(head(Boston[order(Boston$medv, decreasing = FALSE), ], 2))
summary(Boston)

# (h) In this data set, how many of the census tracts average more than seven rooms per dwelling? More than eight rooms per dwelling? 

nrow(Boston[Boston$rm > 7, ] )
nrow(Boston[Boston$rm > 8, ] )

# Comment on the census tracts that average more than eight rooms per dwelling.

colMeans(Boston[Boston$rm > 8, ])

sum(Boston[Boston$rm > 8, ]$chas == 1) / nrow(Boston[Boston$rm > 8, ]) * 100
sum(Boston$chas == 1) / nrow(Boston) * 100

### CHAPTER 03: QUESTION 09 ###

# This question involves the use of multiple linear regression on the Auto data set.

Auto

# (a) Produce a scatterplot matrix which includes all of the variables in the data set.

pairs(Auto)

# (b) Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, which is qualitative.

autosQuant = Auto[, 1:ncol(Auto) -1]
cor(autosQuant)

# (c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. 
# Use the summary() function to print the results. Comment on the output. For instance:

autosQuant$Euro = ifelse(autosQuant$origin == 2, 1, 0)
autosQuant$Japan = ifelse(autosQuant$origin == 3, 1, 0)
autosQuant = autosQuant[, !names(autosQuant) == 'origin']
autosLinMod = lm(mpg ~ ., autosQuant)
summary(autosLinMod)

#   i. Is there a relationship between the predictors and the response?
#
# SEE ASSIGNMENT SUBMISSION
#
#   ii. Which predictors appear to have a statistically significant relationship to the response?
#
# SEE ASSIGNMENT SUBMISSION
#   
#   iii. What does the coefficient for the year variable suggest?
#
# SEE ASSIGNMENT SUBMISSION
#
# (d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit.
# Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?

par(mfrow = c(2, 2))
plot(autosLinMod)

par(mfrow = c(2, 2))
plot(predict(autosLinMod))
plot(residuals(autosLinMod))
plot(rstudent(autosLinMod))
plot(hatvalues(autosLinMod))

# SEE ASSIGNMENT SUBMISSION

# (e) Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?


summary(lm(mpg ~ . * ., data = autosQuant))
partCModel = lm(mpg ~ weight * year, data = autosQuant)
summary(partCModel)

par(mfrow = c(1, 2))
plot(mpg~weight, data = autosQuant)
plot(mpg~year, data = autosQuant)

# (f) Try a few different transformations of the variables, such as log(X), √X, X2. Comment on your findings.

pairs(autosQuant)

par(mfrow = c(1, 1))

plot(autosQuant$weight, autosQuant$mpg, main = 'Comparison of Transformations for Predictor Variable: MPG Regressed on Weight')
linerMod = lm(autosQuant$mpg ~ autosQuant$weight)
logMod = lm(autosQuant$mpg ~ log(autosQuant$weight))
quadMod = lm(autosQuant$mpg ~ I(autosQuant$weight^2))
sqrtMod = lm(autosQuant$mpg ~ sqrt(autosQuant$weight))
lines(sort(autosQuant$weight), fitted(linerMod)[order(autosQuant$weight)], lwd = 2, col = 'blue')
lines(sort(autosQuant$weight), fitted(logMod)[order(autosQuant$weight)], lwd = 2, col = 'red')
lines(sort(autosQuant$weight), fitted(quadMod)[order(autosQuant$weight)], lwd = 2, col = 'green')
lines(sort(autosQuant$weight), fitted(sqrtMod)[order(autosQuant$weight)], lwd = 2, col = 'black')
legend('topleft', legend = c('Linear Regression Line', 'Logarithmic Regression Line', 'Quadratic Regression Line', 'Square Root Regression Line'), col = c('blue', 'red', 'green', 'black'), lty = 1)

summary(linerMod)
summary(logMod)
summary(quadMod)
summary(sqrtMod)

plot(autosQuant$acceleration, autosQuant$mpg, main = 'Comparison of Transformations for Predictor Variable: MPG Regressed on Acceleration')
linerMod = lm(autosQuant$mpg ~ autosQuant$acceleration)
logMod = lm(autosQuant$mpg ~ log(autosQuant$acceleration))
quadMod = lm(autosQuant$mpg ~ I(autosQuant$acceleration^2))
sqrtMod = lm(autosQuant$mpg ~ sqrt(autosQuant$acceleration))
lines(sort(autosQuant$acceleration), fitted(linerMod)[order(autosQuant$acceleration)], lwd = 2, col = 'blue')
lines(sort(autosQuant$acceleration), fitted(logMod)[order(autosQuant$acceleration)], lwd = 2, col = 'red')
lines(sort(autosQuant$acceleration), fitted(quadMod)[order(autosQuant$acceleration)], lwd = 2, col = 'green')
lines(sort(autosQuant$acceleration), fitted(sqrtMod)[order(autosQuant$acceleration)], lwd = 2, col = 'black')
legend('topleft', legend = c('Linear Regression Line', 'Logarithmic Regression Line', 'Quadratic Regression Line', 'Square Root Regression Line'), col = c('blue', 'red', 'green', 'black'), lty = 1)

summary(linerMod)
summary(logMod)
summary(quadMod)
summary(sqrtMod)

### QUESTION 10 ###

# 10. This question should be answered using the Carseats data set.
# (a) Fit a multiple regression model to predict Sales using Price, Urban, and US.

?Carseats
Carseats

csModel = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(csModel)

# (b) Provide an interpretation of each coefficient in the model. Be careful—some of the variables in the model are qualitative!
contrasts(Carseats$Urban)
contrasts(Carseats$US)
#
# SEE ASSIGNMENT SUBMISSION
#
# (c) Write out the model in equation form, being careful to handle the qualitative variables properly.
#
# SEE ASSIGNMENT SUBMISSION
#
# (d) For which of the predictors can you reject the null hypothesis H0 : βj = 0?
#
# SEE ASSIGNMENT SUBMISSION
#
# (e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is
#     evidence of association with the outcome.
csModel2 = lm(Sales ~ Price + US, data = Carseats)
summary(csModel2)

# (f) How well do the models in (a) and (e) fit the data?
anova(csModel, csModel2)
# SEE ASSIGNMENT SUBMISSION
#
# (g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
confint(csModel2, level = 0.95)
#
#(h) Is there evidence of outliers or high leverage observations in the model from (e)?
par(mfrow = c(2, 2))
plot(csModel2)

par(mfrow = c(2, 2))
plot(predict(csModel2))
plot(residuals(csModel2))
plot(rstudent(csModel2))
plot(hatvalues(csModel2))


### QUESTION 11 ###

#Applied problems: In this problem we will investigate the t-statistic for the null hypothesis H0 : β = 0 in simple linear regression without an intercept. 
#To begin, we generate a predictor x and a response y as follows.

set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

# (a) Perform a simple linear regression of y onto x, without an intercept. Report the coefficient estimate ˆ β, the standard error of this coefficient estimate, 
# and the t-statistic and p-value associated with the null hypothesis H0 : β = 0. Comment on these results. 
# (You can perform regression without an intercept using the command lm(y∼x+0).)

questionElevenModel = lm(y ~ x + 0)
summary(questionElevenModel)

# (b) Now perform a simple linear regression of x onto y without an intercept, and report the coefficient estimate, its standard error, and the corresponding t-statistic 
# and p-values associated with the null hypothesis H0 : β = 0. Comment on these results.

questionElevenModel2 = lm(x ~ y + 0)
summary(questionElevenModel2)


# (c) What is the relationship between the results obtained in (a) and (b)?
#
# SEE ASSIGNMENT SUBMISSION
#
#(f) In R, show that when regression is performed with an intercept, the t-statistic for H0 : β1 = 0 is the same for the regression of y onto x as it is for the regression of x onto y.

summary(lm(y ~ x))
summary(lm(x ~ y))


### QUESTION 13 ###
#In this exercise you will create some simulated data and will fit simple linear regression models to it. Make sure to use set.seed(1) prior to starting part (a) to ensure consistent results.

set.seed(1)

#(a) Using the rnorm() function, create a vector, x, containing 100 observations drawn from a N(0, 1) distribution. This represents a feature, X.

x = rnorm(100)

#(b) Using the rnorm() function, create a vector, eps, containing 100 observations drawn from a N(0, 0.25) distribution—a normal distribution with mean zero and variance 0.25.

eps = rnorm(100, sd = 0.5)

#(c) Using x and eps, generate a vector y according to the model Y = −1 + 0.5X + ϵ. (3.39) What is the length of the vector y? What are the values of β0 and β1 in this linear model?

Y = -1 + 0.5 * x + eps
length(Y)

# SEE ASSIGNMENT SUBMISSION FOR PARAMETERS

#(d) Create a scatterplot displaying the relationship between x and y. Comment on what you observe. 
plot(x, Y)
# SEE ASSIGNMENT SUBMISSION

#(e) Fit a least squares linear model to predict y using x. Comment on the model obtained. How do ˆ β0 and ˆ β1 compare to β0 and β1?
ques13 = lm(Y ~ x)
summary(ques13)

# SEE ASSIGNMENT SUBMISSION

#(f) Display the least squares line on the scatterplot obtained in (d). Draw the population regression line on the plot, in a different color. Use the legend() command to create an appropriate legend. 
plot(x, Y)
abline(a = -1, b = 0.5, col = 'red')
abline(a = ques13$coefficients[1], b = ques13$coefficients[2], col = 'blue')
legend('topleft', legend = c('Population Regression Line', 'Least Squares Regression Line'), col = c('red', 'blue'), lty = 1)

#(g) Now fit a polynomial regression model that predicts y using x and x2. Is there evidence that the quadratic term improves the model fit? Explain your answer.
ques13_2 = lm(Y ~ x + I(x^2))
summary(ques13_2)

# (h) Repeat (a)–(f) after modifying the data generation process in such a way that there is less noise in the data. The model (3.39) should remain the same. 
# You can do this by decreasing the variance of the normal distribution used to generate the error term ϵ in (b). Describe your results.

x = rnorm(100)
eps = rnorm(100, sd = 0.25)
Y = -1 + 0.5 * x + eps
ques13_3 = lm(Y ~ x)
plot(x, Y)
abline(a = -1, b = 0.5, col = 'red')
abline(a = ques13$coefficients[1], b = ques13$coefficients[2], col = 'blue')
legend('topleft', legend = c('Population Regression Line', 'Least Squares Regression Line'), col = c('red', 'blue'), lty = 1)
summary(ques13_3)

# (i) Repeat (a)–(f) after modifying the data generation process in such a way that there is more noise in the data. The model (3.39) should remain the same. 
# You can do this by increasing the variance of the normal distribution used to generate the error term ϵ in (b). Describe your results.

x = rnorm(100)
eps = rnorm(100, sd = 0.75)
Y = -1 + 0.5 * x + eps
ques13_4 = lm(Y ~ x)
plot(x, Y)
abline(a = -1, b = 0.5, col = 'red')
abline(a = ques13$coefficients[1], b = ques13$coefficients[2], col = 'blue')
legend('topleft', legend = c('Population Regression Line', 'Least Squares Regression Line'), col = c('red', 'blue'), lty = 1)
summary(ques13_4)

#(j) What are the confidence intervals for β0 and β1 based on the original data set, the noisier data set, and the less noisy data set? Comment on your results.

print('Original Data, SD = 0.50, (var = 0.25)')
confint(ques13) 
print('Increased Noise Data, SD = 0.75, (var = 0.56)')
confint(ques13_4)
print('Reduced Noise Data, SD = 0.25, (var = 0.06')
confint(ques13_3) 

## QUESTION 14 ##
# 
# This problem focuses on the collinearity problem.
# (a) Perform the following commands in R:
  
set.seed (1)
x1 = runif (100)
x2 = 0.5 * x1 + rnorm (100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm (100)

# The last line corresponds to creating a linear model in which y is a function of x1 and x2. 
# Write out the form of the linear model. What are the regression coefficients?
# 
# SEE ASSIGNMENT SUBMISSION
#   
#(b) What is the correlation between x1 and x2? Create a scatterplot displaying the relationship between the variables.
plot(x1, x2)
cor(x1, x2)

# (c) Using this data, fit a least squares regression to predict y using x1 and x2. Describe the results obtained. What are ˆ β0, ˆ β1, and ˆ β2? 
#   How do these relate to the true β0, β1, and β2? Can you reject the null hypothesis H0 : β1 = 0? How about the null hypothesis H0 : β2 = 0?

q14mod = lm(y ~ x1 + x2)
summary(q14mod)
#
# SEE ASSIGNMENT SUBMISSION
#
# (d) Now fit a least squares regression to predict y using only x1. Comment on your results. Can you reject the null hypothesis H0 : β1 = 0?
q14mod2 = lm(y ~ x1)
summary(q14mod2)
#
# SEE ASSIGNMENT SUBMISSION
#
# (e) Now fit a least squares regression to predict y using only x2. Comment on your results. Can you reject the null hypothesis H0 : β1 = 0?
q14mod3 = lm(y ~ x2)
summary(q14mod3)
#
# SEE ASSIGNMENT SUBMISSION
# 
# (f) Do the results obtained in (c)–(e) contradict each other? Explain your answer.
#
# SEE ASSIGNMENT SUBMISSION
# 
#(g) Now suppose we obtain one additional observation, which was unfortunately mismeasured.
x1 = c(x1 , 0.1)
x2 = c(x2 , 0.8)
y = c(y, 6)

# Re-fit the linear models from (c) to (e) using this new data. What effect does this new observation have on the each of the models? 
# In each model, is this observation an outlier? A high-leverage point? Both? Explain your answers.

q14modb = lm(y ~ x1 + x2)
summary(q14modb)

q14mod2b = lm(y ~ x1)
summary(q14mod2b)

q14mod3b = lm(y ~ x2)
summary(q14mod3b)

par(mfrow = c(2, 2))
plot(q14modb)
rstudent(q14modb)[101]
hatvalues(q14modb)[101]

plot(q14mod2b)
rstudent(q14mod2b)[101]
hatvalues(q14mod2b)[101]

plot(q14mod3b)
rstudent(q14mod3b)[101]
hatvalues(q14mod3b)[101]

# SEE ASSIGNMENT SUBMISSION


### QUESTION 15 ###

# 15. Applied Problems: 
# This problem involves the Boston data set, which we saw in the lab for this chapter. We will now try to predict per capita crime rate using the other variables in this data set. 
# In other words, per capita crime rate is the response, and the other variables are the predictors.
library(ISLR2)
Boston
# 
# (a) For each predictor, fit a simple linear regression model to predict the response. Describe your results. 
# In which of the models is there a statistically significant association between the predictor and the response? Create some plots to back up your assertions.
# 
for (var in 2:length(names(Boston))) {
  formula = paste('crim ~ ', names(Boston)[var], sep = '')
  print(summary(lm(formula, data = Boston)))
}

par(mfrow = c(1, 3))
plot(Boston$rad, Boston$crim, xlab = 'Accessibility to Radial Highways', ylab = 'Per Capita Crime Rate', main = 'Crime Rate by Proximity to Radial Highways')
plot(Boston$tax, Boston$crim, xlab = 'Full Value Property Tax Rate per $10k', ylab = 'Per Capita Crime Rate', main = 'Crime Rate by Property Tax Rate')
plot(Boston$lstat, Boston$crim, xlab = 'Percent of Low Status Population', ylab = 'Per Capita Crime Rate', main = 'Crime Rate by Percent of Low Status Population')

# SEE ASSIGNMENT SUBMISSION

# 
# (b) Fit a multiple regression model to predict the response using all of the predictors. Describe your results. For which predictors can we reject the null hypothesis H0 : βj = 0?

allPredsBoston = lm(cim ~ ., data = Boston)
summary(allPredsBoston)
#   
# (c) How do your results from (a) compare to your results from (b)? Create a plot displaying the univariate regression coefficients from 
# (a) on the x-axis, and the multiple regression coefficients from (b) on the y-axis. That is, each predictor is displayed as a single point in the plot. 
# Its coefficient in a simple linear regression model is shown on the x-axis, and its coefficient estimate in the multiple linear regression model is shown on the y-axis.

coefficients = data.frame(matrix(nrow = ncol(Boston) - 1, ncol = 3))
coefficients$X1 = names(Boston)[2:length(names(Boston))]
slrcoeffs = list()

for (var in 2:length(names(Boston))) {
  formula = paste('crim ~ ', names(Boston)[var], sep = '')
  output = summary(lm(formula, data = Boston))
  slrcoeffs = append(slrcoeffs, output$coefficients[2])
}

coefficients$X2 = slrcoeffs
coefficients$X3 = allPredsBoston$coefficients[-1]
names(coefficients) = c('Predictor', 'SLRBeta', 'MLRBeta')

par(mfrow = c(1,1))
plot(coefficients$SLRBeta, coefficients$MLRBeta, xlab = 'Simple Linear Regression Coefficient', ylab = 'Multiple Linear Regression Coefficient', main = 'Simple vs. Multiple Linear Regression Coefficients for 11 Variables Predicting Per Capita Crime Rate in Boston')
text(coefficients$SLRBeta, coefficients$MLRBeta, coefficients$Predictor)

# SEE ASSIGNMENT SUBMISSION

# 
# (d) Is there evidence of non-linear association between any of the predictors and the response? To answer this question, for each predictor X, fit a model of the form: 
#   Y = β0 + β1X + β2X2 + β3X3 + ϵ.

pvalueComps = data.frame(matrix(nrow = ncol(Boston) - 2, ncol = 8))
names(pvalueComps) = c('Predictor', 'RSquared', 'X1PValue', 'X1SEValue', 'X2PValue', 'X2SEValue', 'X3Pvalue', 'X3SEValue')

for (var in 2:length(names(Boston))) {
  
  if (names(Boston)[var] == 'chas') next
  
  polFormula = paste('crim ~ ', names(Boston)[var], ' + I(', names(Boston)[var], '^2)', ' + I(', names(Boston)[var], '^3)',  sep = '')
  polyno = summary(lm(polFormula, data = Boston))
  rsquared = round(polyno$r.squared, digits = 2)
  polynop1 = polyno$coefficients[2, 4]
  polynse1 = polyno$coefficients[2, 2]
  polynop2 = polyno$coefficients[3, 4]
  polynse2 = polyno$coefficients[3, 2]
  polynop3 = polyno$coefficients[4, 4]
  polynse3 = polyno$coefficients[4, 2]
  row = cbind(names(Boston)[var], rsquared, polynop1, polynse1, polynop2, polynse2, polynop3, polynse3)
  names(row) = names(pvalueComps)
  
  pvalueComps[var, ] = row
}

pvalueComps = data.frame(pvalueComps[complete.cases(pvalueComps), ])
pvalueComps[, 2:8] = as.numeric(unlist(pvalueComps[, 2:8]))
pvalueComps$BestFitP = ''
pvalueComps$BestFitSE = ''
for (row in 1:nrow(pvalueComps)) {
  if (pvalueComps[row, 3] == min(pvalueComps[row, c(3,5,7)])) 
    pvalueComps$BestFitP[row] = 'Linear'
  if (pvalueComps[row, 4] == min(pvalueComps[row, c(4,6,8)])) 
    pvalueComps$BestFitSE[row] = 'Linear'
  if (pvalueComps[row, 5] == min(pvalueComps[row, c(3,5,7)])) 
    pvalueComps$BestFitP[row] = 'Quadratic'
  if (pvalueComps[row, 6] == min(pvalueComps[row, c(4,6,8)])) 
    pvalueComps$BestFitSE[row] = 'Quadratic'
  if (pvalueComps[row, 7] == min(pvalueComps[row, c(3,5,7)])) 
    pvalueComps$BestFitP[row] = 'Cubic'
  if (pvalueComps[row, 8] == min(pvalueComps[row, c(4,6,8)])) 
    pvalueComps$BestFitSE[row] = 'Cubic'
  }
pvalueComps


par(mfrow = c(1, 3))
plot(Boston$medv, Boston$crim, main = 'Median Value vs. Crime Rate: Linear Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$medv, 1))
lines(sort(Boston$medv), fitted(polyMod)[order(Boston$medv)], lwd = 2, col = 'blue')
plot(Boston$medv, Boston$crim, main = 'Median Value vs. Crime Rate: Quadratic Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$medv, 2))
lines(sort(Boston$medv), fitted(polyMod)[order(Boston$medv)], lwd = 2, col = 'blue')
plot(Boston$medv, Boston$crim, main = 'Median Value vs. Crime Rate: Cubic Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$medv, 3))
lines(sort(Boston$medv), fitted(polyMod)[order(Boston$medv)], lwd = 2, col = 'blue')

par(mfrow = c(1, 3))
plot(Boston$rad, Boston$crim, main = 'Radial Highway Accessibility vs. Crime Rate: Linear Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$rad, 1))
lines(sort(Boston$rad), fitted(polyMod)[order(Boston$rad)], lwd = 2, col = 'blue')
plot(Boston$rad, Boston$crim, main = 'Radial Highway Accessibility vs. Crime Rate: Quadratic Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$rad, 2))
lines(sort(Boston$rad), fitted(polyMod)[order(Boston$rad)], lwd = 2, col = 'blue')
plot(Boston$rad, Boston$crim, main = 'Radial Highway Accessibility vs. Crime Rate: Cubic Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$rad, 3))
lines(sort(Boston$rad), fitted(polyMod)[order(Boston$rad)], lwd = 2, col = 'blue')

par(mfrow = c(1, 3))
plot(Boston$tax, Boston$crim, main = 'Property Tax Rate vs. Crime Rate: Linear Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$tax, 1))
lines(sort(Boston$tax), fitted(polyMod)[order(Boston$tax)], lwd = 2, col = 'blue')
plot(Boston$tax, Boston$crim, main = 'Property Tax Rate vs. Crime Rate: Quadratic Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$tax, 2))
lines(sort(Boston$tax), fitted(polyMod)[order(Boston$tax)], lwd = 2, col = 'blue')
plot(Boston$tax, Boston$crim, main = 'Property Tax Rate vs. Crime Rate: Cubic Model Fit')
polyMod = lm(Boston$crim ~ poly(Boston$tax, 3))
lines(sort(Boston$tax), fitted(polyMod)[order(Boston$tax)], lwd = 2, col = 'blue')


