### This script will demonstrate basic familiarity with R
###
### Noah Hillman
### April 20, 2021


# Load the R library, ggplot2, and no others (i.e., you should use base R for most
# of this exercise)

library(ggplot2)
library(MASS)

# Create a dataframe with NAs in every cell, 100 rows, and columns with the
# following names: bblid,sex,age,cognition.

df <- data.frame(bblid = rep(NA,100),sex = rep(NA,100),age = rep(NA,100),cognition = rep(NA,100))

# Sample 100 random integers, without replacement, between 10,000 and 20,000
# to serve as bblids (our lab's subject identifier). Put these integers into the
# bblid column.

ids <- sample(10000:20000,size = 100,replace = FALSE)
df$bblid <- ids

# Simulate 100 ages and 100 cognition scores from a bivariate normal distribution
# with rho=.7, and put these values into their respective columns in the dataframe.

set.seed(10)
scores <- mvrnorm(n = 100, mu = c(0,0),Sigma = matrix(c(1,.7,.7,1),nrow = 2,ncol = 2))
colnames(scores) <- c("age","cognition")
df$age <- scores[,"age"]
df$cognition <- scores[,"cognition"]

# Scale the age values so mean=15 and variance=6.
newMean <- 15
newVar <- 6
oldMean <- with(df,mean(age))
oldVar <- with(df,var(age))
a <- sqrt(newVar/oldVar)
b <- newMean - a*oldMean

df$age <-  a*df$age + b

with(df,mean(age))
with(df,var(age))

# Check if any age values are below zero. How many are there?

(belowZero <- sum(df$age < 0))

# There appears to be no individuals in the dataset whose age is recorded as less than 0.

# For any age values that are below zero, replace that age with zero. Write this
# code whether or not there are ages below zero.

df$age <- with(df,ifelse(age < 0,0,age))

# Create two ggplots: a histogram of ages, and a histogram of cognition. Use
# non-default themes and colors (explore!).

AgeHist <- ggplot(data = df) + geom_histogram(aes(x = age),fill = "steelblue2") + labs(x = "Age",title = "Age of Participants") + theme_classic()


CogHist <- ggplot(data = df) + geom_histogram(aes(x = cognition),fill = "gray70") + labs(x = "Cognitive Scores",title = "Distribution of Cognitive Scores") + theme_classic()

# Calculate the correlation between age and cognition. Does it look familiar?

with(df,cor(age,cognition))

# The correlation between age and cognition is nearly identical to the value of $\rho$ which was specified when generating the random values for cognition and age using the multivariate normal distribution. 

# Now assign the first 50 rows in your dataframe to be female, and the latter
# 50 to be male. Make sure your sex variable is coded as a factor.

df$sex <- c(rep("female",50),rep("male",50))
df$sex <- with(df,factor(sex))

# Create a scatterplot with age on the x-axis, and cognition on the y-axis.
# Color your points by sex, and plot a regression line per sex.

Scatterplot <- ggplot(data = df,aes(x = age,y = cognition,color = sex)) + geom_point() + geom_smooth(method = "lm") + theme_classic() + labs(x = "Age",y = "Cognitive Scores",title = "Cognitive Scores Improve throughout Development") + scale_color_brewer(palette = "Dark2")

# Export all of your plots to one pdf, with each plot on a different page.

pdf(file = "UPennPlots.pdf")

AgeHist
CogHist
Scatterplot 

dev.off()