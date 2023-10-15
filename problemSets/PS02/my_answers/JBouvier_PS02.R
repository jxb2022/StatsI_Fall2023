getwd()
setwd("/Users/ierja/Documents/GitHub/StatsI_Fall2023")

# Stating my Null and Alternate Hypothesis 
# Ho - Police officers are more likely to stop you depending on if you seeminly 
# look upper class H1 - Police officers do not care of social status

#Finding X^2 test statistic Formula X^2 = (fo - fe)^2/fe - the observed - expected squared 
# and then divided by the expected

# First find total of columns and rows 
# Not Stopped = 21 Bribe Requested =13 Stopped/given warning = 8
# Upper class = 27 Lower Class = 15
# Total records = 42
# Find percentage of upper and lower class 
upper <- 27/42
lower <- 15/42
# We calculate the percentage of occurance out to be 64.29% for Upper class and 
# Lower class to be 35.71%
# We will then take these percentages and calculate and expected value for each outcome

# Upper class row total of not stopped = 21 we multiply this by 64% to get expected 
# to equal 13.50. Then we continue this with each column total for upper class by 64.29%
Up_notstop_expec <- 21*0.6429
Up_Bribe_expec <- 13*0.6429
Up_Stop_expec <- 8*0.6429
# We repeat this with the lower class and the percentage 35.71% and totals
Low_notstop_expec <- 21*0.3571
Low_Bribe_expec <- 13*0.3571
Low_Stop_expec <- 8*0.3571
# We now have our expected values for our formula -we will take each piece of data and 
# put it into the formula - each observed and corresponding expected value will be squared 
# and then divided by expected. We will then sum all the values together for the X^2 test statistic

Xsqrd <- (((14-Up_notstop_expec)^2/Up_notstop_expec) + 
            (6 - Up_Bribe_expec)^2/ Up_Bribe_expec + 
              (7 - Up_Stop_expec)^2/ Up_Stop_expec +
          (7 - Low_notstop_expec)^2/ Low_notstop_expec +
              (7 - Low_Bribe_expec)^2/ Low_Bribe_expec +
              ( 1 - Low_Stop_expec)^2/ Low_Stop_expec
)
# Our X^2 test statistic is 3.79


# We can now find our p - value
# First to find the degrees of freedom with formula 
# df = (r-1)(c-1)
df <- (2-1)*(3-1)
# Our degrees of freedom is 2
# plug into our p-value for chisq formula pchisq( X^2, df=, lower.tail=F)
pvalue <- pchisq(3.79, df=2, lower.tail=FALSE)
# Our p-value is 0.15032


# We would have to accept our Ho as we do not have enough statistical significance to reject
# Since we are accepting our Ho we now should calculate standardized residuals
# We can use the formula z = (fo - fe)/sqrt(fe(1-rowprop)(1-columnprop))

# Standardized residual for upper class not stopped
z_UpNS <- (14 - Up_notstop_expec)/sqrt((Up_notstop_expec*(1-(27/42))*(1-(21/42))))

# Standardized residual for upper class bribe requested 
z_UpBr <- (6 - Up_Bribe_expec)/sqrt((Up_Bribe_expec*(1-(27/42))*(1-(13/42))))

# Standardized residual for upper class stopped
z_UpStop <- (7 - Up_Stop_expec)/sqrt((Up_Stop_expec*(1-(27/42))*(1-(8/42))))


# Standardized residual for lower class not stopped 
z_LowNS <- (7 - Low_notstop_expec)/sqrt((Low_notstop_expec*(1-(15/42))*(1-(21/42))))


# Standardized residual for lower class bribe requested 
z_LowBr <- (7 - Low_Bribe_expec)/sqrt((Low_Bribe_expec*(1-(15/42))*(1-(13/42))))

# Standardized residual for lower class bribe requested 
z_LowStop <- (1 - Low_Stop_expec)/sqrt((Low_Stop_expec*(1-(15/42))*(1-(8/42))))


#As we are accepting the Ho we would expect that little to none of our results would 
# exceed 2 in absolute value meaning that it further that we should be accepting our Ho.
# Since the points given to us are not considered outliers. We can also assume
# since that the value is not more than 2 the cell has less observations than expected if the 
# variables were truly independent.



# Ho - The gender of a leader of a village had no impact on water facilities
# Ha- Female leaders are more likely to support water facilities therefore there would be more facilities

data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
data

#Looking at the data 
plot(data$female,data$water)
str(data)
mean_female <- mean(data$female)
mean_water <- mean(data$water)
sd_female <- sd(data$female)
sd_water <- sd(data$water)
# we run the regression
biV <- lm(data$water ~ data$female)
summary(biV)
# regression by hand

r <- cov(data$water, data$female)/ (sd_water * sd_female)
n <- dim(data)[1]
t_stat <- ((r*sqrt(n-2))/ (1-r^2))
p_value <- 2*pt(t_stat, n-2, lower.tail=FALSE)


# The formula we would use for a bivarate regression test is y=Bo +BiX + E (y=14.813+7.864X+E) but with large data sets through r
# Running our data through the lm(Y ~ X ) function and looking at the information given we can assume
# that there is little statistical evidence that the gender of the political leader
# has anything to do with the number of water facilities/polices. We would therefor accept the Ho.







