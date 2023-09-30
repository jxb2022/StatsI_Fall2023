#####################
# load libraries
# set wd
# clear global .envir
#####################
18/9
getwd()
setwd("/Users/ierja/Documents/GitHub/StatsI_Fall2023/problemSets/JB_My_Answers/template")
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr"),  pkgTest)


#####################
# Problem 1
#####################
# Part 1
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
length(y)
# y = 25
n <- length(y)
df <- (n-1)
# df = 24
# Find Mean of y
mean(y)
mean <- mean(y)
# mean = 98.44
# Find Standard Deviation of y
sd(y)
sd <- sd(y)
# sd = 13.0928733795654
# Since n < 30 we use t-statistic
t <-1.318
# CI_90 = mean +_ t (sd/sqrt(n))
Upper_CI_90 <- (mean + t(sd/sqrt(n)))
Lower_CI_90 <- (mean - t(sd/sqrt(n)))

# CI 90% [95.8, 101] - We are 90% confident that the average student IQ is 
# between [95.8, 101]

#Problem 1 Part 2 (https://www.adamnsmith.com/MSIN0010/hypothesis-tests.html)
#(http://courses.washington.edu/psy315/tutorials/t_test_tutorial.pdf)
# Using one tailed t.test as there is one parameter
#Standard Error
Sx <- (sd/sqrt(25))
a <- 0.05
# t value from t.table from https://www.sjsu.edu/faculty/gerstman/StatPrimer/t-table.pdf
t_tab <- 1.711

t_obs <- ((mean - 100)/(Sx))
pt(t_obs,df,lower.tail= TRUE)
p <- pt(t_obs,df,lower.tail= TRUE)

            
t.test(y, mu = 100) # to double check?

# Our Ho would be that the average IQ of students in the counselors school was 
# lower than that of the average IQ score of 100 from all schools across the country
# we would accept the Ho as the t.table value for 90% CI is 1.711 and is larger than our 
# t_obs value of 0.59




#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
expenditure
str(expenditure)
# part 1 (Please Refer to Figure 1 & 2)
# Code to get the plots to be shown together from https://www.statmethods.net/advgraphs/layout.html
attach(expenditure)
par(mfrow = c(2,2))
plot(expenditure$X1, expenditure$Y, main = "Personal Income - Shelter Assistance",
     xlab = "Personal Income", ylab = "$Shelter Assistance",
)
plot(expenditure$X2, expenditure$Y, main = "Financially Insecure - Shelter Assistance",
     xlab = "Financially Insecure", ylab = "$Shelter Assistance",
)
plot(expenditure$X3, expenditure$Y, main = "Urban Area - Shelter Assistance",
     xlab = "Residing Urban", ylab = "$Shelter Assistance"
)
# Create scatter plot of (X1,Y),(X2,Y),(X3,Y)
pdf("PS01_Q2_pt1")
# We see that in the graph mapping personal income, there is somewhat of a positive 
# correlation between state spending on shelter assistance and the per capita personal
# income in the state, with greater variation in the middle of the graph. 
# In the graph mapping financial insecurity, there is a vague "U" shape of the
# relationship, suggesting a somewhat postive correlation with a slight bump with the 
# states with the highest numbers of financially insecure residents spending more on
# shelter assistance than a strong positive correlation would suggest.
# In the graph considering urban area, there is a very weak positive correlation
# between the amount states spend on urban assistance and the number of residents 
# living in urban areas. 
# Create scatter plot of (X1,X2),(X1,X3),(X2,X3)
pdf("JBouvier_X1X2X3plots")
attach(expenditure)
par(mfrow = c(2,2))
plot(expenditure$X1, expenditure$X2, main = "Personal Income vs Financially Insecure",
     xlab = "Personal Income", ylab = "Financially Insecure"
     )
plot(expenditure$X1, expenditure$X3, main = "Personal Income vs Urban Residing",
     xlab = "Personal Income", ylab = "Urban Residing"
)
plot(expenditure$X2, expenditure$X3, main = "Financially Insecure vs Urban Residing ",
     xlab = "Financially Insecure", ylab = "Urban Residing"
)


# I would have thought there would be more correlations between these. In the plot of
# personal income vs urban residing we can assume a positive correlation  where the 
# higher the personal income the more likely to reside in an urban area. Which then if you look
# at financially insecure vs urban residing, we can see that there is very little correlation
# as this could be because we could assume that more people living in urban areas would perhaps make more income
# Though what we don't see with these is the states where the states/regions could tell us a lot more about
# any relationships that could be there.



# part 2 (Please Refer to Figure 3)

plot(expenditure$Region, expenditure$Y, main = "Region vs Expenditure",
     xlab = "Region", ylab = "Expenditure")
# It looks like that Region 4 has the highest expenditure per capita on housing assistance
# Though the data also is much more spread out than say Region 2 or 3. Looking at the Data we can see that 
# California is the highest expenditure. Region 4 also has the lowest expenditure as well with Wyoming.
# 

# part 3 (Please Refer to Figure 4)

attach(expenditure)
par(mfrow = c(2,2))
plot(expenditure$X1, expenditure$Y, main = "Personal Income - Shelter Assistance",
     xlab = "Personal Income", ylab = "$Shelter Assistance",
)
# We can see how personal income vs how much is spent on shelter/housing assistance. As mentioned in part
# one of this question that perhaps with higher income comes higher taxes - and depending on the 
colors <- c('green', 'blue','red', 'black')
plot(expenditure$X1, expenditure$Y, main = "Personal Income - Expenditure",
     xlab = "Personal Income", ylab = "$Shelter Assistance", 
     pch = expenditure$Region, col = colors[expenditure$Region])

# Green Region 1, Blue Region 2, Red Region 3, Black Region 4
  

