#####################
# load libraries
# set wd
# clear global .envir
#####################

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
lapply(c('stargazer'), pkgTest)
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
getwd()
# set wd for current folder
setwd("Users/ierja/Documents/GitHub/Stats_Fall2023")

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

par(mfrow = c(2,2))
###### Question 1 ######

q1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(q1)

plot(inc.sub$difflog, inc.sub$voteshare, main = "Figure 1: Difflog to Votershare",
     xlab = "Difflog", ylab = "Voteshare",
     col = "darkgreen")
abline(q1, col = "black")

stargazer(q1, title = "Vote Share vs Spending Difference", type = "latex")

resid_1 <- residuals(q1)
print(resid_1)



##### Question 2 #####

q2 <- lm(presvote ~ difflog, data = inc.sub)
summary(q2)
stargazer(q2, title = "Pres Vote vs Spending Difference", type = "latex")
plot(inc.sub$difflog, inc.sub$presvote, main = "Figure 2: Difflog to Presvote",
     xlab = "Difflog", ylab = "Presvote",
     col = "cyan")
abline(q2, col = "black")


resid_2 <- residuals(q2)
print(resid_2)



#### Question 3 #####



q3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(q3)
stargazer(q3, title = "Vote Share vs Pres Vote", type = "latex")
plot(inc.sub$presvote, inc.sub$voteshare, main = "Figure 3: Presvote to Votershare",
     xlab = "Presvote", ylab = "Voteshare",
     col = "darkorange")
abline(q3, col = "black")


resid_3 <- residuals(q3)



#### Question 4 ####


q4 <- lm(resid_1 ~ resid_2)
summary(q4)
stargazer(q4, title = "Vote Share vs Spending Difference", type = "latex")

plot(resid_2, resid_1, 
     main = "Figure 4: Residuals Plot",
     xlab = "Residuals 1", 
     ylab = "Residuals 2",
     col = "darkorchid")
abline(q4, col = "black")

resid_4 <- residuals(q4)


#### Question 5 ####

q5 <- lm(voteshare ~ difflog + presvote, data= inc.sub)
summary(q5)
stargazer(q5, title = "Comparing Difflog and Pres Vote to Vote Share", type = "latex")
