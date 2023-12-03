# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("car"),  pkgTest)


data("Prestige")
help("Prestige")

getwd()
setwd("/Users/ierja/Documents/GitHub/StatsI_Fall2023")
getwd()


## Question 1

# A

professional <- ifelse(
                Prestige$type == "prof", 1, 0
)


print(professional)

# B

lm_prestige <-lm(prestige ~ income + professional + income:professional, data = Prestige)

summary(lm_prestige)

lapply(c('stargazer'), pkgTest)
stargazer(lm_prestige, title = "Linear Model of Prestige with Income and Professional", type = "latex")



# F 

Fnon_yHat1000 <-21.142+(0.003*1000)+(37.781*0)+(-0.002*(1000*0))
24.142

Fprof_yHat1000 <- 21.142+(0.003*1000)+(37.781*1)+(-0.002*(1000*1))
59.923

ME_1000 <- (0.003+(-0.002))*1000
1


# G

Fnon_yHat6000 <- 21.142+(0.003*6000)+(37.781*0)+(-0.002*(1000*0))
39.142

Fprof_yHat6000 <- 21.142+(0.003*6000)+(37.781*1)+(-0.002*(1000*1))
74.923

ME_6000 <-(0.003+(-0.002))*6000
6

# Problem 2

# a

t_inYard <- 0.042/0.016
2.625

p_inYard <- pt(2.625, 29, lower.tail = FALSE)
0.007




# b


t_AdjYard <- 0.042/0.013
3.230
p_AdjYard <- pt(3.230, 75, lower.tail = FALSE)
0.001

