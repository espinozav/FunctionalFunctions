#########################
#                       #
# Functions from HW 1   #
#                       #
#########################

#this function counts even numbers in a vector
evencount <- function(y) {
  #   print("x is:")
  print(y)
  k <- 0 # assign 0 to k
  print(paste("k is initialized as",k))
  for (n in y) {
    print(paste("current x value being tested is",n))
    if(n %% 2 == 0) # Giving it the condition that if the modulo operator = 0 then even 
    {
      k <- k+1 # %% is the modulo operator
      print(paste(n,"is an even number!"))
    } else
    {
      print(paste(n,"is an odd number!"))
    }
    print(paste("k is currently",k))
  }
  print(paste("The final k is",k))
  return(k)
}

evencount(y <- c(4,2,3,7,8,6,9,1,6,2))



#Systemtime function 
v <- matrix(rnorm(100),100,100) 
v

system.time( # to measure the execution time 
  for (i in 1:m) {
    for (j in 1:n) {
      v[i,j]<-v[i,j] + 10*sin(0.9*pi)
    }
  }
)
v

system.time(v[i,j]<-v[i,j] + 10*sin(0.9*pi))

#########################
#                       #
# Functions from HW 4   #
#                       #
#########################
#Function that turns a vector c("a", "b", "c") into the string a, b, and c.
commasep <- function(x, sep = ", ", last = ", and ") {
  if (length(x) > 1) {
    str_c(str_c(x[-length(x)], collapse = sep),
          x[length(x)],
          sep = last)
  } else {
    x
  }
}
commasep("a")
commasep("data")
commasep(c("data", "stats"))
commasep(c("data", "stats", "analysis"))



#Function that takes the mean of air qaulity variable o3 in Merced County 
#need to improve this and make efficient/correct

library(data.table)
library(tidyverse)
site.mean <- function(x){
  x <- c("Merced")
  name_match <- str_c(x,collapse = "|")
  name_subset <- str_subset(loc$`Site Name`,name_match)
  merceddata <- subset(daily.site, daily.site$`Site Name`== name_subset)
  mean=mean(x, na.rm=TRUE)
}
site.mean(merceddata$o3)
mean



###Alternatives code to fix later 
#Problem11 Hw 4 
library(data.table)
library(tidyverse)

site.mean <- function(x,y,z){
  x$year <- year(x$date)
  yearlySan <- x%>%
    group_by(site, year1=as.factor(year))
  c <- c(y, z)
  name_match <- str_c(c,collapse = "|")
  d <- str_subset(loc$`Site Name`,name_match)
  San <- subset(yearlySan, yearlySan$`Site Name`== d )%>%
    group_by(site,year1 )%>%
    summarize(mean=mean(o3, na.rm=TRUE),median=median(o3, na.rm = TRUE),max=max(o3, na.rm=TRUE),min=min(o3, na.rm=TRUE))
  San
}
site.mean(daily.site,"San","Santa")




library(data.table)
library(tidyverse)

merced.mean <- function(x,y){
  x$year <- year(x$date)
  yearlyMerced <- x%>%
    group_by(site, year1=as.factor(year))
  d <- c(y)
  name<- str_c(d)
  str_subset(loc$`County Name`,name)
  Merced <- subset(yearlyMerced, yearlyMerced$`Site Name`== name_subset)%>%
    group_by(year1)%>%
    mean=mean(o3, na.rm=TRUE))
Merced
}
merced.mean(daily.site,"Merced")


#Problem 12 HW 4 
daily.site$year <- year(daily.site$date)
yearlyMerced <- daily.site%>%
  group_by(site, year1=as.factor(year))
d <- c("Merced")
name<- str_c(d)
str_subset(loc$`County Name`,name)
Merced <- subset(yearlyMerced, yearlyMerced$`Site Name`== name_subset)%>%
  group_by(`County Name`,year1%>%
             mean(o3, na.rm=TRUE))
Merced
}
merced.mean(daily.site,"Merced")


