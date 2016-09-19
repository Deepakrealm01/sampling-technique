rm(list=ls(all=TRUE))

#set current working directory
setwd("G:/Analytics/Edwisor/Edwisor/Predictive Analytics using R/R code")

#load data into R
data = read.csv("", header = T)

#Load sample data and practice sampling techniques
#https://vincentarelbundock.github.io/Rdatasets/datasets.html
library(robustbase)
data = pension

#select data by random sampling
data_sample = data[sample(nrow(data), 10, replace = F), ]

#Divide data into train and test
data = iris
train = data[sample(nrow(data), 120, replace = F), ]
test = data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]

##stratified sampling
library(sampling)

#create our own data
data = rbind(matrix(rep("nc", 165), 165, 1, byrow=TRUE), matrix(rep("sc", 70), 70, 1, byrow = TRUE))
data = cbind.data.frame(data, c(rep(1, 100), rep(2, 50), rep(3, 15), rep(1, 30),rep(2, 40)), 1000*runif(235))
names(data) = c("state", "region", "income")

#create table to analyse data and its propotion
table(data$region)

#sampling using  startified
stratas = strata(data, c("region", "state"), size = c(10, 5, 2, 3, 4), method = "srswor")
stratas_v1 = strata(data, c("region"), size = c(13, 9, 2, 4), method = "srswor")

stratified_data = getdata(data, stratas)

##Systematic sampling
#load base data
data(belgianmunicipalities)
Tot = belgianmunicipalities$Tot04
pik = inclusionprobabilities(Tot,200)

#selects a sample
s = UPsystematic(pik)

# extracts the observed data
systematic_data = getdata(belgianmunicipalities, s)

#another method
#This R code selects a systematic sample of size n from a  population of size N. 
sys.sample = function(N,n){
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k*(n-1), k)
  #cat("The selected systematic sample is: \"", sys.samp, "\"\n")
}

lis = sys.sample(589, 200) #select the repective rows

belgianmunicipalities$index = 1:589
systematic_data = belgianmunicipalities[which(belgianmunicipalities$index %in% lis),]

#Cluster sampling
data(swissmunicipalities)
# the variable 'REG' has 7 categories in the population
# the sample size is 3; the method is simple random sampling without replacement
cl = cluster(swissmunicipalities, clustername=c("REG"), size=3, method="srswor")
cluster_data = getdata(swissmunicipalities, cl)















