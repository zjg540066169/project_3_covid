library(tidyverse)
test <- read_csv("test.csv")
coef <- read.csv("cumulative_region_no_0.csv")
new_test <- data.frame()
for (i in 1:nrow(test)){
  raw_case <- test[i,5:ncol(test)]
  if (test[i,2] %in% coef$X.region_name.){
    
    if (test[i,2] %in% new_test$Country.Region){
      new_test[which(new_test$Country.Region==as.character(test[i,2])),2:ncol(new_test)] = new_test[which(new_test$Country.Region==as.character(test[i,2])),2:ncol(new_test)] + raw_case
    }
    else {
      new_test <- rbind(new_test,data.frame(test[i,2],raw_case))
    }
  }
}
date <- ncol(new_test)
for (i in 1:nrow(new_test)){
  case <- new_test[i,2:date]
  case <- case[case!=0]
  a <- coef$X.a.[which(coef$X.region_name.==as.character(new_test[i,1]))]
  b <- coef$X.b.[which(coef$X.region_name.==as.character(new_test[i,1]))]
  c <- coef$X.c.[which(coef$X.region_name.==as.character(new_test[i,1]))]
  estimate_case <- a/(1+exp(-b*(1:length(case)-c)))
  new_test$mse[i] <- sum((estimate_case-case)^2)
}
data.frame(new_test$Country.Region,new_test$mse)

plot(density(log(new_test$mse)))

     