#Problem Set 2
#Dino Hadzic

rm(list=ls()) #Clears the worspace in R.
setwd("/Users/dinohadzic/Desktop/ProblemSet2") #Sets the working directory

#Question 1

Election_fraud <- function(Totals, statistic){         #Sets Totals and statistic as the two arguments for the function Election_fraud.   
  
  Int <- as.numeric(substr(Totals, start=1, stop=1))   #Creates a vector from the input Totals (which can be either a vector or matrix) 
                                                         #which only includes the first digit from every element in Totals.  
  
  X <- numeric(9)                                     #Creates an empty numeric vector of length 9 and stores it as X.
  
  for(i in 1:9){                                      #This for loop stores, in X, the proportional frequency of the digits 1 through 
    X[i] <- length(which(Int==i))/length(Int)            #9 in the Integer vector.  
  }                                                      
  
  X <- as.matrix(X)                                   #Converts X into a matrix.
  
  rownames(X) <- c(1,2,3,4,5,6,7,8,9)                 #Names the rows in X according to the digits and the column "Pro. Frequency," 
  colnames(X) <- "Pro. Frequency"                        #meaning proportional frequency.
  
  m1 <- numeric(9)                                    #Creates an empty numeric vector of length 9 named m1.
  
  for(i in 1:9){                                      #This for loop carries out the first calculation of the Leemis' m statistic.
    m1[i] <- X[i] - log10(1 + 1/i)
  }
  
  m2 <- sqrt(length(Int)) * max(abs(m1))              #This concludes the calculation for the Leemis' m statistic, which is then 
                                                         #stored as m2.
  
  d1 <- numeric(9)                                    #Creates an empty numeric vector of length 9, and stores it as d1.
  
  for(i in 1:9){                                      #This for loop carries out the first part of the Cho-Gains' d statistic calculation.
    d1[i] <- (X[i]-log10(1+1/i))^2
  }
  
  d2 <- sqrt(sum(d1))                                 #The code on the left carries out the final parts of the d statistic calculation.
  d3 <- sqrt(length(Int)) * d2                           #The relevant statistic is then stored as d3.
  
  if(statistic == "m"){                                       #This if statement returns a list that includes Leemis' m statistic and
    return(list("Leemis' m" = m2, "Digit Distribution" = X))     #the digit distribution when the statistic argument is set to "m."
  }
  
  if(statistic == "d"){
    return(list("Cho Gains' d" = d3, "Digit Distribution" = X)) #This if statement returns a list that includes Cho Gains' d statistic
  }                                                               #and the digit distribution when the statistic argument is set to "d."
  
  if(statistic == "m&d"){
    return(list("Cho Gains' d" = d3, "Leemis' m" = m2, "Digit Distribution" = X)) #This if statement returns a list that includes
  }                                                                                 #Leemis' m statistic, Cho Gains' d statistic, and
}                                                                                   #the digit distribution if the statistic argument
                                                                                    #is set to "m&d."

