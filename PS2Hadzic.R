#Problem Set 2
#Dino Hadzic

rm(list=ls()) #Clears the workspace in R.
setwd("/Users/dinohadzic/Desktop/ProblemSet2") #Sets the working directory

#Question 1
#Write a function to calculate Leemis' m statistic and Cho-Gains' d statistic.  The function should have as an input (i) a matrix or 
#vector of election returns and (ii) an option (or options) that controls whether the m statistic should be calculated, the d statistic
#should be calculated, or both.  The output should be a list containing the results, including the full digit distribution.

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


#Question 2
#Create a new function called print.benfords() that will ouput a table containing: (1) The name of each statistic; (2) The statistic as
#it was calculated; (3) The relevant number of asterisks; (3) A legend at the bottom explaining the asteriks.

print.benfords <- function(Totals){                           #Sets Totals as the only argument in the function print.benfords.  Totals
                                                                #can be a vector or a matrix.
  
  Int <- as.numeric(substr(Totals, start=1, stop=1))          #Creates a vector from the input Totals (which can be either a vector or matrix) 
                                                                #which only includes the first digit from every element in Totals. 
  
  X <- numeric(9)                                             #Creates an empty numeric vector of length 9 and stores it as X. 
  
  for(i in 1:9){                                              #This for loop stores, in X, the proportional frequency of the digits 1 through 
    X[i] <- length(which(Int==i))/length(Int)                   #9 in the Integer vector.
  } 
  
  m1 <- numeric(9)                                            #Creates an empty numeric vector of length 9 named m1.        
  
  for(i in 1:9){                                              #This for loop carries out the first calculation of the Leemis' m statistic.
    m1[i] <- X[i] - log10(1 + 1/i)
  }
  
  m2 <- sqrt(length(Int)) * max(abs(m1))                      #This concludes the calculation for the Leemis' m statistic, which is then 
                                                                #stored as m2.
  
  d1 <- numeric(9)                                            #Creates an empty numeric vector of length 9, and stores it as d1.
  
  
  for(i in 1:9){                                              #This for loop carries out the first part of the Cho-Gains' d statistic calculation.
    d1[i] <- (X[i]-log10(1+1/i))^2
  }
  
  d2 <- sqrt(sum(d1))                                         #The code on the left carries out the final parts of the d statistic calculation.        
  d3 <- sqrt(length(Int)) * d2                                   #The relevant statistic is then stored as d3.
  
  m2 <- substr(m2, start=1, stop=5)                           #The two lines on code on the left shorten the length of the Leemis statistic
  m2 <- as.numeric(m2)                                          #that will eventually appear in the table.
  
  d3 <- substr(d3, start=1, stop=5)                           #The two lines on code on the left shorten the length of the Cho-Gain statistic
  d3 <- as.numeric(d3)                                          #that will eventually appear in the table.
 
  Sig_Level <- character(2)                                   #Creates a character vector of length 2 called Sig_Level
  
  for(i in 1:length(Sig_Level)){                              #This for loop ensures that the appropriate significance levels will appear in
    if(m2 >= 0.851 & m2 < 0.967){                               #the table for the Leemis and Cho-Gain statistics.  The first element in the
      Sig_Level[1] <- "*"                                       #Sig_Level vector will be "*" if the Leemis statistic is less than 0.967 and 
    }                                                           #greater than or equal to 0.851, "**" if less than 1.212 and greater than or
    if(m2 >= 0.967 & m2 < 1.212){                               #0.967, "***" if greater than or equal to 1.212, and "Not Significant" if less
      Sig_Level[1] <- "**"                                      #than 0.851.  With respect to the Cho-Gain statistic, the second element of the
    }                                                           #Sig_Level vector will be "*" if the Cho-Gain statistic is less than 1.330 and
    if(m2 >= 1.212){                                            #equal to or greater than 1.212, "**" if less than 1.569 and equal to or greater
      Sig_Level[1] <- "***"                                     #1.330, "***" if equal to or greater than 1.569, and "Not Significant" if less                              
    }                                                           #than 1.212.
    if(m2 < 0.851){
      Sig_Level[1] <- "Not Significant"
    }
    if(d3 >= 1.212 & d3 < 1.330 ){
      Sig_Level[2] <- "*"
    }
    if(d3 >= 1.330 & d3 < 1.569 ){
      Sig_Level[2] <- "**"
    }
    if(d3 >= 1.569){
      Sig_Level[2] <- "***"
    }
    if(d3 < 1.212){
      Sig_Level[2] <- "Not Significant"
    }
  }
  
  Value <- c(m2,d3)                                             #Creates a vector called Value that includes the calculated Leemis and Cho-Gain
                                                                  #statistics.
  
  Statistic <- c("Leemis_m", "Cho-Gains_d")                     #Creates a vector called Statistic tha includes the names of the statistics.
  
  print(as.table(cbind("Statistic"=Statistic, "Value"=Value, "Significance Level"=Sig_Level)))  #Cbinds the vectors Statistic ("Statistic"), 
  #Value ("Value"), and Sig_Level ("Significance Level"), converts that matrix into a table, and finally prints the output.
 
  cat("\n")                                                     #Creates a linebreak in the output.
  
  cat("Significance levels: 0.10*, 0.05**, 0.01***")            #Adds a legend beneath the table that includes the meaning of the asterisks
}                                                                 #with respect to significance levels.
