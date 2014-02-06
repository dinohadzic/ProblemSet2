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
    X[i] <- length(which(Int==i))/length(Int)            #9 in the Int vector.  
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
    X[i] <- length(which(Int==i))/length(Int)                   #9 in the Int vector.
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


#Question 3: Part 1
#Develop a function that will test your function.

Yes.Benfords <- sample(1:1000000, 10000)                        #Creates data where Benford's Law is met.
No.Benfords <- seq(1:100)                                       #Creates data where Benford's Law is not met.

testing <- function(Yes.Benfords, No.Benfords){                 #Sets Yes.Benfords and No.Benfords as the two arguments in the function "testing."
  
  Int <- as.numeric(substr(Yes.Benfords, start=1, stop=1))      #Creates a vector from the input Yes.Benfords, which only includes the first digit
                                                                  #from every element in Yes.Benfords.  
  
  X <- numeric(9)                                               #Creates an empty numeric vector of length 9 and stores it as X.
  
  for(i in 1:9){                                                #This for loop stores, in X, the proportional frequency of the digits 1 through   
    X[i] <- length(which(Int==i))/length(Int)                     #9 in the Int vector.
  }   
  
  m1 <- numeric(9)                                              #Creates an empty numeric vector of length 9 named m1.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Leemis' m statistic for 
    m1[i] <- X[i] - log10(1 + 1/i)                                #Yes.Benfords, which satisfies Benford's Law.
  } 
  
  m2 <- sqrt(length(Int)) * max(abs(m1))                        #This concludes the calculation for the Leemis' m statistic for Yes.Benfords, which is then 
                                                                  #stored as m2.
  
  d1 <- numeric(9)                                              #Creates an empty numeric vector of length 9, and stores it as d1.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Cho-Gains' d statistic for
    d1[i] <- (X[i]-log10(1+1/i))^2                                #Yes.Benfords, which satisfies Benford's Law.
  }
  
  d2 <- sqrt(sum(d1))                                           #The code on the left carries out the final parts of the d statistic calculation.
  d3 <- sqrt(length(Int)) * d2                                    #The relevant statistic is then stored as d3.
  
  Int2 <- as.numeric(substr(No.Benfords, start=1, stop=1))      #Creates a vector from the input No.Benfords, which only includes the first digit
                                                                  #from every element in No.Benfords.
  
  X2 <- numeric(9)                                              #Creates an empty numeric vector of length 9 and stores it as X2.
  
  for(i in 1:9){                                                #This for loop stores, in X2, the proportional frequency of the digits 1 through
    X2[i] <- length(which(Int2==i))/length(Int2)                  #9 in the Int2 vector.
  }  
  
  m1.2 <- numeric(9)                                            #Creates an empty numeric vector of length 9 named m1.2.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Leemis' m statistic for 
    m1.2[i] <- X2[i] - log10(1 + 1/i)                             #No.Benfords, which does not satisfy Benford's Law.
  } 
  
  m2.2 <- sqrt(length(Int2)) * max(abs(m1.2))                   #This concludes the calculation for the Leemis' m statistic for No.Benfords, which is then 
                                                                  #stored as m2.2.
  
  d1.2 <- numeric(9)                                            #Creates an empty numeric vector of length 9, and stores it as d1.2.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Cho-Gains' d statistic for
    d1.2[i] <- (X2[i]-log10(1+1/i))^2                             #No.Benfords, which does not satisfy Benford's Law.
  }
  
  d2.2 <- sqrt(sum(d1.2))                                       #The code on the left carries out the final parts of the d statistic calculation.
  d3.2 <- sqrt(length(Int2)) * d2.2                               #The relevant statistic is then stored as d3.2.
  
  if(sum(m2 == m2.2) != 0){                                     #This if statement ensures that if the m statistic for Yes.Benfords and No.Benfords is
    cat("FALSE")                                                  #the same, the output will say that the function calculates wrong Leemis statistics.
    cat("\n")
    cat("The function calculates wrong Leemis statistics.")
  }
  if(sum(d3 == d3.2) != 0){                                     #This if statment ensures that if the d statistic for Yes.Benfords and No.Benfords is
    cat("False")                                                  #the same, the output will say that the function calculates wrong Cho-Gain statistics.
    cat("\n")
    cat("The function calculates wrong Cho-Gain statistics.")
  }
  if(sum(X) != 1){                                              #This if statement ensures that if the proportional digit frequencies for Yes.Benfords
    cat("FALSE")                                                  #do not add up to 1, the ouput will say that the function calculates the wrong
    cat("\n")                                                                 #digit distribution for Yes.Benfords.
    cat("The function calculates the wrong distribution for Yes.Benfords.")
  }
  if(sum(X2) != 1){                                             #This if statement ensures that if the proportional digit frequencies for No.Benfords
    cat("FALSE")                                                  #do not add up to 1, the ouput will say that the function calculates the wrong
    cat("\n")                                                                 #digit distribution for No.Benfords.
    cat("The function calculates the wrong distribution for No.Benfords.")
  }
  if(sum(m2 == m2.2) == 0 & sum(d3 == d3.2) == 0 & sum(X) == 1 & sum(X2) == 1 & sum(X) - sum(X2) == 0){
    cat("TRUE")
    cat("\n")
    cat("All unit tests have been passed.")                     #Finally, this if statment ensures that all unit tests pass, the output will say
  }                                                               #that your function has passed all unit tests.
}       

testing(Yes.Benfords, No.Benfords)                              #The output from running the function states that all unit tests have been passed.

#Question 3: Part 2
#For each way that the function can fail this test, create a branch where you edit the code some way to make the code fail to pass the unit testing.

#The code below will make the function fail by producing the wrong digit distribution for No.Benfords.

Yes.Benfords <- sample(1:1000000, 10000)                        #Creates data where Benford's Law is met.
No.Benfords <- seq(1:100)                                       #Creates data where Benford's Law is not met.

testing <- function(Yes.Benfords, No.Benfords){                 #Sets Yes.Benfords and No.Benfords as the two arguments in the function "testing."
  
  Int <- as.numeric(substr(Yes.Benfords, start=1, stop=1))      #Creates a vector from the input Yes.Benfords, which only includes the first digit
  #from every element in Yes.Benfords.  
  
  X <- numeric(9)                                               #Creates an empty numeric vector of length 9 and stores it as X.
  
  for(i in 1:9){                                                #This for loop stores, in X, the proportional frequency of the digits 1 through   
    X[i] <- length(which(Int==i))/length(Int)                     #9 in the Int vector.
  }   
  
  m1 <- numeric(9)                                              #Creates an empty numeric vector of length 9 named m1.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Leemis' m statistic for 
    m1[i] <- X[i] - log10(1 + 1/i)                                #Yes.Benfords, which satisfies Benford's Law.
  } 
  
  m2 <- sqrt(length(Int)) * max(abs(m1))                        #This concludes the calculation for the Leemis' m statistic for Yes.Benfords, which is then 
  #stored as m2.
  
  d1 <- numeric(9)                                              #Creates an empty numeric vector of length 9, and stores it as d1.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Cho-Gains' d statistic for
    d1[i] <- (X[i]-log10(1+1/i))^2                                #Yes.Benfords, which satisfies Benford's Law.
  }
  
  d2 <- sqrt(sum(d1))                                           #The code on the left carries out the final parts of the d statistic calculation.
  d3 <- sqrt(length(Int)) * d2                                    #The relevant statistic is then stored as d3.
  
  Int2 <- as.numeric(substr(No.Benfords, start=1, stop=1))      #Creates a vector from the input No.Benfords, which only includes the first digit
  #from every element in No.Benfords.
  
  X2 <- numeric(9)                                              #Creates an empty numeric vector of length 9 and stores it as X2.
  
  
  for(i in 1:9){                                                #This for loop ensures that the proportional frequency distribution of the digits for
    X2[i] <- length(which(Int2==i))/length(Int2) * 2                #Yes.Benfords will fail. 
  }  
  
  
  m1.2 <- numeric(9)                                            #Creates an empty numeric vector of length 9 named m1.2.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Leemis' m statistic for 
    m1.2[i] <- X2[i] - log10(1 + 1/i)                             #No.Benfords, which does not satisfy Benford's Law.
  } 
  
  m2.2 <- sqrt(length(Int2)) * max(abs(m1.2))                   #This concludes the calculation for the Leemis' m statistic for No.Benfords, which is then 
  #stored as m2.2.
  
  d1.2 <- numeric(9)                                            #Creates an empty numeric vector of length 9, and stores it as d1.2.
  
  for(i in 1:9){                                                #This for loop carries out the first calculation of the Cho-Gains' d statistic for
    d1.2[i] <- (X2[i]-log10(1+1/i))^2                             #No.Benfords, which does not satisfy Benford's Law.
  }
  
  d2.2 <- sqrt(sum(d1.2))                                       #The code on the left carries out the final parts of the d statistic calculation.
  d3.2 <- sqrt(length(Int2)) * d2.2                               #The relevant statistic is then stored as d3.2.
  
  if(sum(m2 == m2.2) != 0){                                     #This if statement ensures that if the m statistic for Yes.Benfords and No.Benfords is
    cat("FALSE")                                                  #the same, the output will say that the function calculates wrong Leemis statistics.
    cat("\n")
    cat("The function calculates wrong Leemis statistics.")
  }
  if(sum(d3 == d3.2) != 0){                                     #This if statment ensures that if the d statistic for Yes.Benfords and No.Benfords is
    cat("False")                                                  #the same, the output will say that the function calculates wrong Cho-Gain statistics.
    cat("\n")
    cat("The function calculates wrong Cho-Gain statistics.")
  }
  if(sum(X) != 1){                                              #This if statement ensures that if the proportional digit frequencies for Yes.Benfords
    cat("FALSE")                                                  #do not add up to 1, the ouput will say that the function calculates the wrong
    cat("\n")                                                                 #digit distribution for Yes.Benfords.
    cat("The function calculates the wrong distribution for Yes.Benfords.")
  }
  if(sum(X2) != 1){                                             #This if statement ensures that if the proportional digit frequencies for No.Benfords
    cat("FALSE")                                                  #do not add up to 1, the ouput will say that the function calculates the wrong
    cat("\n")                                                                 #digit distribution for No.Benfords.
    cat("The function calculates the wrong distribution for No.Benfords.")
  }
  if(sum(m2 == m2.2) == 0 & sum(d3 == d3.2) == 0 & sum(X) == 1 & sum(X2) == 1 & sum(X) - sum(X2) == 0){
    cat("TRUE")
    cat("\n")
    cat("All unit tests have been passed.")                     #Finally, this if statment ensures that all unit tests pass, the output will say
  }                                                               #that your function has passed all unit tests.
}       

testing(Yes.Benfords, No.Benfords)                              #The output from the function states "FALSE" and "The function calculates the wrong
                                                                  #distribution for No.Benfords."
