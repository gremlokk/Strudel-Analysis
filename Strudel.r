# STRUDEL FIDELITY RESEARCH v1-07-05-2020
# Dr. Ritter, Fredrick Ryans, Matthew Koh
# This document is a visual analysis of the strudel data models provided
# in the Ritter paper: fidelity on learning.
# Last updated: 07/05/2020

library(dplyr)
library(ggplot2)
library(tidyr)

print ("STRUDEL FIDELITY RESEARCH")#Learning curve plot: values must be assigned as such - x= c(1,2,..)

print ('#VERSIONS 1, PLOT A (small) LEARNING CURVE')

#Formula 
df = data.frame(
data = seq(1,50), # Sequence of numbers
meanX = mean(data), # Means of seq X
stdX = sd(data), # Standard deviation of seq X
Y = dnorm(data, mean = meanX, sd = stdX) # Normal distribution of seq X
)

cat("Mean: ", mean(df$data))

cat("Standard Deviation: ", sd(df$data))

print(df$Y)

summary(df$Y)

#Plot Graph
ggplot() + 
  geom_line(aes(data,Y/2),color='black') + 
  
  ylab('Learning Efforts')+xlab('Performance Measures (# of Trials)') +
  
  ggtitle("SMALL LEARNING CURVE 1-50")


print('#VERSION 2, PLOT LARGER LEARNING CURVE, 1-2000')

#Bell Curve Formula - https://www.tutorialspoint.com/r/r_normal_distribution.htm
data2 = seq(1,2000)# Sequence of numbers
meanX = mean(data2) # Means of seq X
stdX = sd(data2) # Standard deviation of seq X
Y = dnorm(data2, mean = meanX, sd = stdX) # Normal distribution of seq X

#options(max.print = 999999999)

cat("Mean: ", meanX)

cat("Standard Deviation: ", stdX)

print (Y)

#Plot Graph
ggplot() + 
  geom_line(aes(data2,Y),color='red') + 
  
  ylab('Learning Efforts')+xlab('Performance Measures (# of Trials)') +
  
  ggtitle("LARGER LEARNING CURVE 1-2000") # for the main title


print ('#VERSION 3, PLOT TWO CURVES, HiFi and LoFi')

#Bell Curve Formula - put in dataframe
data3 = seq(1,2000)# Sequence of numbers
meanX = mean(data3) # Means of seq X
stdX = sd(data3) # Standard deviation of seq X
Y = dnorm(data3, mean = meanX, sd = stdX) # Normal distribution of seq X

options(max.print = 999999999)

cat("Mean: ", meanX)

cat("Standard Deviation: ", stdX)

print(Y)

#Plot Graph
ggplot() + 
  geom_line(aes(data3,Y),color='blue') + 
  
  geom_line(aes(data3,Y/2),color='red') + 
  
  ylab('Learning Efforts')+xlab('Performance Measures (# of Trials)') +
  
  ggtitle("LARGER (HiFi and LoFi) LEARNING CURVE 1-2000") # for the main title

####################################### REFERENCES ####################################
# [1] http://michaelminn.net/tutorials/r-normal-rank-order/
# [2] https://www.tutorialspoint.com/r/r_normal_distribution.htm
# [3] https://stackoverflow.com/questions/15589601/print-string-and-variable-contents-on-the-same-line-in-r

