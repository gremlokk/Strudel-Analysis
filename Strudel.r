# STRUDEL FIDELITY RESEARCH v1-07-20-2020
# Dr. Ritter, Fredrick Ryans, Matthew Koh
# This document is a visual analysis of the strudel data models provided
# in the Ritter paper: fidelity on learning.
# Last updated: 07/16/2020

library(dplyr)
library(ggplot2)
library(tidyr)

options(max.print=1000000)

#PRELIMINARY PLOTS -------------------------------------------------------------

#RESPONSE FUNCTION GRAPH
df = data.frame(
  id = c(1:50),
  values = runif(50, min=0, max=10)#Generates Random Values N=2000, i = 0-61
)

respFunc = function(x) {
  alpha = .02
  fixedTasks = 1
  learnedTasks = x[2]
  trial = 10
  respTime = fixedTasks + (learnedTasks*((trial)^-alpha))
}

Y = apply(df, 1, respFunc)

#Create function data visualization 1-2000
#Takes paramater of log
ggplot(data = df, aes(x = id, y = Y, color=Y)) + 
  labs(x = "Response Time(s)",y = "Learning Rate") +
  geom_line() +
  geom_point() +
  ggtitle("Response Function Graph") +  
  theme_classic()

print(Y)
summary(Y)

#RANDOM GRAPHS
efforts = runif(10, min=0, max=10)
rGraph = data.frame(
  eff = efforts,
  Y = c(1:10)
)

ggplot(data = rGraph, aes(x = efforts, y = Y, color=Y)) +
  labs(x = "Learning Efforts (# Attempts)",y = "Performance") +
  geom_line() +
  geom_point() + 
  ggtitle("Random Graph") +
  theme_gray()

#LEARNING CURVE 1-50
v1 = data.frame(
  learning_efforts = c(1:50),
  performance =  (c(1:50))^(1/2) #sqrt function x^1/2
)

ggplot(data = v1, aes(x = learning_efforts, y = performance, color = learning_efforts)) +
  labs(x = "Learning Efforts (# Attempts)",y = "Performance") +
  geom_line() +
  geom_point() + 
  ggtitle("Learning Curve 1-50") +
  theme_classic()

#LEARNING CURVE 1-2000
v2 = data.frame(
  learning_efforts = c(0:2000),
  performance =  (c(0:2000))^-(1/2) #sqrt function x^1/2
)

ggplot(data = v2, aes(x = learning_efforts, y = performance, color=performance)) +
  labs(x = "Learning Efforts (# Attempts)",y = "Performance") +
  geom_line() +
  geom_point() + 
  ggtitle("Learning Curve 1-2000") +
  theme_grey()


#LN(X) Graph
v3 = data.frame(
  learning_efforts = c(0:50),
  performance =  log(c(0:50))
)

ggplot(data = v3, aes(x = learning_efforts, y = performance, color=performance)) +
  labs(x = "Learning Efforts (# Attempts)",y = "Performance") +
  geom_line() +
  geom_point() + 
  ggtitle("LN(X) Plot") +
  theme_grey() 

#END----------------------------------------------------------------------------



#VARIABLES----------------------------------------------------------------------

alpha = .02
fixedTasks = 1
learnedTasks = 1
trial = 5

#FORMULAS-----------------------------------------------------------------------

#Recommendations by Martin: 7/24/20
#hifi + lofi should have their own matrix/ data-structure to generate numbers... 
#Only apply learning rate to learning task not fixed tasks
respTime = fixedTasks + (learnedTasks*((trial)^-alpha))

#FIGURES------------------------------------------------------------------------

rand_values = runif(2000, min=0, max=61)#Generates Random Values N=2000, i = 0-61

fig1 = data.frame(
  ui_combo = 	c("Lhu","LHap","Mhu","Mhap","Hhu","HHap","RWhu","RWHap"),												
  id = c(1:8),
  time_per_trial = c(11.75,12.95,15.95,15.95,13.75,35.4,60.25,60.25), 
  stringsAsFactors = FALSE
)

fig2 = data.frame(
  ui_combo = 	c("Lhu","LHap","Mhu","Mhap","Hhu","HHap","RWhu","RWHap"),												
  id = c(1:8),
  time_per_trial = c(4.6,4.6,3.8,3.8,1.7,1.7,1,1)
)

fig3 = data.frame(
  time_blocks = c(1:10),
  lofi_resp_time = c(11.8,5.5,4.3,3.8,3.5,3.2,3.1,2.9,2.8,2.7),
  hifi_resp_time = c(13.75,7.807,5.544,4.756,4.316,4.022,3.805,3.635,3.497,3.382)
)

#PLOT VISUALIZATION-------------------------------------------------------------

#results order plot in alphabetical order, we want to keep natural order
ggplot(fig1, aes(ui_combo,time_per_trial, group = 1, color=ui_combo))+
  geom_line(color='black',size=1) +
  geom_point(size = 4) +
  ylab("Times Per Trial (s)") +
  xlab("User Interface Combination") +
  ggtitle("Figure 1") +
  theme_gray()

#results order plot in alphabetical order, we want to keep natural order
ggplot(fig2, aes(ui_combo,time_per_trial, group = 1, color=ui_combo))+
  geom_line(color='black',size=1) +
  geom_point(size = 4) +
  ylab("Times Per Trial (s)") +
  xlab("User Interface Combination") +
  ggtitle("Figure 2") +
  theme_minimal()

#FIGURE 3
ggplot(fig3, aes(x=time_blocks)) +
  geom_line(aes(y=lofi_resp_time, color="Lofi"),size=1) + 
  geom_line(aes(y=hifi_resp_time,color="Hifi"),size=1) + 
  labs(x = "Response Time(s)",y = "10-Min Blocks") +
  ggtitle("Figure 3: HiFi and LoFi Curves") +
  theme_bw()

#END----------------------------------------------------------------------------

#Updates: Make more
#Ask for user input
#Create visualization functions for graphing (takes some data,visualize it)

#SOURCES------------------------------------------------------------------------
# [1] https://www.nngroup.com/articles/power-law-learning/#:~:text=Definition%3A%20The%20power%20law%20of,shape%20of%20a%20power%20law.
# [2] https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
# [3] https://www.tutorialkart.com/r-tutorial/act-for-each-row-in-an-r-data-frame/
# [4] https://www.guru99.com/r-apply-sapply-tapply.html#:~:text=apply()%20takes%20Data%20frame,be%20used%20over%20a%20matrice.&text=The%20simplest%20example%20is%20to%20sum%20a%20matrice%20over%20all%20the%20columns.
# [5] https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# [6] https://www.mathsisfun.com/sets/functions-common.html
# [7] https://www.valamis.com/hub/learning-curve
# [8] https://www.statmethods.net/management/functions.html
# [9] 
# [10]
