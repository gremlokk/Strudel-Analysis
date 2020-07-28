# STRUDEL FIDELITY RESEARCH v3-07-25-2020
# Dr. Ritter, Fredrick Ryans, Matthew Koh
# This document is a visual analysis of the strudel data models provided
# in the Ritter paper: fidelity on learning.
# Last updated: 07/25/2020

library(dplyr)
library(ggplot2)
library(tidyr)

options(max.print=1000000)

#Simple Fault Mixed Learning Plot-----------------------------------------------
fig1 = data.frame(
  labels = c("Low", "High", "HHap", "RWhu", "RWHap"),
  data = c(23.3,58.75,0,0,0), 
  sorted_labels = factor(labels, levels = c("Low", "High", "HHap", "RWhu", "RWHap"))#sorted labels (!redundant but works)
)

ggplot(fig1, aes(x = sorted_labels, y = data, color=sorted_labels, group= 1)) + 
  labs(x = "Interface Type",y = "Time Per Trial") +
  geom_line() +
  geom_point(size = 4) +
  ggtitle("Simple Fault Mixed Learning Plot") +
  theme_bw()

#Simple Fault Mixed Learning Plot-----------------------------------------------
fig2 = data.frame(
  labels = c("Low", "High", "HHap", "RWhu", "RWHap"),
  data = c(2.6,1,0,0,0),
  sorted_labels = factor(labels, levels = c("Low", "High", "HHap", "RWhu", "RWHap"))#sorted labels (!redundant but works)
)

ggplot(fig2, aes(x = sorted_labels, y = data, color=sorted_labels, group= 1)) + 
  labs(x = "Interface Type",y = "Time Per Trial") +
  geom_line() +
  geom_point(size = 4) +
  ggtitle("Simple Fault Mixed Learning Plot") +
  theme_gray()

#Complex Learning Curve Plot----------------------------------------------------

alpha = .02
fixedTasks = 26
learnedTasks = 32.75

fig3 = data.frame(
  labels = c(1:10),
  respFunc = fixedTasks + (learnedTasks*((c(1:10))^-alpha))
)

ggplot(fig3, aes(x = labels, y = respFunc, color="High", group= 1)) + 
  labs(x = "Response Times", y = "Blocks of 10 Min.") +
  geom_line() +
  geom_point(size = 4) +
  ggtitle("Complex Learning Curve Plot") +
  theme_light()

fig = data.frame(
  labels = c(1:2000),
  respFunc = fixedTasks + (learnedTasks*((c(1:2000))^-alpha))
)

ggplot(fig, aes(x = labels, y = respFunc, color="Lofi", group= 1)) + 
  labs(x = "Response Times", y = "Blocks of 10 Min.") +
  geom_line() +
  geom_point(size = 2) +
  ggtitle("Complex Learning Curve Plot 1-2000") +
  theme_linedraw()



#Multi-Complex Learning Curve Plot----------------------------------------------

alpha = .20
fixedTasks = 26
learnedTasks = 32.75

fig4 = data.frame(
  labels = c(1:10),
  respFunc = fixedTasks + (learnedTasks*((c(1:10))^-alpha)),#Function working for this
  respFunc2 = c(40.5,37.4,36,35.2,34.6,34.1,33.8,33.5,33.2,33),#Change to working function
  respFunc3 = c(23.3,14.5,13.1,12.4,12,11.7,11.5,11.3,11.1,11)#Change to working function
)

ggplot(fig4, aes(x = labels, color="High")) + 
  labs(x = "Response Times", y = "Blocks of 10 Min.") +
  geom_line(aes(y = respFunc, color="High", group= 1)) +
  geom_line(aes(y = respFunc2, color="Low", group= 2)) +
  geom_line(aes(y = respFunc3, color="Low-to-High", group= 3)) +
  ggtitle("Multi-Complex Learning Curve Plot") +
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 75)) +
  theme_light()


#Draw-Function------------------------------------------------------------------
#This functions draws single lines given a dataframe, x-vals, and y-vals

draw = function(df,x_vals, y_vals){
  
  ggplot(df, aes(x = x_vals, y = y_vals, group= 1)) + 
    labs(x = "Response Times", y = "Blocks of 10 Min.") +
    geom_line() +
    geom_point(size = 4) +
    ggtitle("Complex Learning Curve Plot") +
    theme_light()
}

draw(df = fig4, x = fig4$labels, y = fig4$respFunc)#Draws graph plot


#User-Input---------------------------------------------------------------------
m = readline(prompt="Enter m value: ")
x = readline(prompt="Enter x value: ")
b = readline(prompt="Enter b value: ")



#END----------------------------------------------------------------------------


#SOURCES------------------------------------------------------------------------
# [1] https://www.nngroup.com/articles/power-law-learning/#:~:text=Definition%3A%20The%20power%20law%20of,shape%20of%20a%20power%20law.
# [2] https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
# [3] https://www.tutorialkart.com/r-tutorial/act-for-each-row-in-an-r-data-frame/
# [4] https://www.guru99.com/r-apply-sapply-tapply.html#:~:text=apply()%20takes%20Data%20frame,be%20used%20over%20a%20matrice.&text=The%20simplest%20example%20is%20to%20sum%20a%20matrice%20over%20all%20the%20columns.
# [5] https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# [6] https://www.mathsisfun.com/sets/functions-common.html
# [7] https://www.valamis.com/hub/learning-curve
# [8] https://www.statmethods.net/management/functions.html
# [9] https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/factor
# [10]
