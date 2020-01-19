actsetwd("/Users/masamiyamaguchi/Documents/GitHub/RepData_PeerAssessment1")
###
library(dplyr)
library(datasets)
library(ggplot2)

filename <- "activity.csv"

## Reading activity file
'{r, echo=TRUE}
activity <- read.csv(filename)
'''

#Exploring the basics of this data
'{r, echo=TRUE}
dim(activity)
head(activity)
'''
