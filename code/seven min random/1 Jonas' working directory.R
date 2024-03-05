#this is the first R script that always needs to be run
#all the data cleaning is here 
#if a mistake is found message April before making changes
#Last edited on Dec 12, 2023 by A. R. Martinig 

#axy-assay analysis for Jonas

#Delete previous information stored 
rm(list=ls(all=T))

##set wd to the folder with all your csv's in it
setwd("~/Documents/ECOL 530/axy-assay-comparison/data")
