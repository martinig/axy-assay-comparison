#scripts for Jonas to just directly pull the data (beccause he does not have access to KRSP cloud), meaning Jonas cannot run scripts 6, 7, and 8
#last updated Jan 16, 2024 by A. R. Martinig

#this needs to be run before you run scripts 9, 10, 11, etc.
clean_assay<-read.csv("clean_assay.csv", header=T) 
clean_axy<-read.csv("clean_axy.csv", header=T) 

#note that I didn't load the personality_all and the axy1 given these are in scripts 2 and 3