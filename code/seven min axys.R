#script to pull out seven minutes of data from the accelerometer data
#Written by A.R.Martinig, E.K.Studd
#Last edited Mar 3, 2024 by E.K.Studd

# Load packages ####
library(purrr)
library(data.table)
library(tidyverse)
library(lubridate)

## Files must include these columns
#id
#datetime
#tod
#date
#month(date)
#All


####CREATE FUNCTION ####
### Create a function to run on each axy file that will pull out 7 minutes of data per day (at 30 second intervals) in both a consecutive and a random fashion 

focal_sampler<-function(axyfile){

#### LOAD FILE

data<-fread(axyfile)
#fix IDs to match file name (a couple are wrong in the original files. This corrects them)
data$id<-sub("squirrel", "", scan(text=axyfile, what=character(), sep="_", quiet=T)[2])
#import datetime column as a time format
data$datetime<-ymd_hms(data$datetime)

# filter data to remove night measurements and non-summer data
data<-data%>%
	filter(!tod=="night", #remove night axy data because focals are never at night 

		month(date) %in% c(5:9)) #keep only records from the same months as focals (May through September)

## IF FILE HAS SUMMER DATA, proceed to subsample

if(nrow(data)>0){
  ################################################-
  #this is where we organize the data in a format that will allow us to pull out 7 minutes of data consecutively or randomly
  ################################################-
  #STEP ONE:
  #keep records for every 30 seconds
  keep_30<- data %>% 
    group_by(id) %>%
  	slice(seq(1, n(), by = 30)) %>%
  	arrange(id, datetime) 

#STEP TWO:
#how to have it sample a set amount of time (e.g., 7 minutes of the whole day) = continous/consecutive time 

# generate a random start point for the focal for each day
keep_30<-keep_30 %>% 
  group_by(date) %>% 
  mutate(startRow=floor(runif(1, min=1, max=n()-14))) #random integer between 1 and 14 below the max number of rows of data for each day

# Function to consecutively select one minute of data
consecutive_sample <- keep_30 %>%
	  arrange(id, datetime) %>%
	  group_by(id, date) %>% 
	  slice(max(startRow):(max(startRow)+13)) #keep the 14 rows starting at the randomly choosen start row #.
consecutive_sample<-select(consecutive_sample, -startRow)

#STEP THREE:
#have it sample a random set amount of time (e.g., 7 minutes randomly spread out across the whole day) = noncontinous/nonconsecutive time

# Function to randomly select one minute of data
random_sample <-  keep_30 %>%
	 group_by(id, date) %>% #add in axy_date when we have more data
	sample_n(size=14) #keep 14 rows (this means 7 minutes of data)

random_sample<-select(random_sample, -startRow)

#save the outputs
write.csv(keep_30, paste("~/Desktop/Data_behavior_7minfocals/", unlist(strsplit(axyfile, "[.]"))[1], "keep_30.csv", sep=""), row.names=F)
write.csv(consecutive_sample, paste("~/Desktop/Data_behavior_7minfocals/", unlist(strsplit(axyfile, "[.]"))[1], "consecutive_sample.csv", sep=""), row.names=F)
write.csv(random_sample, paste("~/Desktop/Data_behavior_7minfocals/", unlist(strsplit(axyfile, "[.]"))[1], "random_sample.csv", sep=""), row.names=F)

print("file sampled")
}


### IF FILE HAS NO DATA, print message informing of that. 
if(nrow(data)==0){
  print("0 summer data")
}
}


##### RUN FUNCTION ON ALL AXY FILES ####
setwd("~/Desktop/Data_behaviorClassCompile_20240109/")
files<-dir(pattern="*TOD.csv")
files[] %>% map(focal_sampler)


#### COMPILE ALL THE SUBSAMPLED DATA INTO ONE FILE ####
#for consecutive sample
setwd("~/Desktop/Data_behavior_7minfocals")
files=dir(pattern="*consecutive_sample.csv")
consec<-files %>% map(fread) %>% reduce(rbind)
consec<-select(consec, id, date, datetime, tod, odba, Nest, All)
write.csv(consec, "allaxy_consecutive_7minute_sample.csv", row.names=F)

# for random sample
files=dir(pattern="*random_sample.csv")
random<-files %>% map(fread) %>% reduce(rbind)
random<-select(random, id, date, datetime, tod, odba, Nest, All)
write.csv(random, "allaxy_random_7minute_sample.csv", row.names=F)
