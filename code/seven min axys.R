#script to pull out seven minutes of data from the accelerometer data
#Last edited Feb 27, 2024 by A. R. Martinig

# Load packages
library(purrr)
library(data.table)

#I am not sure what format the data is in on your end, however to be compatible with this script, it needs to have these columns:
squirrel id
time (we call this timestamp)
tod (this is to remove the nighttime axy data - see note about just filtering out the records that are taken during the night)
axy_date (ymd format)
axy_month (month format)
plus feed, forage, nestmove, nestnotmove, notmoving, travel

################################################
#this is where we have created the functions to organize the data in a format that will allow us to pull out 7 minutes of data consecutively or randomly
################################################

#STEP ONE:
#keep records for every 30 seconds

keep_30<-data%>%
	filter(!tod=="night", #remove night axy data because focals are never at night 

####################
####EMILY: consider replacing this with a time filter to remove the nighttime measurements
####################

		axy_month %in% c(5:9)) %>% #keep only records from the same months as focals (May through September)
	group_by(squirrel_id) %>%
  	slice(seq(1, n(), by = 30)) %>%
  	arrange(squirrel_id, timestamp) 

keep_30

keep_30 %>% filter(squirrel_id== 12678)



#STEP TWO:
#how to have it sample a set amount of time (e.g., 7 minutes of the whole day) = continous/consecutive time 

# Function to consecutively select one minute of data
consecutive_sample <- keep_30 %>%
	  arrange(squirrel_id, timestamp) %>%
	  group_by(squirrel_id, axy_date) %>% 
	  slice(1:14) #keep the first 14 rows (this means 7 minutes of data) # If you want to specify a starting row other than the first row, you can use slice() accordingly. # For example, to start from the third row for each squirrel_id: # slice(3:4)

consecutive_sample %>% filter(squirrel_id== 12678)  %>% arrange(timestamp)


#STEP THREE:
#have it sample a random set amount of time (e.g., 7 minutes randomly spread out across the whole day) = noncontinous/nonconsecutive time

# Function to randomly select one minute of data
random_sample <-  keep_30 %>%
	 group_by(squirrel_id) %>% #add in axy_date when we have more data
	sample_n(size=14) #keep 14 rows (this means 7 minutes of data)

random_sample %>% filter(squirrel_id== 12678) %>% arrange(timestamp)

#save the outputs
write.csv(keep_30, "keep_30.csv")
write.csv(consecutive_sample, "consecutive_sample.csv")
write.csv(random_sample, "random_sample.csv")
