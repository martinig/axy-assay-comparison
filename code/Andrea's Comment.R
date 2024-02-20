#Andrea's Comment
#Last edited Feb 20, 2024 by A. R. Martinig

# Load packages
library(purrr)
library(data.table)

################################################
#here we create a fake dataset that is ~ similar to the real axy data to test our code on
################################################

# Create a fake dataset to test the program with
axy_test <- axy %>%
  filter(squirrel_id %in% c(12678, 19890, 21599)) %>%
  arrange(squirrel_id, axy_date) %>%
  group_by(squirrel_id) %>%
  mutate(
    timestamp = format(seq(as.POSIXct("12:00:00", format = "%H:%M:%S"), by = "1 sec", length.out = n()), format = "%H:%M:%S"), 
    start_time = as.POSIXct(sample(timestamp, 1), format = "%H:%M:%S"), 
    end_time = start_time + minutes(2)) %>%
  select(-c(start_time, end_time))

head(axy_test)


#creating a function to generate the data in a way that is usable for us to then test the sampling code on

# Create a data.table for efficient grouping
dt <- as.data.table(axy_test)

# Define function to randomly generate consecutive samples
generate_sample <- function(x) {
  n <- nrow(x)
  if (n < 120) return(NULL)

  start_indices <- sample(1:(n-119), 1)
  selected_indices <- start_indices:(start_indices + 119)
  return(x[selected_indices, , drop = FALSE])
}

# Apply the function to each squirrel
result <- dt[, generate_sample(.SD), by = squirrel_id]

# Print the result
result

result %>% filter(squirrel_id== 12678)  %>% arrange(timestamp)


################################################
#this is where we have created the functions to organize the data in a format that will allow us to pull out 7 minutes of data consecutively or randomly
################################################



#now that the FAKE data is in a format that is similar to what the axy data will actually be in, we can begin to try to sample the data accordingly

#STEP ONE:
#keep records for every 30 seconds

keep_30<-result%>%
#Emily: unhashtag the below script with the real data
	#filter(!tod=="night", #remove night axy data because focals are never at night 
	#axy_month %in% c(5:9)) %>% #keep only records from the same months as focals (May through September)
	group_by(squirrel_id) %>%
  	slice(seq(1, n(), by = 30)) %>%
  	arrange(squirrel_id, timestamp) 

keep_30

keep_30 %>% filter(squirrel_id== 12678)



#STEP TWO:
#how to have it sample a set amount of time (e.g., 7 minutes of the whole day) - here it could be 1 minute of the "whole (2 minute)" day = continous/consecutive time 

# Function to consecutively select one minute of data
consecutive_sample <- keep_30 %>%
	  arrange(squirrel_id, timestamp) %>%
	  group_by(squirrel_id) %>% #add in axy_date when we have more data
	
	#### Emily: THIS SHOULD BE 1:14 for 7 minutes of data
	
	slice(1:2) #keep the first two rows (this means 1 minute of data) # If you want to specify a starting row other than the first row, you can use slice() accordingly. # For example, to start from the third row for each squirrel_id: # slice(3:4)

consecutive_sample %>% filter(squirrel_id== 12678)  %>% arrange(timestamp)


#STEP THREE:
#have it sample a random set amount of time (e.g., 7 minutes randomly spread out across the whole day) - here it could be 1 minute of the "whole (2 minute)" day = noncontinous/nonconsecutive time

# Function to randomly select one minute of data
random_sample <-  keep_30 %>%
	 group_by(squirrel_id) %>% #add in axy_date when we have more data

	#### Emily: THIS SHOULD BE size=14 for 7 minutes of data

	sample_n(size=2) #keep two rows (this means 1 minute of data)

random_sample %>% filter(squirrel_id== 12678) %>% arrange(timestamp)
