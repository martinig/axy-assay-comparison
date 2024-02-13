#Andrea's Comment
#Last edited Feb 11, 2024 by J. I. Sanders

# Load packages
library(purrr)
library(data.table)

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

#now that the FAKE data is in a format that is similar to what the axy data will actually be in, we can begin to try to sample the data accordingly

#STEP ONE:
#keep records for every 30 seconds

keep_30<-result%>%
	group_by(squirrel_id) %>%
  	slice(seq(1, n(), by = 30)) %>%
  	arrange(squirrel_id, timestamp)

keep_30

keep_30 %>% filter(squirrel_id== 12678)


#next steps

#STEP TWO A:
#how to have it sample a set amount of time (e.g., 7 minutes of the whole day) - here it could be 1 minute of the "whole (2 minute)" day = continous/consecutive time 

# Function to randomly select one minute of data
consecutive_minute <- function(x) {
  n <- nrow(x)
  if (n < 2) return(NULL)
  
  start_index <- sample(1:(n-1), 1)
  selected_indices <- start_index:(start_index + 1)
  return(x[selected_indices, , drop = FALSE])
}

# Apply the function to each squirrel
consecutive_minute <- keep_30 %>%
  group_split(squirrel_id) %>%
  map(~ consecutive_minute(.x))

# Convert the list of data.frames to a single data.frame
result2 <- bind_rows(consecutive_minute)

# Print the result
result2

result2 %>% filter(squirrel_id== 12678)  %>% arrange(timestamp)

#STEP THREE:
#have it sample a random set amount of time (e.g., 7 minutes randomly spread out across the whole day) - here it could be 1 minute of the "whole (2 minute)" day = noncontinous/nonconsecutive time

# Function to randomly select one minute of data from a two-minute sample
random_one_minute <- function(x) {
  n <- nrow(x)
  if (n < 2) return(NULL)  # Need at least 2 records (1 minute) for sampling
  
  selected_indices <- sample (1:n, 2) # Select 2 random samples (one minute total)
  return(x[selected_indices, , drop = FALSE])
}

# Apply the function to each squirrel
random_one_minute_result <- keep_30 %>%
  group_split(squirrel_id) %>%
  map(~ random_one_minute(.x))

# Convert the list of data.frames to a single data.frame
result3 <- bind_rows(random_one_minute_result)

# Print the result
result3

# Put random samples in numerical order
result3 %>% filter(squirrel_id== 12678)  %>% arrange(timestamp)
