#Andrea's Comment
#Last edited Jan 2, 2023 by J. I. Sanders

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

#now that the FAKE data is in a format that is similar to what the axy data will actually be in, we can begin to try to sample the data accordingly

#STEP ONE:
#keep records for every 30 seconds

keep_30<-result%>%
	group_by(squirrel_id) %>%
  	slice(seq(1, n(), by = 30)) %>%
  	arrange(squirrel_id, timestamp)

keep_30

#next steps

#STEP TWO:
#how to have it sample a set amount of time (e.g., 7 minutes of the whole day) - here it could be 1 minute of the "whole (2 minute)" day = continous/consecutive time 

#STEP THREE:
#have it sample a random set amount of time (e.g., 7 minutes randomly spread out across the whole day) - here it could be 1 minute of the "whole (2 minute)" day = noncontinous/nonconsecutive time