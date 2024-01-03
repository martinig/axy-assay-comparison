#Andrea's Comment
#Last edited Jan 2, 2023 by J. I. Sanders

# Load packages
library(purrr)
library(data.table)

# Create a fake dataset to test the program with
axy_test <- axy %>%
  filter(squirrel_id %in% c(12678, 21599)) %>%
  arrange(squirrel_id) %>%
  group_by(squirrel_id) %>%
  mutate(
  timestamp = format(seq(as.POSIXct("12:00:00", format = "%H:%M:%S"), by = "1 sec", length.out = n()), format = "%H:%M:%S"), 
  start_time = as.POSIXct(sample(timestamp, 1), format = "%H:%M:%S"), 
  end_time = start_time + minutes(2), 
  sampled_data = list(filter(timestamp, between(timestamp, start_time, end_time, incbounds = FALSE)))
  ) %>%
  unnest(sampled_data) %>%
  filter(minute(timestamp) %% 2 == 0)

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




