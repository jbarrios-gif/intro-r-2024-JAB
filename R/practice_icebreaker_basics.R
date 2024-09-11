#### Practice Problem: Loading and manipulating a data frame ####
# Don't forget: Comment anywhere the code isn't obvious to you!

# Load the readxl and dplyr packages
library(readxl) 
library(dplyr)

# Use the read_excel function to load the class survey data
class_survey_data <- read_excel("data\\icebreaker_answers.xlsx")

# Take a peek!
View(class_survey_data)
class_survey_data
head(class_survey_data)
tail(class_survey_data)

# Create a travel_speed column in your data frame using vector operations and 
#   assignment
class_survey_data$travel_speed <- 
  (class_survey_data$travel_distance / class_survey_data$travel_time)*60

# Look at a summary of the new variable--seem reasonable?
class_survey_data |> summary(travel_speed)

summary(class_survey_data)
boxplot(class_survey_data$travel_speed ~ class_survey_data$travel_mode)
hist(class_survey_data$travel_speed)

# Choose a travel mode, and use a pipe to filter the data by your travel mode
class_survey_data |>
  filter (
    travel_mode == "bus"
  )

bus <- class_survey_data |> filter(travel_mode == "bus")
# Note the frequency of the mode (# of rows returned)


# Repeat the above, but this time assign the result to a new data frame
bus_mode <- class_survey_data |>
  filter (
    travel_mode == "bus"
  )

# Look at a summary of the speed variable for just your travel mode--seem 
#   reasonable?
summary(bus_mode$travel_speed)

# Filter the data by some arbitrary time, distance, or speed threshold
class_survey_data |>
  filter(travel_time <= 25 )

# Stretch yourself: Repeat the above, but this time filter the data by two 
#   travel modes (Hint: %in%)
class_survey_data |>
  filter( travel_mode == "bus" |
            travel_mode == "car")

class_survey_data |> filter (travel_mode %in% c("bus", "cars"))
