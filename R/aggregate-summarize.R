#### Aggregating and summarizing data ####

library(readxl)
library(dplyr)
library(ggplot2)

#Read in the excel file
df <- read_excel("Data\\icebreaker_answers.xlsx")
df
summary(df)

# custom summaries of the data fram
df |> summarize(
  avg_dist = mean(travel_distance),
  sd_dist = sd(travel_distance),
  pct60_dist = quantile(travel_distance, prob = 0.60),
  avg_time = mean(travel_time)
)

# if you want an integer, must specify
df %>% mutate(travel_time = as.integer(travel_time))

## now save to an object and save summary for later
# View() will show more precision
df_summ <- df |> summarize(
  avg_dist = mean(travel_distance),
  sd_dist = sd(travel_distance),
  pct60_dist = quantile(travel_distance, prob = 0.60),
  avg_time = mean(travel_time)
)
View(df_summ) # same as clicking df_summ in environment window

# Aggregating and summarizing subsets of a data frame
df <- df |>
  mutate(travel_speed = travel_distance/ travel_time * 60)
df

df |>
  summarize(avg_speed = mean(travel_speed))

#average speed by mode
#you can group by a variable or a combination of variables

df |> group_by(travel_mode) |>
  summarize(avg_speed = mean(travel_speed))

#sort by avg_speed
df |> group_by(travel_mode) |>
  summarize(avg_speed = mean(travel_speed)) |>
  arrange(desc(avg_speed))

#grouped data frame
df |> group_by(travel_mode)

# group by multiple 
df_mode_comma_grp <- df |> group_by(travel_mode, serial_comma) |>
  summarize((avg_speed = mean(travel_speed)))
#by default , summarize will leave data grouped by next higher level by the order in which
# you listed as you continue to summarize
#make sure to ungroup when you continue on with your work
#ungroup()

#frequencies so common there are shortcuts
df |> group_by(serial_comma) |>
  summarize(n =n())

df |> group_by(serial_comma) |>
  tally()

df |> count(serial_comma)
#can arrange this also 
df |> count(serial_comma, sort = T) # High to low
df |> count(serial_comma, sort = F) #Low to high

#calculate a mode split (percentage using each travel mode)
df |> count(travel_mode)
df |> group_by(travel_mode) |>
  summarize(split = n() / nrow(df) * 100 ) |>
  arrange(desc(split))

