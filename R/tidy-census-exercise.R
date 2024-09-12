library(tidycensus) #acts as gateway to the census API for ACS and Decenial data
#for more info:https://walker-data.com/tidycensus/
library(dplyr)
library(tidyr)
library(ggplot2)


####Run on first use if not already stored in r ##
census_api_key("myCensusAPIKey", install = T) #installs into 
#R user environemnt
readRenviron("~/.Renviron")

### User functions
tidy_acs_result <- function(raw_result, include_moe = FALSE){
  #takes tidycensus acs result and returns a wide and tidy table
  if(isTRUE(include_moe)){
    new_df <-raw_result |> pivot_wider(id_cols = GEOID:NAME,
                                       names_from = variable,
                                       values_from = estimate:moe)
  } else{
    new_df <-raw_result |> pivot_wider(id_cols = GEOID:NAME,
                                       names_from = variable,
                                       values_from = estimate)
  }
  return(new_df)
}

###

# get a searchable census variable table
v19 <- load_variables(2019, "acs5")

# ^ look at the begining
v19 %>% filter(grepl("^B08006_", name)) |>
  print(n =25)

#get data for transit, work from home, and total workers
#?get_acs
comm_19_raw <- get_acs(geography = "tract",
                       variables = c(wfh = "B08006_017",
                                     transit ="B08006_008",
                                     tot ="B08006_001" ),
                       county = "Multnomah",
                       state = "OR",
                       year = 2019,
                       survey = "acs5",
                       geometry = FALSE) # can retrieve library(sf)spatial geoms pre-joined

comm_19_raw
#
comm_19 <- comm_19_raw |>
  pivot_wider(id_cols = GEOID,
              names_from = variable,
              values_from = estimate:moe)
comm_19

comm_19 <- tidy_acs_result(comm_19_raw)
comm_19



# get 2022 ACS data
comm_22_raw <- get_acs(geography = "tract",
                       variables = c(wfh = "B08006_017",
                                     transit ="B08006_008",
                                     tot ="B08006_001" ),
                       county = "Multnomah",
                       state = "OR",
                       year = 2022,
                       survey = "acs5",
                       geometry = FALSE) 

comm_22_raw
#applying our function to pivot wider and drop moe's
comm_22 <- tidy_acs_result(comm_22_raw)
comm_22


#did the change of working from home in 2019 and 2022 correlated with transit choice?

#join the years
comm_19_22 <- comm_19 |> inner_join(comm_22, 
                                     by = "GEOID",
                                     suffix = c("_19", "_22")) |>
                 # only the ones matching would get a number

select(-starts_with("NAME"))
comm_19_22

#Now we have a dataset
#create some change variables
comm_19_22 <- comm_19_22 |>
  mutate(wfh_chg = wfh_22 - wfh_19,
         transit_chg = transit_22 - transit_19)
summary(comm_19_22 |> select(ends_with("_chg")))


#plot them
p <- comm_19_22 |> 
  ggplot(aes(x = wfh_chg, y = transit_chg))

p + geom_point()
p + geom_point() + geom_smooth(method = "lm") +
  labs (x = "change in WFH",
        y = "change in transit",
        title = "ACS 2022 vs 2019 (5 - year)") +
  annotate ("text", x= 800, y =50,
            label = paste("r =", 
                          round(cor(comm_19_22$wfh_chg,
                              comm_19_22$transit_chg), 2
            )))

#simple linear (default pearson) correlation
cor(comm_19_22$wfh_chg, comm_19_22$transit_chg) 

#model formula is dependant variable ~ independent variables(1 = x1 = x2)

#model it
m <- lm(transit_chg ~ wfh_chg,
        data = comm_19_22)
summary(m)

#model is an object ready for re-use!
head(m$model) # model comes with data included

scen1 <- comm_19_22 |>
  mutate(wfh_chg = wfh_chg * 1.5)

scen1_pred <- predict(m, newdata = scen1)
#difference in total daily transit impact from 50% increase in wfh
sum(comm_19_22$transit_chg)
sum(scen1_pred)


#update(model, data =) function re-estimate model on a new data
