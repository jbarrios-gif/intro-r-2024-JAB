library(tidyr)
library(dplyr)
library(xml2)
library(readr)

#read in xml data for wsdot stations metadata
meta_xml <- as_list(read_xml("https://wsdot.wa.gov/Traffic/WebServices/SWRegion/Service.asmx/GetRTDBLocationData"))

meta_df <- as_tibble(meta_xml) %>%
  unnest_longer(RTDBLocationList)

meta_unnest_df <- meta_df %>%
  filter (RTDBLocationList_id == "RTDBLocation") |>
  unnest_wider(RTDBLocationList)

meta_unnest_more <- meta_unnest_df %>%
  unnest(cols = names(.)) %>%
  unnest(cols = names(.)) %>%
  type_convert()

saveRDS(meta_unnest_more, "data/unnested_wsdot_stations_meta.rds")
