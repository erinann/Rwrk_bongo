library(tidyverse)
library(lubridate)
library(stringr)

dta_bg <- read_csv("bongo_20180604_edit.csv")

names(dta_bg)
#[1] "opsid"                        "zplk_pk_seq"                  "taxa_004"                     "cruise_name"                 
#[5] "station"                      "event_number"                 "net_number"                   "zoo_aliquot"                 
#[9] "zooplankton_count"            "conc_10m2"                    "conc_100m3"                   "vial_number"                 
#[13] "zoo_stage_000"                "zoo_stage_013"                "zoo_stage_020"                "zoo_stage_021"               
#[17] "zoo_stage_022"                "zoo_stage_023"                "zoo_stage_024"                "zoo_stage_028"               
#[21] "zoo_stage_029"                "zoo_stage_030"                "zoo_stage_050"                "zoo_stage_051"               
#[25] "zoo_stage_054"                "zoo_stage_999"                "net_pk_seq"                   "gear"                        
#[29] "gear_volume_filtered"         "sample_jar_count"             "sample_type"                  "flowmeter_calibration_factor"
#[33] "flowmeter_revolutions"        "flag_region"                  "flag_clogging"                "net_comment"                 
#[37] "event_pk_seq"                 "operation"                    "event_date"                   "formated_event_date"         
#[41] "latitude"                     "longitude"                    "region"                       "region_nvl"                  
#[45] "plankton_stratum"             "groundfish_stratum"           "surface_temperature"          "surface_temperature_nvl"     
#[49] "tow_profile"                  "tow_protocol"                 "tow_maximum_depth"            "bottom_depth_max_wire_out"   
#[53] "bio_volume_zoo"               "bio_volume_zoo_1m2"           "bio_volume_zoo_100m3"         "bio_volume_large_conc_1m2"   
#[57] "bio_volume_large_conc_100m3"  "total_zoo_count"              "total_large_zoo_count"        "total_large_zoo_length"      
#[61] "large_zoo_mean_length"        "taxa_name"  

#Need station number by leg.
# do I need to add the trackline number? Going to Say yes to be able to see which lines had bongo tows to match to 
#acoustic data

head(dta_bg$formated_event_date)
#[1] "6/4/2011 9:07" "6/4/2011 9:07" "6/4/2011 9:07" "6/4/2011 9:07" "6/4/2011 9:07" "6/4/2011 9:07"
#check date time - local or UTC??? UTC


dta_bg <- dta_bg %>% 
  mutate(DT_UTC = mdy_hm(formated_event_date), tz = "UTC",
         Date = date(mdy_hm(formated_event_date))) %>%
  arrange(DT_UTC) %>% # to put in time-order
  mutate( leg = factor(case_when(Date <= ymd("2011-06-22") ~ 1,
                         Date >= ymd("2011-06-27") & Date <= ymd("2011-07-15") ~ 2,
                         Date >= ymd("2011-07-20") ~ 3)))

dta_bg %>%
  group_by(leg) %>%
  summarize(n_st = n_distinct(station)) %>%
  ungroup()
# A tibble: 3 x 2
#leg  n_st
#<fctr> <int>
# 1      1    30
# 2      2    30
# 3      3    25


fnct_grp <- read_csv("plankton_fnct_grp_from_Nadine.csv")



fnct_gpr_join <- dta_bg %>%  
  left_join(fnct_grp, by = "taxa_name")

fnct_gpr_join %>% 
  group_by(functional_group) %>%
  summarise(cnt = n()) %>%
  ungroup()

nas <- fnct_gpr_join %>%
  filter(is.na(functional_group))

#need to fix a typo - "Micorcalanus"

test <- dta_bg_jn %>%
  select(station, DT_UTC, functional_group) %>%
  mutate(tm = hms(DT_UTC)) %>%