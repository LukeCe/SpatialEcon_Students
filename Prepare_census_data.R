# prepare the class data
library(tidycensus)
library(dplyr)
library(stringr)
library(sf)
options(tigris_use_cache = TRUE)

# set key
# key <- ????????????????  <=== get your own 

census_api_key(key, overwrite = FALSE, install = FALSE)

V16 <- load_variables(2016,"acs5")


# variables to extract from the sample
vars_to_load <- c(
  medianage = "B01002_001",
  population = "B01003_001",
  pop_male = "B01001_002",
  pop_female = "B01001_026",
  pop_white = "B01001A_001",
  pop_black = "B01001B_001",
  pop_asia = "B01001D_001",
  medianincome = "B19326_001",
  unemployed = "B23025_005",
  laborforce = "B23025_002",
  incomepercap = "B19301_001",
  ginicoef = "B19083_001",
  cars = "B08015_001",
  bartenders = "B24124_226"
)

# extract the data for diffrent aggregation levels
# Census.CA.tract <- get_acs(
#   geography = "tract",
#   state = "CA",
#   variables = vars_to_load,
#   output = "wide",
#   survey = "acs5",
#   geometry = TRUE
# ) 

Census.county <- get_acs(
  geography = "county",
  variables = vars_to_load,
  output = "wide",
  survey = "acs5",
  geometry = TRUE
) 

Census.state <- get_acs(
  geography = "state",
  variables = vars_to_load,
  output = "wide",
  survey = "acs5",
  geometry = TRUE
) 

# Exclude all states that are not mainland US
# Remove the estimation of the error
states.exclude <- c("Puerto Rico","Hawaii","Alaska")

Census.county49 <- Census.county %>% 
  select(-ends_with("M")) %>%
  mutate(STATE = str_extract(NAME, '\\b[^,]+$')) %>% # macth from the first word that is not followed by comma
  filter(!STATE%in%states.exclude) %>%
  mutate(unemp_rate = unemployedE/populationE,
         car_per_cap = carsE/populationE,
         area_m2 = st_area(geometry) %>% as.numeric())



Census.state49 <- Census.state %>% 
  select(-ends_with("M")) %>%
  filter(!NAME%in%states.exclude) %>%
  mutate(unemp_rate = unemployedE/populationE,
         car_per_cap = carsE/populationE,
         area_m2 = st_area(geometry) %>% as.numeric())


# load("Data/Census/Census.RData")
save(Census.state49, Census.county49, file = "Data/Census/Census.RData")
