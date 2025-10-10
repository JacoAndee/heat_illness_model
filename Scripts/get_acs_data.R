### Fetch API key and search for variables

library(tidycensus)

tidycensus::census_api_key(key = "1d9fe19026f3759c68bc48e2c7ddf1b2b17ec47b",
                           install = T, overwrite = T)
v22 <- load_variables(year = 2022,
                      dataset = "acs5/profile",
                      cache = T)
View(v22)
vars = c("B01001_001E", "B01001_003E", "B01001_027E", "B01001_020E", "B01001_021E", "B01001_022E", "B01001_023E", "B01001_024E", "B01001_025E",
         "B01001_044E", "B01001_045E", "B01001_046E", "B01001_047E", "B01001_048E", "B01001_049E", "B17020_001E", "B08201_002E", "B25034_011E", 
         "B03002_003E", "B03002_004E", "B03002_005E", "B03002_006E", "B03002_012E")
ca_acs <- get_acs(geography = "county",
                  state = "CA",
                  variables = vars,
                  year = 2024,
                  geometry = T,
                  output = "wide",
                  survey = "acs5"
)
View(ca_acs)
str(ca_acs)