## import in CSV with total flu cases, create new DF with only total cases
## get census population estimates for PA counties
## join into one dataframe and calculate the flu rate per 100K

setwd("~/Documents/Census_stuff")

library(readr)
library(dplyr)

flu_18<- read_csv("~/Documents/Census_stuff/PA_final_flu_18.csv") 

flu_18<- select(flu_18, County, Total_flu_cases) 

flu_18 %>% 
  (tolower(County))

##bring in census PA pop data

install.packages("censusapi")
library(censusapi)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="0274e47f16183ccd1ec42fb731164c8dfe5d0062")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

apis <- listCensusApis()

View(apis)

pop_var <- listCensusMetadata(name= "pep/population", vintage = 2017,
                               type= "variables")
head(pop_var)

pop_vars <- listCensusMetadata(name = "pep/population", vintage = 2017, 
                                 type = "geography")
head(pop_vars)

pa_pop<- getCensus("pep/population",
          vintage=2017,
          vars=c("POP", "GEONAME"),
          region="county:*",
          regionin = "state:42")

library(stringr)
library(dplyr)
library(tidyr)

pa_pop_2 <- separate(pa_pop,col = GEONAME, "County",", Pennsylvania") %>% 
  select(County,POP)

pa_pop_2$POP <-as.numeric(pa_pop_2$POP)
  
pa_flu_rate <-full_join(flu_18,pa_pop_2, by="County")


