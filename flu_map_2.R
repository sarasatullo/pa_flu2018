
## join into one dataframe and calculate the flu rate per 100K

setwd("~/Documents/Census_stuff/Pa_flu_cases")

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(tigris)
library(leaflet)

# I used a Table Capture browser extension to grab this data from 
#https://www.health.pa.gov/topics/disease/Flu/Pages/2017-18-Flu.aspx
# import in CSV with total flu cases, create new DF with only total cases

flu_18<- read_csv("~/Documents/Census_stuff/Pa_flu_cases/PA_final_flu_18.csv") 

# Filter down to only total cases and 
flu_18<- select(flu_18, County, Total_flu_cases) 



##bring in census PA pop data

library(censusapi)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="0274e47f16183ccd1ec42fb731164c8dfe5d0062")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

# Looking at the various Apis available 
pop_var <- listCensusMetadata(name= "pep/population", vintage = 2017,
                               type= "variables")
head(pop_var)

pop_vars <- listCensusMetadata(name = "pep/population", vintage = 2017, 
                                 type = "geography")
head(pop_vars)

#Getting the Census population data
pa_pop<- getCensus("pep/population",
          vintage=2017,
          vars=c("POP", "GEONAME"),
          region="county:*",
          regionin = "state:42")

# Cleaning up the data to separate the county 
#name from the state because the flu data is by county only
pa_pop_2 <- separate(pa_pop,col = GEONAME, "County", " County, Pennsylvania") %>% 
  select(County,POP) 

pa_pop_2$POP <-as.numeric(pa_pop_2$POP)

# Joining the total flu cases with the population data
  pa_flu_rate <-full_join(flu_18,pa_pop_2, by="County")
  
  glimpse(flu_18)
  glimpse(pa_pop_2)
  
merge(flu_18,pa_pop_2, by = "County", all = TRUE)
  
  # Adjusting for a flu rate per 100,000 residents
pa_flu_rate$per_capita <- round(pa_flu_rate$Total_flu_cases/pa_flu_rate$POP*100000,0)

##bring in county shapefiles at lowest resolution
library(tigris)

counties <- counties("Pennsylvania",cb=T)

library (leaflet)

# starting to build the mpa
counties %>% 
  leaflet() %>% 
addTiles() %>% 
  addPolygons(popup = ~popup_flu)

# Joining the flu rate info wiht the shapefile
pa_flu_rate_merged <- geo_join(counties,pa_flu_rate,"NAME","County")

#Creating a color palette
pal<- colorNumeric("Blues", domain= pa_flu_rate_merged$per_capita)

pa_flu_rate_merged <- subset(pa_flu_rate_merged, !is.na(Total_flu_cases))

#Creating a popup 
popup_flu <-paste0("County: ", as.character(pa_flu_rate_merged$County),
"<br> Flu rate: ", as.numeric(pa_flu_rate_merged$per_capita), "<br> Total cases: ", 
as.numeric(pa_flu_rate_merged$Total_flu_cases))

# Mapping it with the new tiles CartoDB.Positron
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-80.000402, 40.441153, zoom = 6) %>% 
  addPolygons(data = pa_flu_rate_merged , 
              fillColor = ~pal(pa_flu_rate$per_capita), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_flu) %>%
  addLegend(pal = pal, 
            values = pa_flu_rate_merged$per_capita, 
            position = "bottomright", 
            title = "Flu rate per 100,000 residents")

# Create chart to see hardest hit counties 
library(ggplot2)

pa_flu_rate_2<- arrange(pa_flu_rate, desc(per_capita)) %>% 
  top_n(25)


ggplot(data=pa_flu_rate_2) +
  geom_point(mapping=aes(x=County, y=per_capita)) + 
  expand_limits(x = 0, y = 0) +
  geom_abline(intercept=0, col="light gray") 
