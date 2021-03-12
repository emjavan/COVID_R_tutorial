########################################################
# R code to plot vaccination by ZIP in Texas on map
# Compare with CDC PLACES data
# For Gaby's Class Fri.March.21.2021
# I had a lot of fun
########################################################

# Download libraries before use
library(tidyverse) # Allows us to use ggplot for figures and %>% pipes
library(tigris) # Use to get the shapes of ZIP codes for plotting

# Source of the vaccination data, but it has been converted to a CSV at ZCTA level for easy use
# In general a CSV is better format for long-term access to data, not dependent on any version of Excel
# https://dshs.texas.gov/coronavirus/TexasCOVID19VaccinesbyZIP.xlsx

# col_types = c(ZCTA="c") means I want the column named ZCTA to be read in as a character string instead of as numbers/double
# ZCTA stands for ZIP Code Tabulated Area, since normal ZIP codes are not polygons but mail delivery routes
vac_data = read_csv("vaccine_data_20210301.csv", col_types = c(ZCTA="c"))

# CDC PLACES data of different diseases and unhealthy behaviors
# %>% is a pipe of data to a function from the tidyverse library
# TotalPopulation from 2010 Census
usa_zip_places = read_csv("https://chronicdata.cdc.gov/api/views/kee5-23sr/rows.csv?accessType=DOWNLOAD", col_types = c(ZCTA5="c")) %>%
  rename(ZCTA = ZCTA5) # renaming that column to ZIP so it matches with the ZIP column of vac_data

# Get the ZCTA/ZIP code polygons
zip_geom_us = zctas(cb = TRUE) %>%
  rename(ZCTA = ZCTA5CE10)

# left_join will only keep rows in vac_data since it is on the left and we'll join the other 2 data frames to it
full_data_frame = vac_data %>%
  left_join(zip_geom_us, by="ZCTA") %>%
  left_join(usa_zip_places, by="ZCTA") %>%
  mutate(full_vac_per_pop = People_all_doses/TotalPopulation*100) %>% # We'll add a column which normalizes by population
  drop_na(TotalPopulation) # Remove all the rows with no answer (NA) for total population
  
# Hex codes for colors we'll use in plot
hot_r = c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#800020")

# Plot People with at least 1 vaccine dose on map
ggplot()+
  geom_sf(data=full_data_frame, mapping=aes(geometry=geometry, fill=People_1_dose), 
          size = 0.05, color="black")+
  scale_fill_gradientn(colors = hot_r, na.value = "grey50" )+
  guides(fill = guide_colourbar(frame.colour = "black", ticks.colour="black"))+
  labs(fill = "People with 1 Dose")+
  theme_void()

# Plot the data normalized by population
ggplot()+
  geom_sf(data=full_data_frame, mapping=aes(geometry=geometry, fill=full_vac_per_pop), 
          size = 0.05, color="black")+ # , show.legend = FALSE
  scale_fill_gradientn(colors = hot_r, na.value = "grey50" )+ # , breaks=seq(0, 1200, 200)
  guides(fill = guide_colourbar(frame.colour = "black", ticks.colour="black"))+
  labs(fill = "People with 1 Dose")+
  theme_void()

# Linear regression to see if the total population is a good predictor of the number of doses distributed
ggplot(full_data_frame, aes(x=TotalPopulation, y=People_all_doses))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  labs(x="2010 Total Population", y="People with all doses")+
  theme_bw()

# Are any of the health outcomes correlated with normalized vaccine data?
# At this phase of vaccine roll-out, what would be a better predictor?
ggplot(full_data_frame, aes(x=DIABETES_CrudePrev, y=full_vac_per_pop))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  labs(x="Total Population", y="People with at least 1 dose")+
  theme_bw()
