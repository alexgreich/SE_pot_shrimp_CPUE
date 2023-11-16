##exploring the data##
#Alex Reich
#initiated: 11/14/23
#last worked on: 11/16/23

#github repository is functional for this project as of 11/16/23


#load libraries
library(tidyverse) #includes dplyr
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(viridis)


#intially, I'm looking at Fish Ticket and/or Logbook data just from Ernest Sound 
fish_tickets_ernest <- read.csv("Data/Ernest_sound_fish_tickets.csv")


names(fish_tickets_ernest)
#Pounds..sum and Whole.Weight..sum. should not be identical?
identical(fish_tickets_ernest$Pounds..sum., fish_tickets_ernest$Whole.Weight..sum.)
#yup, they're identical

#anyway...


#####################
#Data wrangling
#####################


###wrangle for my focus years
unique(fish_tickets_ernest$Season.Ref) # we want 01-02 and higher for now

focus_years_ernest <- fish_tickets_ernest %>% filter(Season.Ref %in% c("01-02", "02-03", "03-04", "04-05", "05-06", "06-07",
                                                                       "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20",
                                                                       "20-21", "21-22", "22-23")) #select for the seasons I want here
unique(focus_years_ernest$Season.Ref)

##make sure I only have pot (9 or 91) gear and only have spot shrimp (for this stat area)
unique(focus_years_ernest$Gear.Code) #just 91, indicating pots. Good

unique(focus_years_ernest$Species.Code) #there's multiple here. We just want spot shrimp (965)
focus_years_ernest <- focus_years_ernest %>% filter(Species.Code == 965)
unique(focus_years_ernest$Species.Name) #good, it's just spot shrimp now


###select just the columns that I am interested in for this analysis
focus_years_ernest <- focus_years_ernest %>% select(Batch.Year, Pre.Print.Ticket, Fish.Ticket.Number, ADFG.Number, Vessel.Name, Event.Date, DOL.Month, Stat.Week, Date.of.Landing, Season, Season.Ref,
                                                    Item.Number, 
                                                    Stat.Area, Whole.Weight..sum., Pots) #what is item number? the item per fish ticket

###wrangle so that I have a new column called Number_of_vessels, which will represent the number of vessels in a current season
focus_years_ernest <- focus_years_ernest %>% 
  group_by(Season.Ref) %>% #grouping by season (year)
  mutate(vessel_count = n_distinct(ADFG.Number)) %>% #count the unique # of vessels (by ADFG number)
  ungroup() #ungroup


###wrangle: sum the weight(sum) per each fish ticket, while keeping the MAX of pots for that fish ticket. This will insure that pots are not counted twice/multiple times
#there are some NA's but I dont want to deal with them
sum_shrimp_ernest <- focus_years_ernest %>%
  group_by(Fish.Ticket.Number, Season.Ref, ADFG.Number, Vessel.Name, DOL.Month, Stat.Week) %>%
  summarise(
    total_weight = sum(Whole.Weight..sum.),
    max_pots = max(Pots)) %>%
  ungroup()

str(sum_shrimp_ernest)
str(focus_years_ernest)


###############################
#Exploratory data analysis
#############################





##WIP below##


#############################################################3
#I'm avoiding OceanAK altogether and importing straight into R
###########################################################
library(odbc)
library(DBI)
library(RODBC)
library(tidyverse)


# check that crab survey database is listed as a data source
odbcDataSources() #sign. I'll need to ask Caitlin and/or OIT for help

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "driver_name",
                      DBQ = "server_host:port/service_name",
                      UID = "user_id",
                      PWD = "password")
