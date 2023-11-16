##exploring the data##
#Alex Reich
#initiated: 11/14/23
#last worked on: 11/14/23

#argh why is github not connecting?

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
unique(fish_tickets_ernest$Season.Ref) # we want 01-02 and higher
foucs_years_ernest <- fish_tickets_ernest %>% filter(Season.Ref==c("01-02",
                                                                   "02-03"))#unfinished, need to select for the seasons I want here

##make sure I only have pot (9 or 91) gear and only have spot shrimp (for this stat area)

###wrangle so that I have a new column called Number_of_vessels, which will represent the number of vessels in a current season


###wrangle: sum the weight(sum) per each fish ticket, while keeping the MAX of pots for that fish ticket. This will insure that pots are not counted twice/multiple times





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