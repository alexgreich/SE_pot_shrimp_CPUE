###
#Alex Reich
#exploratory script: shrimp standardized CPUE
# expanding upon Spot_shrimp_ernest_clean_4.R to incorporate overall std cpue, district cpue, and area cpue. Standardized CPUE's for all of these
##side note: Does it make sense to do all this???


##The Setup###################################################################################################
#load libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

#load data
one <- read.csv("Data/Shrimp Tickets before and including 2004.csv")
two <- read.csv("Data/Shrimp Tickets 2005 to 2017.csv")
three <- read.csv("Data/Shrimp Tickets 2018 to 2024.csv")

#combine data
big_shrimp_dataset <- rbind(one,two,three)
str(big_shrimp_dataset) #looks about right
#write.csv(big_shrimp_dataset, "Data/Complete pot shrimp dataset.csv") #this should exist SOMEWHERE in a usable format...

##the Wrangle#################################################################################################################################
#wrangle data
##see earnest code, and Max's data analysis setup word doc code
##something to do with pot lifts. Make sure you wrangle that in a way not to create errors.
##THIS IS REALLY IMPORTANT
##figure out a way to QC to make sure you did it right.

#seems like I chose to use 01-02 and higher for now
unique(big_shrimp_dataset$Season.Ref) # we want 01-02 and higher for now

big_shrimp_focus_years <- big_shrimp_dataset %>% filter(Season.Ref %in% c("01-02", "02-03", "03-04", "04-05", "05-06", "06-07",
                                                                       "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20",
                                                                       "20-21", "21-22", "22-23", "23-24")) #select for the seasons I want here
unique(big_shrimp_focus_years$Season.Ref)

##make sure I only have pot (9 or 91) gear and only have spot shrimp (for this stat area)
unique(big_shrimp_focus_years$Gear.Code) #just 91 and 9, indicating pots. Good

unique(big_shrimp_focus_years$Species.Code) #multiple species. I'll want to delete the not-spot shrimp later
##shit one of the areas uses coonstripe insetad of spot shrimp. I forget which one.
###FIND THAT OUT!!

###select just the columns that I am interested in for this analysis
big_shrimp_focus_years <- big_shrimp_focus_years %>% select(Batch.Year, Pre.Print.Ticket, Fish.Ticket.Number, ADFG.Number, Vessel.Name, Date.of.Landing, DFB.Month, Stat.Week, Date.of.Landing, Season, Season.Ref,
                                                    Item.Number, 
                                                    Stat.Area, Whole.Weight..sum., Pot.Lifts, Species.Code, Batch.Year) #what is item number? the item per fish ticket

#IMMEDIATELY BELOW (THE NEXT TWO BLOCKS) ARE WHERE WE REAAAAALY WANT TO PAY ATTENTION WITH THE QC
###wrangle so that I have a new column called Number_of_vessels, which will represent the number of vessels in a current season
big_shrimp_focus_years <- big_shrimp_focus_years %>% 
  group_by(Season.Ref) %>% #grouping by season (year)
  mutate(vessel_count = n_distinct(ADFG.Number)) %>% #count the unique # of vessels (by ADFG number)
  ungroup() #ungroup
####QC PLEASE!!

###wrangle: sum the weight(sum) per each fish ticket, while keeping the MAX of pots for that fish ticket. This will insure that pots are not counted twice/multiple times
#there are some NA's but I dont want to deal with them <- UM, INDENTIFY/ELABORATE PLEASE

sum_shrimp_focus_years <- big_shrimp_focus_years %>%
  mutate(Pot.Lifts = replace_na(Pot.Lifts, 0)) %>%
  group_by(Fish.Ticket.Number, Species.Code, Stat.Area) %>% #also group by stat area for analysis areas with multiple stat areas. OOH. SHOULD I GROUP BY ANALYSIS AREA ALSO?? I THHINK MAYBE SO!
  summarise(
    total_weight = sum(Whole.Weight..sum.), #WHAT THE HELL AM i DOING HERE, AND IS IT RIGHT?? IS THIS OK TO DO BEFORE FILTERING OUT OTHER SHRIMP SPECIES??
    max_pots = max(Pot.Lifts),
    ADFG.Number = max(ADFG.Number),
    Season.Ref=max(Season.Ref),
    Vessel.Name = max(Vessel.Name),
    DOL.Month= max(DFB.Month),
    Stat.Week=max(Stat.Week), 
    Batch.Year=max(Batch.Year), #add Event date, date of landing, or vessel count here? #change from V1 ... vessel count is a continuous variable (integer... GAM time?)
    Event.Date=max(Date.of.Landing),
    vessel_count=max(vessel_count) #that should do it.
  ) %>%
  ungroup() #this has pots and total weight for all shrimp species

max_pots_total <- sum_shrimp_focus_years %>% #get # pots fpr each fish ticket
  group_by(Fish.Ticket.Number) %>%
  summarise(
    max_pots_2 = max(max_pots)
  )%>%
  ungroup()

sum_spot_shrimp_all <- sum_shrimp_focus_years %>% #this is where NA's appear for the season.ref code. need to figure out why.
  #filter(Species.Code == 965) %>%
  right_join(max_pots_total) %>% #probs has to do with the join, the NA's that is
  filter(Species.Code == 965) %>% #switched this over here, no more NAs'
  select(-max_pots)

#LETS THINK ABOUT POTENTIAL CAVEATS TO THIS METHOD.
##DID IT WRANGLE RIGHT? RE-VISIT TOMORROW.

##################################################################################
#functions (see Tyler's AIGKC code for inspiration)


###############################################################################
#Analysis!


#QUESTIONS
###which districts/areas use coons insetad of spot shrimp for stock assessment purposes
###what is the VALUE column in the raw data?