###
##Focus Wrangle
##I'll explore different data wrangling options, see if they are the same.

library(tidyverse)



#which is the one area/district that uses coonstripe shrimp instead of pot shrimp for analysis?
##should be both in my notes and in the doucment that Max sent me.

#in district 15, we care about coons, not spot shrimp. Everywhere else, we care about spot shrimp

#wrangle setup
all_shrimp <- read.csv("Data/Complete pot shrimp dataset.csv")



#wrangle in 3 ways: by area (smallest), by district (medium), and all at once (large)
##can do it all 3 ways and see if comparible
##I've already done this method for Ernest sound in the spot_shrimp_ernest_clean_4.R file. I can run the other ways, filter for earnest, and see if the same
#actually, seems like the most logical way to clean is by district (the medium option). Thats how things are done on the Pot Shrimp Assessment Instructions



#CAUTION:
##there is a cleanedFT excel file
##check for duplicates in the TICKET_NO column? Is that my problem?
####likely a function in R
####"make fixes to the Analysis Pots column?
#EXTRA CAUTION
##some entries have pots




#Questions
##MAx, can you send me the D1 cleanedFT excel file?
###(might need to reference in my own data cleaning)
##Looks like the Pot Shrimp Assessment Instructions has some data cleaning involved, for each year. AM I WORKING WITH THE CLEAN DATA OR THE MESSY DATA?
###THERE IS ABOLUTELY NO REASON WHY I SHOULD HAVE THE MESSY DATA
##what do the district numbers correspond to? !!!!!!!!!!! I have: 102 112 115 101 103 113 111 107 106 110 108 116 183 104 105 109 114 181


###############################
#ok soooo let's clean by distrit
unique(all_shrimp$District.Number)
unique(all_shrimp$Region) #um are there not only 61 or so areas
all_shrimp_foucs_years <-all_shrimp %>% filter(Season.Ref %in% c("01-02", "02-03", "03-04", "04-05", "05-06", "06-07",
                                                       "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20",
                                                       "20-21", "21-22", "22-23", "23-24"))
unique(all_shrimp_foucs_years$Region)
unique(all_shrimp_foucs_years$District.Number)
unique(all_shrimp_foucs_years$Stat.Area) #why are there so many stat areas? Maybe because multiple species

#what do the district numbers correspond to?
##idk


#which stat area is upper enest sound again? 101-70?

##let'start with 101


#district 101
names(all_shrimp_foucs_years)
#View(ordered(unique(all_shrimp_foucs_years$Stat.Area.1)))


all_shrimp_foucs_years_2 <- all_shrimp_foucs_years %>% rename(district = District.Number)#, sub_district = Stat.Area) #renamed district to fit in the function
all_shrimp_foucs_years_2$sub_district <- substr(all_shrimp_foucs_years_2$Stat.Area, 4,5)
unique(all_shrimp_foucs_years_2$sub_district)

all_shrimp_w_analysis_area <- add_analysis_area(all_shrimp_foucs_years_2)
#IT worked!!
#View(all_shrimp_w_analysis_area)
#ok so I need another column called sub_district or add an analysis area column. Back to oceanak we go.

#ohh. might need to filter for shrimp first??
