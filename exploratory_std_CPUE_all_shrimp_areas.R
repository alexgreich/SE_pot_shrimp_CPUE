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
library(lubridate)

source("Code/Shrimp_functions.R")

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

all_shrimp_focus_years <- big_shrimp_dataset %>% filter(Season.Ref %in% c("01-02", "02-03", "03-04", "04-05", "05-06", "06-07",
                                                                       "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20",
                                                                       "20-21", "21-22", "22-23", "23-24")) #select for the seasons I want here
unique(all_shrimp_focus_years$Season.Ref)

##make sure I only have pot (9 or 91) gear and only have spot shrimp (for this stat area)
unique(all_shrimp_focus_years$Gear.Code) #just 91 and 9, indicating pots. Good

unique(all_shrimp_focus_years$Species.Code) #multiple species. I'll want to delete the not-spot shrimp later
##shit one of the areas uses coonstripe insetad of spot shrimp. I forget which one.
###FIND THAT OUT!!

###select just the columns that I am interested in for this analysis
#all_shrimp_focus_years <- all_shrimp_focus_years %>% select(Batch.Year, Pre.Print.Ticket, Fish.Ticket.Number, ADFG.Number, Vessel.Name, Date.of.Landing, DFB.Month, Stat.Week, Date.of.Landing, Season, Season.Ref,
 #                                                   Item.Number, 
  #                                                  Stat.Area, Whole.Weight..sum., Pot.Lifts, Species.Code, Batch.Year) #what is item number? the item per fish ticket

#add analysis area names
##first need to wrangle so the function will work
all_shrimp_foucs_years_2 <- all_shrimp_focus_years %>% rename(district = District.Number)#, sub_district = Stat.Area) #renamed district to fit in the function
all_shrimp_foucs_years_2$sub_district <- substr(all_shrimp_foucs_years_2$Stat.Area, 4,5)

all_shrimp_w_analysis_area <- add.analysis.area(all_shrimp_foucs_years_2) #add analysis area
all_shrimp_w_analysis_area <- add.mgmt.unit(all_shrimp_w_analysis_area) #add mgmt area

#quick QC of mgmt area
##TO DO


##################################################################################
#functions (see Tyler's AIGKC code for inspiration)

#wrangle with functions

#IMPORTANT: SOME ENTRIES HAVE POT LIFTS REPLICATED PER ROW. DOES MY FILTERING ACCOMIDATE THAT? WE DO NOT WANT TO SUM THESE

#wragnle spot shrimp by district
unique(all_shrimp_w_analysis_area$district)
unique(all_shrimp_w_analysis_area$Management_unit)

#what do all of the districts correspond to??
#102 112 115 101 103 113 111 107 106 110 108 116 183 104 105 109 114 181


dist_107_pot_shrimp <-  wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, 107)

dist_101_pot_shrimp <- wrangle.spot.shrimp.by.district(all_shrimp_w_analysis_area, 101)

#the only coon district
dist_15_coon_shrimp <- wrangle.coonstripe.shrimp.by.district(all_shrimp_w_analysis_area, 115)


#filter using mgmt unit (more practical)

mgmt_u_District_7 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 7")
View(mgmt_u_District_7)
str(mgmt_u_District_7) # 4639 by 16. 
str(filter(mgmt_u_District_7, Analysis.Area=="Upper Ernest Sound")) #ok that looks about right


##################################################################################################
#Analysis
##do I need to incoporate survey info, or is it already incorporated?

#for entire region 1 (including stray areas?) (are there stray areas?)
###oh a random effects model with nested random effecs (where did I do this before??)
###random effects: area nested within management unit
###glmm, probably


#by Management Unit
##start here
##Wait. This will be a mixed effects model, with the random effects being the analysis area.
### I do not think I will weigh each analysis area. I think it is not relevant, the way I calculate things. The model will know which area has more fishing/more entries
### glmm, probably

#by Area
##See tyler code. Create a function that will do it for me




############################################################################################


#QUESTIONS
###which districts/areas use coons insetad of spot shrimp for stock assessment purposes #district 15 uses coons. #is this dist.... 115?
###what is the VALUE column in the raw data?
###what is the spot shrimp and coon code? Spot is 965, coon is 964