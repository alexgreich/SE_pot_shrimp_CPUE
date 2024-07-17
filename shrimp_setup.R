###
##Shrimp setup
#author: Alex Reich 
##created (aka edited into this form): 07/17/24
##last edits: 07/17/24


##The Setup###################################################################################################
#load libraries
library(tidyverse)
#library(ggplot2)
#library(cowplot)
library(lubridate)

source("Code/Shrimp_functions.R") #load functions

#load data
one <- read.csv("Data/Shrimp Tickets before and including 2004.csv")
two <- read.csv("Data/Shrimp Tickets 2005 to 2017.csv")
three <- read.csv("Data/Shrimp Tickets 2018 to 2024.csv")

#combine data
big_shrimp_dataset <- rbind(one,two,three)
str(big_shrimp_dataset) #looks about right
#write.csv(big_shrimp_dataset, "Data/Complete pot shrimp dataset.csv") #this should exist SOMEWHERE in a usable format...

##the Wrangle#################################################################################################################################
#Years 01-02 and higher for now
unique(big_shrimp_dataset$Season.Ref)

all_shrimp_focus_years <- big_shrimp_dataset %>% filter(Season.Ref %in% c("01-02", "02-03", "03-04", "04-05", "05-06", "06-07",
                                                                       "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20",
                                                                       "20-21", "21-22", "22-23", "23-24")) #select for the seasons I want here
unique(all_shrimp_focus_years$Season.Ref)


unique(all_shrimp_focus_years$Gear.Code) #just 91 and 9, indicating pots. Good

unique(all_shrimp_focus_years$Species.Code) 


all_shrimp_foucs_years_2 <- all_shrimp_focus_years %>% rename(district = District.Number)#, sub_district = Stat.Area) #renamed district to fit in the function
all_shrimp_foucs_years_2$sub_district <- substr(all_shrimp_foucs_years_2$Stat.Area, 4,5)

all_shrimp_w_analysis_area <- add.analysis.area(all_shrimp_foucs_years_2) #add analysis area
all_shrimp_w_analysis_area <- add.mgmt.unit(all_shrimp_w_analysis_area) #add mgmt area


##################################################################################
#wrangle with functions

#what do all of the districts correspond to??
#102 112 115 101 103 113 111 107 106 110 108 116 183 104 105 109 114 181- dont use 181 for analysis, barely any fishing

#filter using mgmt unit

mgmt_u_District_7 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 7")
#View(mgmt_u_District_7)
#n_distinct((mgmt_u_District_7 %>% filter(Season.Ref == "21-22") %>% select(Vessel.Name))) #quick QC of vessel name count by district. Looks good, 
##but I should check the spot and coons combined areas
#str(mgmt_u_District_7) # 4639 by 16. 
#str(filter(mgmt_u_District_7, Analysis.Area=="Upper Ernest Sound")) #ok that looks about right


#clean by management unit
unique(all_shrimp_w_analysis_area$Management_unit)
mgmt_u_District_1 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 1")
mgmt_u_District_2 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 2")
mgmt_u_Section_3A <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Section 3A")
mgmt_u_Section_3B <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Section 3B")
mgmt_u_Tenakee_Inlet <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Tenakee Inlet")
mgmt_u_R_District_12 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Remainder District 12")
mgmt_u_R_District_11 <- wrangle.spotcoon.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Remainder District 11") #both spots AND coons #guess I could have species code choice written into the function...instead of making 3 different functions
mgmt_u_District_7 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 7")
mgmt_u_North_Clarence <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "North Clarence")
mgmt_u_N_Fred_Sound <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Northern Frederick Sound")
mgmt_u_Seymour <- wrangle.spotcoon.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Seymour") #both spot and coons
mgmt_u_S_Fred_Sound <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Southern Frederick Sound")
mgmt_u_N_Sumner_Strait <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Sumner Strait")
mgmt_u_District_4 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 4")
mgmt_u_District_5 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 5")
mgmt_u_District_9 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 9")
#dist_15_coon_shrimp <- wrangle.coonstripe.shrimp.by.district(all_shrimp_w_analysis_area, 115) 
mgmt_u_Section_13AB <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Section 13-A/B") #something is nor working with the page 2 values
mgmt_u_Section_13C <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Section 13-C")
mgmt_u_District_14 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 14")
mgmt_u_District_16 <- wrangle.spotcoon.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 16") 
##D16 and D11 both spots AND coons. D15 just coons
#the coons
mgmt_u_District_15E <- wrangle.coon.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 15 East")
#D15 remainder was... spot or coons?
mgmt_u_R_District_15 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Remainder District 15")



#combine to make master cleaned data with cpue
wrangled_shrimp <- rbind(mgmt_u_District_1, mgmt_u_District_2, mgmt_u_Section_3A, mgmt_u_Section_3B, mgmt_u_Tenakee_Inlet, mgmt_u_R_District_12,
      mgmt_u_R_District_11, mgmt_u_District_7, mgmt_u_North_Clarence, mgmt_u_N_Fred_Sound, mgmt_u_Seymour, mgmt_u_S_Fred_Sound,
      mgmt_u_N_Sumner_Strait, mgmt_u_District_4, mgmt_u_District_5, mgmt_u_District_9, #)#, dist_15_coon_shrimp) #16 total, 17 with the coons
      mgmt_u_Section_13AB, mgmt_u_Section_13C, mgmt_u_District_14, mgmt_u_District_16,#adding the ones I missed from page 2.  
mgmt_u_District_15E, mgmt_u_R_District_15) #the coons

#write a csv for the by-district RMD's
write.csv(x= wrangled_shrimp, file="Data/wrangled shrimp focus years.csv")

