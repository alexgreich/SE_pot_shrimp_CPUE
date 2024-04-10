###
##Focus Wrangle
##I'll explore different data wrangling options, see if they are the same.

library(tidyverse)
library(lubridate)

#source("analysisareas_function.R") #mind the location of this function



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
unique(all_shrimp_w_analysis_area$Analysis.Area)
#some still have NA's, idk why.

#NOW filter by district
#start with 107 (upper ernest sound is 107-20)

#DISTRICT 107
##(filter filter)
##follow the filter pathway, mind the instructions in the word doc please.

Upp_ern <- all_shrimp_w_analysis_area %>% filter(Analysis.Area == "Upper Ernest Sound") # filter(district ==107) #upper ernest sound

#vessel count
Upp_ern_2 <- Upp_ern %>% 
  group_by(Season.Ref) %>% #grouping by season (year)
  mutate(vessel_count = n_distinct(ADFG.Number)) %>% #count the unique # of vessels (by ADFG number)
  ungroup() #ungroup

Upper_ernest_sound_d <- Upp_ern_2 %>%
  mutate(Pot.Lifts = replace_na(Pot.Lifts, 0)) %>% ##below: upper ernest sound.. only has one stat areas
  group_by(Fish.Ticket.Number, Species.Code) %>% #, Stat.Area) %>% #also group by stat area for analysis areas with multiple stat areas. OOH. SHOULD I GROUP BY ANALYSIS AREA ALSO?? I THHINK MAYBE SO!
  ##adding Stat.Area matters and I do NOT know why for upper ernest
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
  ungroup() #the upper ernest sound, filtered by district way

#View(Upper_ernest_sound_d)


max_pots_total <- Upper_ernest_sound_d %>% #get # pots fpr each fish ticket
  group_by(Fish.Ticket.Number) %>%
  summarise(
    max_pots_2 = max(max_pots)
  )%>%
  ungroup()

Upper_ernest_sound_d_2 <- Upper_ernest_sound_d %>% 
  right_join(max_pots_total) %>% 
  filter(Species.Code == 965) %>%
  select(-max_pots)

#View(Upper_ernest_sound_d_2)

#get cpue
Upper_ernest_sound_d_3 <- Upper_ernest_sound_d_2 %>%
  filter(max_pots_2 != 0) %>%
  mutate(CPUE_nom = total_weight/max_pots_2)

#fix event date to not be weird
Upper_ernest_sound_d_3$Event.Date <- as.Date(Upper_ernest_sound_d_3$Event.Date)

#wrangle jdate
Upper_ernest_sound_d_4 <-  Upper_ernest_sound_d_3 %>%
  mutate(landing_date = parse_date_time(Event.Date,c("%Y/%m/%d"))) %>%
  mutate(jdate = as.numeric(format(landing_date,"%j"))) #nailed it!!!

#compare this way to the other way.
str(Upper_ernest_sound_d_4) #has 3 more rows and I dont know why. Oh duh. becasuse it contains more fishing years...
str(sum_spot_shrimp_ernest) #Fixed it. set analysis area to upper ernest sound
#str(na.omit(Upper_ernest_sound_d_4))

###########################################################################
#make this a Function

#before adding data to this function, the data (dat) needs to be combined and wrangled to add district and Analysis.Area columns
##Can make that its own function, I guess, but only needs to be done once.

#dat has these columns and is from years 01-02 to current (I think)

wrangle.spot.shrimp.by.district <- function(dat, distr){  
  
  #test
  dat <- all_shrimp_w_analysis_area
  distr <- 107
  
  #filter for the district of interest
  df_1 <- dat %>% filter(district == distr)
  
  
  
  
  #vessel count in the area of fishing during that year. SHOULD I DO THIS AFTER FILTERING FOR SPOT SHRIMP?? Perhaps.
  df_2 <- df_1 %>% 
    group_by(Season.Ref, Analysis.Area, Species.Code) %>% #grouping by season (year) AND.. fish ticket #AND... species??
    mutate(vessel_count = n_distinct(ADFG.Number)) %>% #count the unique # of vessels (by ADFG number) #should I do this AFTER filtering for spot shrimp??
    ungroup() #ungroup
  
  #unique(df_2$Species.Code) #what is 962?

  #ca
  df_3 <- df_2 %>%
    mutate(Pot.Lifts = replace_na(Pot.Lifts, 0)) %>% ##where there are NA's, put 0
    group_by(Fish.Ticket.Number, Species.Code, Analysis.Area) %>% #are these the right groupings? should I group by season(year) instead of fish ticket? No, because I want max pots per boat
    summarise(
      total_weight = sum(Whole.Weight..sum.), #total weight for each species, fish ticket, and analysis area (hopefully they ddidnt group coons and spot)
      max_pots = max(Pot.Lifts), #max pots for each species, fish ticket, and analysis area
      ADFG.Number = max(ADFG.Number), #I'm just saying I want this in the resulting df
      Season.Ref=max(Season.Ref), #I'm just saying I want this in the resulting df
      Vessel.Name = max(Vessel.Name), #I'm just saying I want this in the resulting df
      DOL.Month= max(DFB.Month), #I'm just saying I want this in the resulting df
      Stat.Week=max(Stat.Week), #I'm just saying I want this in the resulting df
      Batch.Year=max(Batch.Year), #I'm just saying I want this in the resulting df
      Event.Date=max(Date.of.Landing), #I'm just saying I want this in the resulting df
      vessel_count=max(vessel_count) #I'm just saying I want this in the resulting df
    ) %>%
    ungroup() 
  
  #calculating max pots, as across species they only lsit one pot sometimes
  max_pots_total <- df_3 %>% #get # pots fpr each fish ticket
    group_by(Fish.Ticket.Number) %>%
    summarise(
      max_pots_2 = max(max_pots)
    )%>%
    ungroup()
  
  df_4 <- df_3 %>% #this is where NA's appear for the season.ref code. need to figure out why.
    #filter(Species.Code == 965) %>%
    right_join(max_pots_total) %>% #probs has to do with the join, the NA's that is
    filter(Species.Code == 965) %>% #switched this over here, no more NAs'
    select(-max_pots)
  
  #calc nomimal cpue
  #calculate nominal CPUE
  df_5 <- df_4 %>%
    filter(max_pots_2 != 0) %>%
    mutate(CPUE_nom = total_weight/max_pots_2)
  
  #fix event date to not be weird
  df_5$Event.Date <- as.Date(df_5$Event.Date)
  
  #wrangle jdate
  df_6 <-  df_5 %>%
    mutate(landing_date = parse_date_time(Event.Date,c("%Y/%m/%d"))) %>%
    mutate(jdate = as.numeric(format(landing_date,"%j"))) #nailed it!!!
  
  #ok, that should do it, QC last 3 blocks please.
  
  return(df_6)

}



#next do this for every district
##as a for loop? as individuals?
##and make a functions R file for this and and analysis areas function. Keep your functions in one place!
#back to explotary_CPUE_all_shrimp_areas now, since the focus wrangle is complete?


