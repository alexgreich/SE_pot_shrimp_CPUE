##
#TO DO
#so it turns out that managment unit and district are not the same thing. And I'll want to make a function to filter for MGMT UNIT, not district.
 

#also see max email for:  WEIGHTS. ADD WEIGHTS FOE EARCH ANALYSIS AREA
##I am unsure about weighing the areas tho. Doesnt CPUE... weigh itself??

#####make function after analysis area called add.mgmt.area
##
#functions for the pot shrimp CPUE standardization

library(tidyverse)
library(lubridate)

#a function for adding the analysis area name to the df
##thanks Max for writing!!
add.analysis.area <- function(df) {
  df <- df %>%
    mutate(Analysis.Area = case_when(
      district == 101 & sub_district %in% c(85, 90, 95) ~ "West Behm Canal",
      district == 101 & sub_district %in% c(51, 53, 55, 60, 71, 73) ~ "East Behm Canal",
      district == 101 & sub_district %in% c(75, 77, 80) ~ "Back Behm Canal",
      district == 101 & sub_district == 30 ~ "Boca de Quadra",
      district == 101 & sub_district %in% c(10, 11, 13, 15) ~ "Portland Canal",
      district == 101 & sub_district %in% c(27, 40, 43, 44, 45, 46, 48) ~ "Inner Ketchikan Inlets",
      district == 101 & sub_district %in% c(21, 22, 23, 25, 29, 41) ~ "Revilla Channel/Gravina",
      district == 102 & sub_district %in% c(70, 80) ~ "Middle Clarence Strait",
      district == 102 & sub_district == 40 ~ "Cholmondeley Sound",
      district == 102 & sub_district == 50 ~ "Cholmondeley Sound",
      district == 102 & sub_district == 60 ~ "Kasaan Bay",
      district == 102 & sub_district %in% c(10, 15, 20) ~ "Lower Clarence Strait",
      district == 102 & sub_district == 30 ~ "Moria Sound",
      district == 103 & sub_district %in% c(11, 15) ~ "Lower Cordova Bay",
      district == 103 & sub_district %in% c(21, 23) ~ "Mid Cordova Bay",
      district == 103 & sub_district == 25 ~ "Hetta Inlet",
      district == 103 & sub_district %in% c(30, 40) ~ "Upper Cordova Bay",
      district == 103 & sub_district %in% c(50, 60, 70, 80) ~ "Craig",
      district == 103 & sub_district %in% c(90) ~ "Sea Otter Sound",
      district == 104  ~ "district 4",
      district == 105 & sub_district %in% c(10,30) ~ "Affleck/Port Beauclerc",
      district == 105 & sub_district %in% c(31,32) ~ "Rocky Pass",
      district == 105 & sub_district %in% c(41,42,43,50) ~ "Cape Pole to Point Baker",
      district == 106 & sub_district %in% c(10,30) ~ "Upper Clarence Strait",
      district == 106 & sub_district %in% c(20,22,25) ~ "SW Etolin",
      district == 106 & sub_district %in% c(41,42,43,44) ~ "Western Sumner",
      district == 107 & sub_district %in% c(10) ~ "Lower Ernest Sound",
      district == 107 & sub_district %in% c(20) ~ "Upper Ernest Sound",
      district == 107 & sub_district %in% c(30,35) ~ "Zimovia Strait",
      district == 107 & sub_district %in% c(40,45) ~ "Bradfield Canal",
      district == 108 & sub_district %in% c(10,20) ~ "Stikine Strait/Chichagof Pass",
      district == 108 & sub_district %in% c(30,40) ~ "Eastern Sumner",
      district == 108 & sub_district %in% c(41,50,60) ~ "Frederick Sound",
      district == 109 & sub_district %in% c(10,11,13,20) ~ "SE Baranof",
      district == 109 & sub_district %in% c(30) ~ "Eliza Harbor",
      district == 109 & sub_district %in% c(40,41,42,43) ~ "Keku Straits/Port Camden",
      district == 109 & sub_district %in% c(44,45,51,52,61,62,63) ~ "Western Kuiu",
      district == 110 & sub_district %in% c(31,32,33) ~ "Hobart/Windham Bays",
      district == 110 & sub_district %in% c(21,22,23,24) ~ "SE Admiralty",
      district == 110 & sub_district %in% c(13,14,15,16,12) ~ "Farragut Bay",
      district == 110 & sub_district %in% c(34) ~ "Port Houghton",
      district == 111 & sub_district %in% c(11,12,13,14) ~ "Seymour Canal",
      district == 111 & sub_district %in% c(20,21,33,34,35) ~ "Glacier-fed Bays",
      district == 111 & sub_district %in% c(50,55) ~ "11-A",
      district == 112 & sub_district %in% c(41,42) ~ "East Tenakee",
      district == 112 & sub_district %in% c(43,44,45,46,47,48) ~ "West Tenakee",
      district == 112 & sub_district %in% c(11,21,22) ~ "Kelp Bay",
      district == 112 & sub_district %in% c(50) ~ "Freshwater Bay",
      district == 112 & sub_district %in% c(61) ~ "Point Couverden",
      district == 113 & sub_district %in% c(51,52,54,59) ~ "Peril Strait",
      district == 113 & sub_district %in% c(55,56,57,58) ~ "Hoonah Sound",
      district == 113 & sub_district %in% c(21,22) ~ "Whale Bay",
      district == 113 & sub_district %in% c(11,12) ~ "Larch/Branch Bays",
      district == 113 & sub_district %in% c(31,32,33) ~ "Crawfish Inlets",
      district == 113 & sub_district %in% c(34) ~ "Necker Bay",
      district == 114 & sub_district %in% c(25,80,27) ~ "Eastern Icy Strait",
      district == 114 & sub_district %in% c(31,32,33,34) ~ "Port Frederick",
      district == 115 & sub_district %in% c(32) ~ "Chilkat Inlet",
      district == 115 & sub_district %in% c(33) ~ "Lutak Inlet",
      district == 115 & sub_district %in% c(34) ~ "Chilkoot Inlet",
      district == 115 & sub_district %in% c(35) ~ "Taiya Inlet",
      district == 115 & sub_district %in% c(11,10,20) ~ "15-C",
      district == 116 & sub_district %in% c(13) ~ "Lituya Bay",
      district == 116 & sub_district %in% c(11,12,14) ~ "Rest of 116",
    ))   
}



#add managment unit function aka fisheries
add.mgmt.unit <- function(df){ #NEEDS trial run
  df2 <- df %>% mutate(Management_unit = case_when(
    district == 101 ~ "District 1", #only works if we want everthting in district 1 #maybe check if district one has all the necessary subdistricts/areas (and no extras)
    district == 102 ~ "District 2",
    district == 103 & sub_district %in% c(25, 11, 15, 21, 23, 30, 40) ~ "Section 3A",
    district == 103 & sub_district %in% c(50, 60, 70, 80, 90) ~ "Section 3B",
    district == 104 ~ "District 4",
    district == 105 ~ "District 5",
    district == 106 & sub_district %in% c(20,22,25,10,30) ~ "North Clarence",
    district == 107 ~ "District 7",
    #district #hmm, a multi-district one. OR function (VERTICAL LINE), I guess.
    district == 106 & sub_district %in% c(41, 42, 43, 44)|district == 108 & sub_district %in% c(30,40,10,20) ~ "Sumner Strait", #make sure I didnt need paratheses around the & codes
    district == 109 ~ "District 9",
    district == 108 & sub_district %in% c(41,50,60)| district == 110 & sub_district %in% c(11:17) ~ "Southern Frederick Sound",
    district == 110 & sub_district %in% c(31,32,33,34,21:24) ~ "Northern Frederick Sound",
    district == 111 & sub_district %in% c(11,12,13,14) ~ "Seymour",
    district == 111 & sub_district %in% c(50,55,20,21,33,34,35) ~ "Remainder District 11",
    district == 112 & sub_district %in% c(41,42,43,44,45,46,47,48) ~ "Tenakee Inlet",
    district == 112 & sub_district %in% c(50,11,21,22,61) ~ "Remainder District 12",
    district == 113 & sub_district %in% c(31,32,33,11,12,13,34,22,21) ~ "Section 13-A/B", #additional page 2 districts added
    district == 113 & sub_district %in% c(55,56,57,58,51,52,53,54,59) ~ "Section 13-C",
    district == 114 ~ "District 14",
    district == 115 & sub_district %in% c(34,33,35) ~ "District 15 East",
    district == 115 & sub_district %in% c(32) ~ "Remainder District 15",
    district == 116 ~ "District 16"
    
  ))
  
  return(df2)
}

#prep for std cpue analysis. 
##will give you wrangled data (df) and nominal CPUE by shrimp district
###can (manually) filter further to get analysis areas
###kind of needs a smooth throuhg (of hashtags) and a QC
####CHANGE TO BY MGMT UNIT!! (WILL NOT BE HARD)
wrangle.spot.shrimp.by.district <- function(dat, distr){  
  
  #test
  #dat <- all_shrimp_w_analysis_area
  #distr <- 107
  
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
      max_pots = max(Pot.Lifts), #max pots for each species, fish ticket, and analysis area #MIGHT BE AN ERROR HERE. WHAT IF THE LINE IS LISTED FOR A DIFF SPECIS?
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
  max_pots_total <- df_3 %>% #get # pots fpr each fish ticket, Ccombines species
    group_by(Fish.Ticket.Number) %>%
    summarise( #if I needed to account for people correctly partitioning shrimp effort (ex 4 pots to spot and 2 pots to coon, I can add an if statement for if the pot lifts in a fish ticket are unequal and do not contain 0. And... do soemthing with that if statment)
      max_pots_2 = max(max_pots) #takes the max of pots. This will acoount for: sitautions where pots are replicated (ex: 4,4,4), and situations where pots are one entry, (ex: 4 0 0) Does not currently account for when people actually partitioned their effort to spot vs. coon.
      ##whatif pots were already proportioned appropriately tho? Does it account for that?
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

#same but for coons
##I forget which district/analysis area uses coons instead of spot shrimp. District 15(?)
wrangle.coonstripe.shrimp.by.district <- function(dat, distr){  
  
  #test
 # dat <- all_shrimp_w_analysis_area
  #distr <- 107
  
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
      vessel_count=max(vessel_count), #I'm just saying I want this in the resulting 
      Management_unit=max(Management_unit) #added in anticipation of combining this with full dataset
    ) %>%
    ungroup() 
  
  #calculating max pots, as across species they only lsit one pot sometimes
  max_pots_total <- df_3 %>% #get # pots fpr each fish ticket
    group_by(Fish.Ticket.Number) %>%
    summarise( #if I needed to account for people correctly partitioning shrimp effort (ex 4 pots to spot and 2 pots to coon, I can add an if statement for if the pot lifts in a fish ticket are unequal and do not contain 0. And... do soemthing with that if statment)
      max_pots_2 = max(max_pots) #the way I wrangled, this will account for 
    )%>%
    ungroup()
  
  df_4 <- df_3 %>% #this is where NA's appear for the season.ref code. need to figure out why.
    #filter(Species.Code == 965) %>%
    right_join(max_pots_total) %>% #probs has to do with the join, the NA's that is
    filter(Species.Code == 964) %>% #coons is 964??
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



#################################################################
#wrangle by mgmt unit
##CAUTION: this omits some of the analysis areas.
###Do we care about Lookta sound?

#ok well, let's continue this for page 2 on the table
wrangle.spot.shrimp.by.mgmt.unit <- function(dat, m_unit){  #m_unit needs to be in quotes
  
  #test
  #dat <- all_shrimp_w_analysis_area
  #distr <- 107
  
  #filter for the district of interest
  df_1 <- dat %>% filter(Management_unit == m_unit)
  
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
      max_pots = max(Pot.Lifts), #max pots for each species, fish ticket, and analysis area #MIGHT BE AN ERROR HERE. WHAT IF THE LINE IS LISTED FOR A DIFF SPECIS?
      ADFG.Number = max(ADFG.Number), #I'm just saying I want this in the resulting df
      Season.Ref=max(Season.Ref), #I'm just saying I want this in the resulting df
      Vessel.Name = max(Vessel.Name), #I'm just saying I want this in the resulting df
      DOL.Month= max(DFB.Month), #I'm just saying I want this in the resulting df
      Stat.Week=max(Stat.Week), #I'm just saying I want this in the resulting df
      Batch.Year=max(Batch.Year), #I'm just saying I want this in the resulting df
      Event.Date=max(Date.of.Landing), #I'm just saying I want this in the resulting df
      vessel_count=max(vessel_count), #I'm just saying I want this in the resulting df
      Management_unit=max(Management_unit) #added 4/15/24. seems to have worked
    ) %>%
    ungroup() 
  
  #calculating max pots, as across species they only lsit one pot sometimes
  max_pots_total <- df_3 %>% #get # pots fpr each fish ticket, Ccombines species
    group_by(Fish.Ticket.Number) %>%
    summarise( #if I needed to account for people correctly partitioning shrimp effort (ex 4 pots to spot and 2 pots to coon, I can add an if statement for if the pot lifts in a fish ticket are unequal and do not contain 0. And... do soemthing with that if statment)
      max_pots_2 = max(max_pots) #takes the max of pots. This will acoount for: sitautions where pots are replicated (ex: 4,4,4), and situations where pots are one entry, (ex: 4 0 0) Does not currently account for when people actually partitioned their effort to spot vs. coon.
      ##whatif pots were already proportioned appropriately tho? Does it account for that?
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

#need to make a wrangle coons by mgmt unit function
wrangle.coon.shrimp.by.mgmt.unit <- function(dat, m_unit){  #m_unit needs to be in quotes
  
  #test
  #dat <- all_shrimp_w_analysis_area
  #distr <- 107
  
  #filter for the district of interest
  df_1 <- dat %>% filter(Management_unit == m_unit)
  
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
      max_pots = max(Pot.Lifts), #max pots for each species, fish ticket, and analysis area #MIGHT BE AN ERROR HERE. WHAT IF THE LINE IS LISTED FOR A DIFF SPECIS?
      ADFG.Number = max(ADFG.Number), #I'm just saying I want this in the resulting df
      Season.Ref=max(Season.Ref), #I'm just saying I want this in the resulting df
      Vessel.Name = max(Vessel.Name), #I'm just saying I want this in the resulting df
      DOL.Month= max(DFB.Month), #I'm just saying I want this in the resulting df
      Stat.Week=max(Stat.Week), #I'm just saying I want this in the resulting df
      Batch.Year=max(Batch.Year), #I'm just saying I want this in the resulting df
      Event.Date=max(Date.of.Landing), #I'm just saying I want this in the resulting df
      vessel_count=max(vessel_count), #I'm just saying I want this in the resulting df
      Management_unit=max(Management_unit) #added 4/15/24. seems to have worked
    ) %>%
    ungroup() 
  
  #calculating max pots, as across species they only lsit one pot sometimes
  max_pots_total <- df_3 %>% #get # pots fpr each fish ticket, Ccombines species
    group_by(Fish.Ticket.Number) %>%
    summarise( #if I needed to account for people correctly partitioning shrimp effort (ex 4 pots to spot and 2 pots to coon, I can add an if statement for if the pot lifts in a fish ticket are unequal and do not contain 0. And... do soemthing with that if statment)
      max_pots_2 = max(max_pots) #takes the max of pots. This will acoount for: sitautions where pots are replicated (ex: 4,4,4), and situations where pots are one entry, (ex: 4 0 0) Does not currently account for when people actually partitioned their effort to spot vs. coon.
      ##whatif pots were already proportioned appropriately tho? Does it account for that?
    )%>%
    ungroup()
  
  df_4 <- df_3 %>% #this is where NA's appear for the season.ref code. need to figure out why.
    #filter(Species.Code == 965) %>%
    right_join(max_pots_total) %>% #probs has to do with the join, the NA's that is
    filter(Species.Code == 964) %>% #COONS!
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

#wrangle where we care about combined spot and coons, as in D11 and D16
#need to make a wrangle coons by mgmt unit function
wrangle.spotcoon.shrimp.by.mgmt.unit <- function(dat, m_unit){  #m_unit needs to be in quotes
  
  #test
  #dat <- all_shrimp_w_analysis_area
  #distr <- 107
  
  #filter for the district of interest
  df_1 <- dat %>% filter(Management_unit == m_unit)
  
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
      max_pots = max(Pot.Lifts), #max pots for each species, fish ticket, and analysis area #MIGHT BE AN ERROR HERE. WHAT IF THE LINE IS LISTED FOR A DIFF SPECIS?
      ADFG.Number = max(ADFG.Number), #I'm just saying I want this in the resulting df
      Season.Ref=max(Season.Ref), #I'm just saying I want this in the resulting df
      Vessel.Name = max(Vessel.Name), #I'm just saying I want this in the resulting df
      DOL.Month= max(DFB.Month), #I'm just saying I want this in the resulting df
      Stat.Week=max(Stat.Week), #I'm just saying I want this in the resulting df
      Batch.Year=max(Batch.Year), #I'm just saying I want this in the resulting df
      Event.Date=max(Date.of.Landing), #I'm just saying I want this in the resulting df
      vessel_count=max(vessel_count), #I'm just saying I want this in the resulting df
      Management_unit=max(Management_unit) #added 4/15/24. seems to have worked
    ) %>%
    ungroup() 
  
  #calculating max pots, as across species they only lsit one pot sometimes
  max_pots_total <- df_3 %>% #get # pots fpr each fish ticket, Ccombines species
    group_by(Fish.Ticket.Number) %>%
    summarise( #if I needed to account for people correctly partitioning shrimp effort (ex 4 pots to spot and 2 pots to coon, I can add an if statement for if the pot lifts in a fish ticket are unequal and do not contain 0. And... do soemthing with that if statment)
      max_pots_2 = max(max_pots) #takes the max of pots. This will acoount for: sitautions where pots are replicated (ex: 4,4,4), and situations where pots are one entry, (ex: 4 0 0) Does not currently account for when people actually partitioned their effort to spot vs. coon.
      ##whatif pots were already proportioned appropriately tho? Does it account for that?
    )%>%
    ungroup()
  
  df_4 <- df_3 %>% #this is where NA's appear for the season.ref code. need to figure out why.
    #filter(Species.Code == 965) %>%
    right_join(max_pots_total) %>% #probs has to do with the join, the NA's that is
    filter(Species.Code == 964|Species.Code == 965) %>% #spot and coons
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
  
  return(df_6)
  
}
