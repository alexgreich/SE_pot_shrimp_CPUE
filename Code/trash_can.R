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


####4/23/24
#vessel count over each mgmt unit by uear. SOMETHING IS WRONG>
wrangled_shrimp_2 <- wrangled_shrimp %>%
  group_by(Season.Ref, Management_unit) %>% #group by this ok?
  mutate(vessel_count_by_mgmt_unit = sum(vessel_count)) %>%
  ungroup() 
##let's check if that worked
wrangled_shrimp_2 %>% filter(Management_unit == "District 1") %>% #there we go, looks much better. Need to QC that wrangle tho. There were not 6000 fishing bessels in a year right? something is still uip
  ggplot(aes(x=factor(Season.Ref), y= vessel_count_by_mgmt_unit)) + geom_point()


# a graph I dont think I need now taht I wrangled better.
mgmt_u_District_1 %>% filter(Analysis.Area=="Portland Canal") %>%
  ggplot(aes(x=factor(Season.Ref), y= vessel_count_aa)) + geom_point() #ok. need to add up vessel count by analysis area and year, to get the total vessel count by district in a year


#################################3
#from the D7 analysis
test_df <- D7
test_df$Analysis.Area <- factor(test_df$Analysis.Area)
m_glob_r <- gam(log(CPUE_nom + 0.001) ~ factor(Season.Ref) + factor(ADFG.Number)+ s(vessel_count_mgmt_u, k=4) + s(jdate, k=4) + s(Analysis.Area, bs="re", k=4), data=test_df) #ok that worked. I jsut cant make it a factor within my equation. Maybe I should adjust my df code accordingly. #also how to add ar1 auto correlation??
summary(m_glob_r)
