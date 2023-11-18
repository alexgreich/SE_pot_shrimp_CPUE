##exploring the data##
#Alex Reich
#initiated: 11/14/23
#last worked on: 11/17/23

#github repository is functional for this project as of 11/16/23
#COMMENT: 11/17/23: I am (still) concerned that there are zeros where there should not be zeros for the Pots column
##potentially caused by filtering the stat area BEFORE the CPUE data-wrangling.
##There's a chance that a fisherman reported catch from multiple stat areas on one fish ticket.
###seems to be a small number of instances of this, if any
##BUT, I can check by importing the ENTIRE DATASET (maybe filter from season 01-02 onwards), wrangling the CPUE and pot related stuff, and THEN filtering for stat/analysis area
###I will have to have Oracle Client up and running and functional first, and not deal with OceanAK's bullshit csv line limit.
######-> Oracle client is in progress. Ticket is with OIT. Is is downloaded but not appearing where it needs to be in R

#also - 11/17/23 - read other GAM-CPUE studies and make sure I'm setting up a model correctly. But first - > DATA EXPLORATION!


#load libraries
library(tidyverse) #includes dplyr
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(ggcorrplot)
library(corrplot)


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
#focus_years_ernest <- focus_years_ernest %>% filter(Species.Code == 965) NOT YET OR ELSE POTS MIGHT SAY 0!!


###select just the columns that I am interested in for this analysis
focus_years_ernest <- focus_years_ernest %>% select(Batch.Year, Pre.Print.Ticket, Fish.Ticket.Number, ADFG.Number, Vessel.Name, Event.Date, DOL.Month, Stat.Week, Date.of.Landing, Season, Season.Ref,
                                                    Item.Number, 
                                                    Stat.Area, Whole.Weight..sum., Pots, Species.Code, Batch.Year) #what is item number? the item per fish ticket

###wrangle so that I have a new column called Number_of_vessels, which will represent the number of vessels in a current season
focus_years_ernest <- focus_years_ernest %>% 
  group_by(Season.Ref) %>% #grouping by season (year)
  mutate(vessel_count = n_distinct(ADFG.Number)) %>% #count the unique # of vessels (by ADFG number)
  ungroup() #ungroup


###wrangle: sum the weight(sum) per each fish ticket, while keeping the MAX of pots for that fish ticket. This will insure that pots are not counted twice/multiple times
#there are some NA's but I dont want to deal with them

sum_shrimp_ernest <- focus_years_ernest %>%
  mutate(Pots = replace_na(Pots, 0)) %>%
  group_by(Fish.Ticket.Number, Species.Code) %>% #also group by stat area for analysis areas with multiple stat areas.
  summarise(
    total_weight = sum(Whole.Weight..sum.),
    max_pots = max(Pots),
    ADFG.Number = max(ADFG.Number),
    Season.Ref=max(Season.Ref),
    Vessel.Name = max(Vessel.Name),
    DOL.Month= max(DOL.Month),
    Stat.Week=max(Stat.Week), 
    Batch.Year=max(Batch.Year)
    ) %>%
  ungroup() #this has pots and total weight for all shrimp species

max_pots_total <- sum_shrimp_ernest %>% #get # pots fpr each fish ticket
  group_by(Fish.Ticket.Number) %>%
  summarise(
    max_pots_2 = max(max_pots)
  )%>%
  ungroup()

sum_spot_shrimp_ernest <- sum_shrimp_ernest %>%
  filter(Species.Code == 965) %>%
  right_join(max_pots_total) %>%
  select(-max_pots)

View(sum_spot_shrimp_ernest)
str(sum_shrimp_ernest)
str(focus_years_ernest)


#calculate nominal CPUE
sum_spot_shrimp_ernest <- sum_spot_shrimp_ernest %>%
  filter(max_pots_2 != 0) %>%
  mutate(CPUE_nom = total_weight/max_pots_2)

##QC: is it possible that there are one pot # entry for multiple stat areas? If so, more data wrangling required (to make sure there is not a 0 where there should not be a 0)
 
###############################
#Exploratory data analysis
#############################


##correlation plot

#try ggcorplot
#which 
names(sum_spot_shrimp_ernest)

corr_spot <- sum_spot_shrimp_ernest %>%
  select(ADFG.Number, Season.Ref, DOL.Month, Stat.Week, CPUE_nom, Batch.Year) %>%
  mutate(ADFG.Number = factor(ADFG.Number), Season.Ref = as.factor(Season.Ref)) #dol month and stat week should be temporal.

#corr <- round(cor(corr_spot), 1)
#NOPE

#try corrplot
#M <- cor()

#try internet code
#model.matrix(~0+., data=corr_spot) %>% 
 # cor(use="pairwise.complete.obs") %>% 
  #ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

corr_spot2 <- sum_spot_shrimp_ernest %>%
  select(DOL.Month, Stat.Week, CPUE_nom, Batch.Year)

#model.matrix(~0+., data=corr_spot2) %>% 
 # cor(use="pairwise.complete.obs") %>% 
#  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

#ok, when we revisit this next, do exploratory data analysis comprehensively on this sort of data. Think: correlation plots. Should I make stat weeks integers? Probably not...


#11/17/23 messing with correlation matrix

#accomplished what I was trying to do below
#pairs(sum_spot_shrimp_ernest) #non-numeric arg makes it fail
pairs(corr_spot) #this one is more useful
pairs(corr_spot2)

#so, these graphs imply:
### Vessel (ADFG.Number) influences CPUE
### Year (Batch.Year or Season.Ref, equivalently) have a relationship with CPUE
##I dont really see a trend by month (DOL.Month), but we'll invlude it in the model
##Idk about week. Looks more variable some weeks than others, likely beause there is higher effort in some weeks than others
###CONCLUSION: VESSEL (ADFG.Number) and YEAR (Season.Ref) have ties to nominal cpue


ggplot(na.omit(corr_spot)) + aes(x=ADFG.Number, y=(CPUE_nom)) +  geom_boxplot()
ggplot(na.omit(corr_spot)) + aes(x=Season.Ref, y=(CPUE_nom)) + geom_point() #maybe geom_smooth does not work with caregorical variables
ggplot(na.omit(corr_spot)) + aes(x=Batch.Year, y=(CPUE_nom)) + geom_smooth(method="lm") + geom_point()
#ggplot(na.omit(corr_spot)) + aes(x=Season.Ref, y=(CPUE_nom)) + geom_smooth(method="gam") + geom_point()
#linearly, the CPUE increases with year. But it's more cyclical than linear
ggplot(na.omit(corr_spot)) + aes(x=factor(Batch.Year), y=(CPUE_nom)) + geom_boxplot() 
ggplot(na.omit(corr_spot)) + aes(x=factor(Batch.Year), y=(CPUE_nom)) + geom_violin() 

#look closer at the other variables in the pairs() plot:
ggplot(na.omit(corr_spot)) + aes(x=factor(DOL.Month), y=(CPUE_nom)) + geom_boxplot()
ggplot(na.omit(corr_spot)) + aes(x=DOL.Month, y=(CPUE_nom)) + geom_point()

ggplot(na.omit(corr_spot)) + aes(x=factor(Stat.Week), y=(CPUE_nom)) + geom_boxplot()


#what's next: either more exploratory plots or analysis

#density of cpie data
ggplot(corr_spot) + aes(x=CPUE_nom) +geom_density()
##righted-tailed but passable as normal?




###########333
#ANALYSIS
############
#phil's null model:
#m0 <- bam(log_ppm ~ Year, data=fulldat, gamma=1.4)
##Year is in the null model. Interesting.
##referencing around line 523 of Jig_BFG_CPUE.R code
###WHAT IS log_ppm?? -> it's the log of the set pounds per minute

#after the null model, phil looks at the null model plus smoothers - 
##TESTED ALL OF THESE ONE STEP MODELS AGAINST EACH OTHER
##like this:
#m0.jday <- bam(log_ppm ~ Year + s(Jdate, k=4), data=fulldat, gamma=1.4)
#m0.drifts <- bam(log_ppm ~ Year + s(Drifts, k=4), data=fulldat, gamma=1.4)
#m0.depth <- bam(log_ppm ~ Year + s(Depth, k=4), data=fulldat, gamma=1.4)
#m0.boat <- bam(log_ppm ~ Year + s(ADFG, bs='re', by=dum), data=fulldat, gamma=1.4) #this variable is not continuous, right? He does something so it works...


#AND THEN HE LOOKED AT THE GLOBAL MODEL
##which for phil, was:
#global<-bam(log_ppm ~ Year + Gear + s(Hooks, k=4) + 
 #             s(Line_no, k=3) + s(Jdate, k=4) +
  #            s(Drifts, k=4) + s(Depth, k=4) + te(Lon, Lat) +
   #           s(Stat, bs='re', by=dumstat) + s(ADFG, bs='re', by=dum),
            #+ s(Trip, bs='re', by=dum),
    #        data=fulldat, gamma=1.4)


#then he looked at residuals for the global models
##line 569


##Question: so we're using all of these models with CPUE as the Y var. How do we get standardized CPUE from that? Is it just predicted CPUE

#Phil chose a model for standardization: line 794

##phil says:
# Create the STANDARDIZED CPUE INDEX by predicting CPUE with the model of choice
# based on the variable average values

#ok, I thought so
#lines 820 ish, not sure why he selected those particular items.
##he made an average dataset? that line 820 block. Why?

#stopped on section 6 of phil code


##WIP below##4


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
