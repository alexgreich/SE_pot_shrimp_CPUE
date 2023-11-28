
##################################################
##Exploring the data 2: basic Earnest CPUE standardization
#####################################################3
#created 11/27/23 to build upon (aka simmer down) the original Exploring_the_data.R file
##once it finally got into my thick head that I probs should use a GLM instead of a GAM

#############################
#load libraries
###########################
library(tidyverse) #includes dplyr
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(ggcorrplot)
library(corrplot)
library(lubridate)
library(chron)

###################
#load data  -> IN SUBSEQUENT VERSIONS, THIS SHOULD SOURCE DIRECTLY FROM ORACLE
###there have been...frustrating complications and I'm working on it (medium priority)
###################
#intially, I'm looking at Fish Ticket and/or Logbook data just from Ernest Sound 
fish_tickets_ernest <- read.csv("Data/Ernest_sound_fish_tickets.csv")


names(fish_tickets_ernest)
#Pounds..sum and Whole.Weight..sum. should not be identical?
identical(fish_tickets_ernest$Pounds..sum., fish_tickets_ernest$Whole.Weight..sum.)
#yup, they're identical

#anyway...



##################################
#The data wrangle section
###############################

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
    Batch.Year=max(Batch.Year), #add Event date, date of landing, or vessel count here? #change from V1 ... vessel count is a continuous variable (integer... GAM time?)
    Event.Date=max(Event.Date),
    vessel_count=max(vessel_count) #that should do it.
  ) %>%
  ungroup() #this has pots and total weight for all shrimp species

max_pots_total <- sum_shrimp_ernest %>% #get # pots fpr each fish ticket
  group_by(Fish.Ticket.Number) %>%
  summarise(
    max_pots_2 = max(max_pots)
  )%>%
  ungroup()

sum_spot_shrimp_ernest <- sum_shrimp_ernest %>% #this is where NA's appear for the season.ref code. need to figure out why.
  #filter(Species.Code == 965) %>%
  right_join(max_pots_total) %>% #probs has to do with the join, the NA's that is
  filter(Species.Code == 965) %>% #switched this over here, no more NAs'
  select(-max_pots)

#View(sum_spot_shrimp_ernest)
#str(sum_shrimp_ernest)
#str(focus_years_ernest)


#calculate nominal CPUE
sum_spot_shrimp_ernest <- sum_spot_shrimp_ernest %>%
  filter(max_pots_2 != 0) %>%
  mutate(CPUE_nom = total_weight/max_pots_2)

##QC: is it possible that there are one pot # entry for multiple stat areas? If so, more data wrangling required (to make sure there is not a 0 where there should not be a 0)
#Q: do seasons need to be in order??


#WRANGLE.EVENT.DATE.TO.JULIAN.DATE.PLEASE!!!!!!!!!!!!!!!!!:
#jdate = as.POSIXlt(sum_spot_shrimp_ernest$Event.Date)$jul

#sum_spot_shrimp_ernest <-  sum_spot_shrimp_ernest %>%
 # mutate(jdate = ymd(Event.Date) - ymd("1970-01-01"))

sum_spot_shrimp_ernest <-  sum_spot_shrimp_ernest %>%
 mutate(jdate = julian(as.Date(Event.Date)))

###########################
#the exploratory plots section
#############################



names(sum_spot_shrimp_ernest)

corr_spot <- sum_spot_shrimp_ernest %>% #I want to see the correlations graphed
  select(ADFG.Number, Season.Ref, DOL.Month, Stat.Week, CPUE_nom, Batch.Year, Event.Date, vessel_count) %>%
  mutate(ADFG.Number = factor(ADFG.Number), Season.Ref = as.factor(Season.Ref)) 


corr_spot2 <- sum_spot_shrimp_ernest %>% #a closer look
  select(DOL.Month, Stat.Week, CPUE_nom, Batch.Year,vessel_count)

#correlation plots
#pairs(corr_spot) #argh it does not like non numeric arguments
pairs(corr_spot2)


#let's look closer at those graphs individually

### Vessel (ADFG.Number) influences CPUE
ggplot(na.omit(corr_spot)) + aes(x=ADFG.Number, y=(CPUE_nom)) +  geom_boxplot()
## a fixed factor, for now.

### Year (Batch.Year or Season.Ref, equivalently) have a relationship with CPUE
ggplot(na.omit(corr_spot)) + aes(x=Season.Ref, y=(CPUE_nom)) + geom_point() #maybe geom_smooth does not work with caregorical variables
ggplot(na.omit(corr_spot)) + aes(x=Batch.Year, y=(CPUE_nom)) + geom_smooth(method="lm") + geom_point()
ggplot(na.omit(corr_spot)) + aes(x=factor(Batch.Year), y=(CPUE_nom)) + geom_boxplot() #cyclical
###this will be a fixed factor, so we can extract the year effect later

##I dont really see a trend by month (DOL.Month), but we'll invlude it in the model
ggplot(na.omit(corr_spot)) + aes(x=factor(DOL.Month), y=(CPUE_nom)) + geom_boxplot()
ggplot(na.omit(corr_spot)) + aes(x=DOL.Month, y=(CPUE_nom)) + geom_point()
#I'll use a different temporal variable instead

##Idk about week. Looks more variable some weeks than others, likely beause there is higher effort in some weeks than others
ggplot(na.omit(corr_spot)) + aes(x=factor(Stat.Week), y=(CPUE_nom)) + geom_boxplot()
ggplot(na.omit(corr_spot)) + aes(x=Stat.Week, y=(CPUE_nom)) + geom_point()
#mayyybe smooth? do we smooth time?

#vessel number
ggplot(na.omit(corr_spot)) + aes(x=vessel_count, y=(CPUE_nom), group=vessel_count) + geom_boxplot()
ggplot(na.omit(corr_spot)) + aes(x=vessel_count, y=(CPUE_nom), group=vessel_count) + geom_violin()
#this is a continuous integrer. I should smooth this?

#JULIAN DATE GRAPH GOES HERE
##smooth or no? see phil work...

#conclusion: my model will be: log(CPUE_nom) ~ factor(Season (year) variable) + time variable (smoothed?? Julian date or week?) + VESSEL effect + vessel #
##I dont tbink I'll need to add a constand, since none of the cpue's should be 0.

###############################
#the analysis section
####################################
#set reference levels to the factors
names(corr_spot)
unique(corr_spot$Season.Ref) #there are some NA's. Why..?
class(corr_spot$Season.Ref) # a factor
median(corr_spot$Batch.Year) #2009

unique(corr_spot$vessel_count)
class(corr_spot$vessel_count) #maybe make this a gam

unique(corr_spot$ADFG.Number) #vessel ID
class(corr_spot$ADFG.Number)
summary(corr_spot$ADFG.Number)

class(corr_spot$Stat.Week)
summary(corr_spot$Stat.Week)
corr_spot$f.Stat.Week <- factor(corr_spot$Stat.Week)
summary(corr_spot$f.Stat.Week)


corr_spot$Season.Ref <- relevel(corr_spot$Season.Ref, ref="10-11") #reference level selection was arbitrary, honesly. Try changing and see how that impacts results
corr_spot$ADFG.Number <- relevel(corr_spot$ADFG.Number, ref="52131") # this vessel had the most fish tickets. try changing to one with average # of fish tickets?
corr_spot$f.Stat.Week <- relevel(corr_spot$f.Stat.Week, ref= "41") #41 is the median and the mode. made it a factor here.
##ask PHil for advice on how to select this.
##try 01-02 also, since that was the old baseline


#LM model with week
lm_basic_global <- lm(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + f.Stat.Week, data=na.omit(corr_spot)) #factors are year, vessel, and stat week. Maybe stat week should be a gam. Maybe vessel_count should be smoothed
summary(lm_basic_global)

#LM model with julian date


#GAM modwl with week


#GAM model with Julian date


##MODEL SELECTION!!






############################
#the describe-the-data plots section
###make that CPUE graph


#graph if we don't use gams


#graph if we do use gams