
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
library(mgcv)
library(mgcViz)

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


#wrangle jdate
sum_spot_shrimp_ernest <-  sum_spot_shrimp_ernest %>%
 mutate(landing_date = parse_date_time(Event.Date,c("%Y/%m/%d"))) %>%
  mutate(jdate = as.numeric(format(landing_date,"%j"))) #nailed it!!!

#great. does phil smooth week and/or jdate and/or vessel #? and are any of these treated as a factor???



###########################
#the exploratory plots section
#############################



names(sum_spot_shrimp_ernest)

corr_spot <- sum_spot_shrimp_ernest %>% #I want to see the correlations graphed
  select(ADFG.Number, Season.Ref, DOL.Month, Stat.Week, CPUE_nom, Batch.Year, Event.Date, vessel_count, jdate, total_weight, max_pots_2) %>%
  mutate(ADFG.Number = factor(ADFG.Number), Season.Ref = as.factor(Season.Ref)) 


corr_spot2 <- sum_spot_shrimp_ernest %>% #a closer look
  select(DOL.Month, Stat.Week, CPUE_nom, Batch.Year,vessel_count, jdate)

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
ggplot(na.omit(corr_spot)) + aes(x=jdate, y=log(CPUE_nom)) + geom_point() +geom_smooth(method="gam")
ggplot(na.omit(corr_spot)) + aes(x=jdate, y=(CPUE_nom), group=vessel_count) + geom_boxplot()

ggplot(corr_spot_limited) + aes(x=jdate, y=log(CPUE_nom)) + geom_point() +geom_smooth(method="gam") #without 22-23
##smooth or no? see phil work...

#interaction between vessel ID and year?
ggplot(corr_spot) +aes(y=ADFG.Number, x=Season.Ref, color=ADFG.Number) + geom_point()


#density plots
ggplot(corr_spot) + aes(x=CPUE_nom) + geom_density()
ggplot(corr_spot) + aes(x=log(CPUE_nom)) + geom_density()

ggplot(corr_spot) + aes(x=ADFG.Number) + geom_density()


#conclusion: my model will be: log(CPUE_nom) ~ factor(Season (year) variable) + time variable (smoothed?? Julian date or week?) + VESSEL effect + vessel #
##I don't tbink I'll need to add a constant, since none of the cpue's should be 0.

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


#corr_spot$Season.Ref <- relevel(corr_spot$Season.Ref, ref="01-02") #reference level selection was arbitrary, honesly. Try changing and see how that impacts results
#corr_spot$ADFG.Number <- relevel(corr_spot$ADFG.Number, ref="52131") # this vessel had the most fish tickets. try changing to one with average # of fish tickets?
#corr_spot$f.Stat.Week <- relevel(corr_spot$f.Stat.Week, ref= "41") #41 is the median and the mode. made it a factor here.
##ask PHil for advice on how to select this.
##try 01-02 also, since that was the old baseline


#LM model with week
lm_basic_global <- lm(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + f.Stat.Week, data=na.omit(corr_spot)) #factors are year, vessel, and stat week. Maybe stat week should be a gam. Maybe vessel_count should be smoothed
summary(lm_basic_global)

#LM model with julian date
lm_basic_global_jdate <- lm(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + jdate, data=na.omit(corr_spot))
summary(lm_basic_global_jdate) #why NA for season 22-23? does not make sense
alias(lm_basic_global_jdate)

#GAM modwl with week
gam_global_statweek <- gam(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + s(Stat.Week), data=na.omit(corr_spot))

#GAM model with Julian date
gam_global_jdate <- gam(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + s(jdate, k=4), data=(corr_spot))
summary(gam_global_jdate)
AIC(gam_global_statweek, gam_global_jdate)
BIC(gam_global_statweek, gam_global_jdate)
###if other factors are going to be smoothed, they need to be smoothed random effects. I'll need to read about that/
##also check for interaction effects

##MODEL SELECTION!!
gam_global_jdate_2 <- gam(log(CPUE_nom) ~ s(vessel_count, k=4) + Season.Ref + ADFG.Number + s(jdate, k=4), data=corr_spot) #k should be 4, not 3, for vessel count
BIC(gam_global_jdate_2, gam_global_jdate) #same same
summary(gam_global_jdate)
summary(gam_global_jdate_2)

BIC(gam_global_jdate_2, gam_global_jdate, lm_basic_global, lm_basic_global_jdate)

gam_3 <- gam(log(CPUE_nom) ~ Season.Ref + ADFG.Number + s(jdate, k=4), data=corr_spot)
gam_4 <- gam(log(CPUE_nom) ~ Season.Ref + ADFG.Number, data=corr_spot)
gam_5 <- gam(log(CPUE_nom) ~ Season.Ref + s(jdate, k=4), data=corr_spot)

BIC(gam_global_jdate, gam_global_jdate_2, gam_3, gam_4, gam_5) #says use gam_3 as the model
AIC(gam_global_jdate, gam_global_jdate_2, gam_3, gam_4, gam_5)

#ooh what about interaction effects

gam_global_int <- gam(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + s(jdate, k=4) + vessel_count:Season.Ref + ADFG.Number:Season.Ref, data=(corr_spot))
gam_global_int_2 <- gam(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + s(jdate, k=4) + vessel_count:Season.Ref, data=(corr_spot))
##hmm how to check int effects for gams?
summary(gam_global_int)
summary(gam_global_int_2)

BIC(gam_global_jdate, gam_global_jdate_2, gam_3, gam_4, gam_5, gam_global_int_2) #ok, keeping those interaction effects does not seem to make a difference. gam_3 is still our go to

#let's make a random effect
gam_3_ranef <- gam(log(CPUE_nom) ~ Season.Ref + s(ADFG.Number, bs="re") + s(jdate, k=4), data=corr_spot)
summary(gam_3_ranef)
BIC(gam_3, gam_3_ranef) #gam_3_ranef wins

############################
#the describe-the-data plots section
##################################
#predict response variable (the standardized cpue)
pred_ln_U <- predict(gam_3)

#attach data frame
df_std_CPUE <- data.frame(corr_spot, pred_ln_U)

#extract coeff
coef(gam_3)


#graph
ggplot(data=corr_spot, aes(x=Season.Ref, y=log(CPUE_nom), color=ADFG.Number)) +
  theme_linedraw() +
  geom_point() +
  #facet_wrap(~fArea) +
  geom_line(aes(y=pred_ln_U))
###end curry code

##need to make a prediction per year? how did phil do it?
# Data set of average variables to predict CPUE from: 
std_dat<- expand.grid(Season.Ref = as.factor(unique(corr_spot$Season.Ref)),
                              #Gear = "Mechanical jigs", #table(cpue_dat$Gear)
                              #Hooks = round(mean(cpue_dat$Hooks),0), 
                              #Line_no = 2, #mean(cpue_dat$Line_no) unique(cpue_dat$Line_no)
                              jdate = round(mean(corr_spot$jdate),0),   #  unique(cpue_dat$Jdate)
                              ADFG.Number = 52131 #table(corr_spot$ADFG.Number) #52131 #56332 #41228
                      )
pred_cpue <- predict(gam_3, std_dat, type = "response", se = TRUE)


#Put the standardized CPUE and SE into the data frame and convert to
#backtransformed (bt) CPUE
std_dat %>% 
  mutate(fit = pred_cpue$fit,
         se = pred_cpue$se.fit,
         upper = fit + (2 * se),
         lower = fit - (2 * se),
         bt_cpue = exp(fit),
         bt_upper = exp(upper),
         bt_lower = exp(lower),
         bt_se = (bt_upper - bt_cpue) / 2, #,
         bt_cv = bt_se/bt_cpue
  ) -> std_dat


#nominal cpue for comparison
 corr_spot %>%
  group_by(Season.Ref) %>%
  dplyr::summarise(log_fsh_cpue = mean(log(CPUE_nom),na.rm=T),
                   fsh_cpue = exp(log_fsh_cpue),
                   sd = sd(log(CPUE_nom),na.rm=T),
                   n = length(CPUE_nom),
                   se = sd / (n ^ (1/2)),
                   var = var(log(CPUE_nom)),
                   raw_cv = sd / fsh_cpue,
                   upper = exp(log_fsh_cpue + (2 * se)),
                   lower = exp(log_fsh_cpue - (2 * se)),
                   cv = (upper - fsh_cpue) / 2) -> fsh_sum_log
 
 #now I have the standardized CPUE: in std_dat dataframe, and nominal CPUE: in fsh_sum_log df.
 
 
 #my graph
 ggplot() + aes() + 
   geom_point(color="black", aes(x= Season.Ref, y= fsh_cpue), data=fsh_sum_log)+
   geom_errorbar(aes(x=Season.Ref, ymin=lower, ymax=upper), data=fsh_sum_log) + 
   geom_point(color="red", data= std_dat, aes(x=Season.Ref, y=bt_cpue)) +
   geom_errorbar(color="red", aes(x=Season.Ref, ymin=bt_lower, ymax=bt_upper), data=std_dat)
 #I've tested this with a few different reference vessels. REally seems to change, depending on my reference vessel. Is there a more robust way to do this?


#################
 #ranef mod
 ############################
 std_dat_ranef<- expand.grid(Season.Ref = as.factor(unique(corr_spot$Season.Ref)),
                       jdate = round(mean(corr_spot$jdate),0),   #  unique(cpue_dat$Jdate)
                       ADFG.Number = 52131 #table(corr_spot$ADFG.Number) #52131 #56332 #41228
 )
 pred_cpue_ranef <- predict(gam_3_ranef, std_dat_ranef, type = "response", se = TRUE)


 #Put the standardized CPUE and SE into the data frame and convert to
 #backtransformed (bt) CPUE
 std_dat_ranef %>% 
   mutate(fit = pred_cpue_ranef$fit,
          se = pred_cpue_ranef$se.fit,
          upper = fit + (2 * se),
          lower = fit - (2 * se),
          bt_cpue = exp(fit),
          bt_upper = exp(upper),
          bt_lower = exp(lower),
          bt_se = (bt_upper - bt_cpue) / 2, #,
          bt_cv = bt_se/bt_cpue
   ) -> std_dat_ranef
 
 #nominal cpue ranef
 #same as above. use the fsh_sum_log df
 
 #my graph
 ggplot() + aes() + 
   geom_point(color="black", aes(x= Season.Ref, y= fsh_cpue), data=fsh_sum_log)+
   geom_errorbar(aes(x=Season.Ref, ymin=lower, ymax=upper), data=fsh_sum_log) + 
   geom_point(color="red", data= std_dat_ranef, aes(x=Season.Ref, y=bt_cpue)) +
   geom_errorbar(color="red", aes(x=Season.Ref, ymin=bt_lower, ymax=bt_upper), data=std_dat_ranef)
 
 #repliction of max's graph
 #using the GAMM
 ggplot() + aes() +
   geom_point(data= std_dat_ranef, aes(x=Season.Ref, y=bt_cpue), size=2)+
   geom_errorbar( aes(x=Season.Ref, ymin=bt_lower, ymax=bt_upper), data=std_dat_ranef) +
   geom_line(data= std_dat_ranef, aes(x=Season.Ref, y=bt_cpue, group=Season.Ref), size=2)
 
 ggplot(data=std_dat_ranef) + aes(x=Season.Ref, y=bt_cpue) + #GOODPLOT
   geom_point(size=2)+
   geom_errorbar( aes(ymin=bt_lower, ymax=bt_upper)) +
   geom_line(aes(group=1))+ #that works+
   geom_hline(aes(yintercept=mean(std_dat_ranef$bt_cpue)), linetype="dashed")+
   theme_cowplot()+
   labs(y="Standardized CPUE (lbs/pots)", x="Season", title = "Upper Ernest Sound" )
 #dashed line is average std cpue for this area. is max's dashed line the std cpue for all areas?
 #more conservateive(smaller) CPUE's than max's graph. And what is going on in 22-23? Chaos. Is that legit or is something weird in the data?
 ##confidence intervals larger, std CPUE values are lower. Did the old method result in an overestimation? Or is my method an underestimation?
 
 #effort and harvest plot below. we will need: harvest and effort total(?) per year
 
 ############33
 #look more closely at the resdual and indivudal graphs.
 #what does the jdate graph look like?
 
###################################
#replicating max's graphs
 #################################
 #let's remove 22-23 from the dataset to have comparable data to the old method
 #also remove boat 99999?
 corr_spot_limited <- corr_spot %>%
   filter(Season.Ref != "22-23") %>%
   filter(ADFG.Number != "99999")
 
 corr_spot_limited$Season.Ref <- factor(corr_spot_limited$Season.Ref, levels=c("01-02", "02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20", "20-21", "21-22"))
   
   
 gam_3_ranef_limited <- gam(log(CPUE_nom) ~ Season.Ref + s(ADFG.Number, bs="re") + s(jdate, k=4), data=corr_spot_limited)
 gam_3_lim <- gam(log(CPUE_nom) ~ Season.Ref + ADFG.Number + s(jdate, k=4), data=corr_spot_limited)
 
 #some other models because why not
 gam_4_lim <- gam(log(CPUE_nom) ~ Season.Ref + s(vessel_count, k=3) + s(ADFG.Number, bs="re") + s(jdate, k=4), data=corr_spot_limited)
 gam_5_lim <- gam(log(CPUE_nom) ~ Season.Ref + s(vessel_count, k=4) + s(ADFG.Number, bs="re") + s(jdate, k=4), data=corr_spot_limited)
 
 
 BIC(gam_3_ranef_limited, gam_3_lim, gam_4_lim, gam_5_lim) #gam_3_ranef wins_limited wins. seems like vessel count does not have much explanatory power
 #hmm do I need to worry about smoothed interaction effects? is this a thing?
 
 #graph with the limited dataset
 #oh and predict std cpue:
 std_dat_ranef_lim<- expand.grid(Season.Ref = as.factor(unique(corr_spot_limited$Season.Ref)),
                             jdate = round(mean(corr_spot_limited$jdate),0),   #  unique(cpue_dat$Jdate)
                             ADFG.Number = 41228 #table(corr_spot$ADFG.Number) #52131 #56332 #41228
                      
 )
 pred_cpue_ranef_lim <- predict(gam_3_ranef_limited, std_dat_ranef_lim, type = "response", se = TRUE)
 
 
 #Put the standardized CPUE and SE into the data frame and convert to
 #backtransformed (bt) CPUE
 std_dat_ranef_lim %>% 
   mutate(fit = pred_cpue_ranef_lim$fit,
          se = pred_cpue_ranef_lim$se.fit,
          upper = fit + (2 * se),
          lower = fit - (2 * se),
          bt_cpue = exp(fit),
          bt_upper = exp(upper),
          bt_lower = exp(lower),
          bt_se = (bt_upper - bt_cpue) / 2, #,
          bt_cv = bt_se/bt_cpue
   ) -> std_dat_ranef_lim
 
 
 (a<-ggplot(data=std_dat_ranef_lim) + aes(x=Season.Ref, y=bt_cpue) + #Inconsistent results when ADFG.Number changes
   geom_point(size=3)+
   geom_errorbar( aes(ymin=bt_lower, ymax=bt_upper)) +
   geom_line(aes(group=1))+ #that works+
   geom_hline(aes(yintercept=mean(std_dat_ranef_lim$bt_cpue)), linetype="dashed")+
   theme_cowplot()+
   labs(y="Standardized CPUE (lbs/pots)", x=NULL, title = "Upper Ernest Sound" )+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))#+
   #  scale_y_continuous(breaks = c(1,2,3,4,5,6), expand=c(0,0))+
    # ylim(1.8, 5)
   ) 
 
 #add the nominal cpue in there, for comparison
 (a2<-ggplot(data=std_dat_ranef_lim) + aes(x=Season.Ref, y=bt_cpue) + #GOODPLOT
     geom_point(aes(x=Season.Ref, y=fsh_cpue), data=fsh_sum_log_lim, color="springgreen", size=3) + #adding nominal cpue as blue
     geom_line(aes(x=Season.Ref, y=fsh_cpue, group=1), data=fsh_sum_log_lim, color="springgreen") + 
     geom_point(size=3)+
     geom_errorbar( aes(ymin=bt_lower, ymax=bt_upper)) +
     geom_line(aes(group=1))+ #that works+
     geom_hline(aes(yintercept=mean(std_dat_ranef_lim$bt_cpue)), linetype="dashed")+
     theme_cowplot()+
     labs(y="Standardized or Nominal CPUE (lbs/pots)", x="Season", title = "Upper Ernest Sound" ))
 
 #my graph
 #nominal cpue for comparison
 corr_spot_limited %>%
   group_by(Season.Ref) %>%
   dplyr::summarise(log_fsh_cpue = mean(log(CPUE_nom),na.rm=T),
                    fsh_cpue = exp(log_fsh_cpue),
                    sd = sd(log(CPUE_nom),na.rm=T),
                    n = length(CPUE_nom),
                    se = sd / (n ^ (1/2)),
                    var = var(log(CPUE_nom)),
                    raw_cv = sd / fsh_cpue,
                    upper = exp(log_fsh_cpue + (2 * se)),
                    lower = exp(log_fsh_cpue - (2 * se)),
                    cv = (upper - fsh_cpue) / 2) -> fsh_sum_log_lim
 
 ggplot() + aes() + 
   geom_point(color="black", aes(x= Season.Ref, y= fsh_cpue), data=fsh_sum_log_lim)+
   geom_errorbar(aes(x=Season.Ref, ymin=lower, ymax=upper), data=fsh_sum_log_lim) + 
   geom_point(color="red", data= std_dat_ranef_lim, aes(x=Season.Ref, y=bt_cpue)) +
   geom_errorbar(color="red", aes(x=Season.Ref, ymin=bt_lower, ymax=bt_upper), data=std_dat_ranef_lim)
 
 #nominal cpue max graph
 ggplot(data=fsh_sum_log_lim) + aes(x=Season.Ref, y=fsh_cpue) + #GOODPLOT
   geom_point(size=2)+
   geom_errorbar( aes(ymin=lower, ymax=upper)) +
   geom_line(aes(group=1))+ #that works+
   geom_hline(aes(yintercept=mean(fsh_sum_log_lim$fsh_cpue)), linetype="dashed")+
   theme_cowplot()+
   labs(y="Nominal CPUE (lbs/pots)", x="Season", title = "Upper Ernest Sound" )
 
 
 #the second graph
 #need: harvest... and effort. 
 #dang, I did not keep those in my wrangle. Back to the wrangle
 ##wrangle done.
 #let;s make a df and plot of total weight and pots by season
 raw_totals <- corr_spot_limited %>%
   group_by(Season.Ref) %>%
   dplyr::summarize(Harvest = sum(total_weight),
                    Effort = sum(max_pots_2))
 
 (b<-ggplot(raw_totals) + aes(x=Season.Ref) +
   geom_point(aes(y=Harvest), shape=0, size=3)+ #harvest graph
   geom_line(aes(y=Harvest, group=1))+
   geom_point(aes(y=Effort), shape=15, size=3) +
   geom_line(aes(y=Effort, group=1), linetype="dashed") +
   theme_cowplot()+
   labs(x="Season", y="Harvest (lbs) or Effort (pots)")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1)))
 
 #combine the graphs
 library(patchwork)
 (ab <- a/b) #going to want to adjust axes before finalizing
 
 #save the plot
 ggsave("draft_std_cpue_ernest.png",plot=ab, width=7, height=8)
  
  
 
 #################################
###model residual examination/model diagnostics
 ###############################3
 
 
 
 ###############################
 #dealing with interaction effects
 ################################
 #ADFG.Number:Year; jdate:Year
 #while I'd prefer not to deal with jdate:year,ADFG.Number:Year is problematic and needs to be dealt with'
 #see the literature
 #one method: predicting (weighted?) CPUE values for all vessel #'s then averaging?
 
 model <- lm(CPUE_nom ~ Season.Ref * ADFG.Number, corr_spot_limited)
 summary(model)
 
 test_2 <-gam(log(CPUE_nom) ~ Season.Ref*ADFG.Number + s(jdate, k=4), data=corr_spot_limited)
 summary(test_2)
 
 #tyler advice: If you are having to create a prediction set to get to the standardized index, what I’ve done for vessel and other factors is use the mode. You could also expand your prediction set to use all the vessels and combinations of your other variables, and then average them with appropriate weighting (number of observations or something related to influence).
 #so, my current chosen model is:  gam_3_ranef_limited. and gam_3_lim is another option (without the random effects)
 ##first of all: should vessel ID be a fixed or random effect? Each CPUE observation is grouped by vessel ID. 
 #https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html
 #chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://web.pdx.edu/~newsomj/mlrclass/ho_randfixd.pdf
 #For example if g is a factor then s(g,bs="re") produces a random coefficient for each level of g, with the random coefficients all modelled as i.i.d. normal.

 ggplot(corr_spot_limited) + aes(x=ADFG.Number, y=log(CPUE_nom)) + geom_boxplot(aes(group=ADFG.Number))
 summary(gam_3_ranef_limited)
 summary(gam_3_lim)
 
 BIC(gam_3_ranef_limited, gam_3_lim)
 
 #let's try fixed effect first, for simplicity. Might do random effect later.
 #so the model itself is fine. but the predication needs to change and average over everything. And also weight by... vessel freqency? by catch?
 #first, let's make the big prediction dataset
 new_dat_for_avg<- expand.grid(Season.Ref = as.factor(unique(corr_spot_limited$Season.Ref)),
                                 jdate = round(mean(corr_spot_limited$jdate),0),   #  unique(cpue_dat$Jdate)... might need to expand and average this over all dates too
                                 ADFG.Number = as.factor(unique(corr_spot_limited$ADFG.Number)) #table(corr_spot$ADFG.Number) #52131 #56332 #41228
                                 
 )
 pred_for_avg <- predict(gam_3_lim, new_dat_for_avg, type = "response", se = TRUE)
 
 
 #second, we apply the weighted average
 ##need to calculate vessel frequency...
 
 new_dat_for_avg %>% 
   mutate(fit = pred_for_avg$fit,
          se = pred_for_avg$se.fit
   ) -> new_dat_for_avg
 
 new_dat_for_avg %>% #need to qc this!!!!!! Shit am I not git connected right now?
   group_by(ADFG.Number) %>%
   dplyr::mutate(ADFG.no.count = n()) %>%
   ungroup() %>%
   group_by(Season.Ref) %>%
   dplyr::summarise(Weighted_avg_fit = weighted.mean(x=fit, w=ADFG.no.count), #maybe I'm grouping wrong. I dont want to summarize per vessel, I want to summarize over all vessels per year. Oh, I want to regroup by year for this second part
                    Weighted_avg_se = weighted.mean(x=se, w=ADFG.no.count)) -> new_dat_for_avg_2 #%>% # hmm what do I want this to look like? see other predict table : View(std_dat_ranef_lim )
  # ungroup()   #error above. Hmm. Maybe I need to weight in separate steps.
 
 new_dat_for_avg %>% 
   mutate(fit = pred_for_avg$fit,
          se = pred_for_avg$se.fit,
          upper = fit + (2 * se),
          lower = fit - (2 * se),
          bt_cpue = exp(fit),
          bt_upper = exp(upper),
          bt_lower = exp(lower),
          bt_se = (bt_upper - bt_cpue) / 2, #,
          bt_cv = bt_se/bt_cpue
   ) -> new_dat_for_avg
 
 
 