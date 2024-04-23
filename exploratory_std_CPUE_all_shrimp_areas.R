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

#wragnle spot shrimp by district
unique(all_shrimp_w_analysis_area$district)
unique(all_shrimp_w_analysis_area$Management_unit)

#what do all of the districts correspond to??
#102 112 115 101 103 113 111 107 106 110 108 116 183 104 105 109 114 181- dont use 181 for analysis, barely any fishing


dist_101_pot_shrimp <- wrangle.spot.shrimp.by.district(all_shrimp_w_analysis_area, 101)

#the only coon district
dist_15_coon_shrimp <- wrangle.coonstripe.shrimp.by.district(all_shrimp_w_analysis_area, 115)


#filter using mgmt unit (more practical)

mgmt_u_District_7 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 7")
#View(mgmt_u_District_7)
str(mgmt_u_District_7) # 4639 by 16. 
str(filter(mgmt_u_District_7, Analysis.Area=="Upper Ernest Sound")) #ok that looks about right


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
###I'll liekly have to write a special function for D16 and D11, based on what max says. It will be really close to other functions, so not a huge effort
#the coons
mgmt_u_District_15E <- wrangle.coon.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "District 15 East")
mgmt_u_R_District_15 <- wrangle.spot.shrimp.by.mgmt.unit(all_shrimp_w_analysis_area, "Remainder District 15")
#will likely need to make additional fucntion, revise for D16 and D11. See max email about those districts using coon and spot shrimp, both, for CPUE.


#qc, make sure mgmt unit has only the stat areas you want
###I suspect that there might be a surprise extra stat area or two in the older data
#d7 ### Bradfield canal, lower E sound, Upper E sound, Zimovia strait
#d1
#d2
#d4
#d5
#d9


#combine to make master cleaned data with cpue
#REVISE!!
wrangled_shrimp <- rbind(mgmt_u_District_1, mgmt_u_District_2, mgmt_u_Section_3A, mgmt_u_Section_3B, mgmt_u_Tenakee_Inlet, mgmt_u_R_District_12,
      mgmt_u_R_District_11, mgmt_u_District_7, mgmt_u_North_Clarence, mgmt_u_N_Fred_Sound, mgmt_u_Seymour, mgmt_u_S_Fred_Sound,
      mgmt_u_N_Sumner_Strait, mgmt_u_District_4, mgmt_u_District_5, mgmt_u_District_9, #)#, dist_15_coon_shrimp) #16 total, 17 with the coons
      mgmt_u_Section_13AB, mgmt_u_Section_13C, mgmt_u_District_14, mgmt_u_District_16,#adding the ones I missed from page 2.  
mgmt_u_District_15E, mgmt_u_R_District_15) #the coons

##################################################################################################
#exploratory data analysis (make some graphs!!)

#is my data nomrally distributed?
##by full region?\
#hist(wrangled_shrimp$CPUE_nom)
ggplot(wrangled_shrimp) + aes(x=CPUE_nom) + geom_density()
mean<-mean(log(wrangled_shrimp$CPUE_nom))
ggplot(wrangled_shrimp) + aes(x=log(CPUE_nom)) + geom_density()# + geom_vline(aes(xintercept = mean(log(CPUE_nom))))
#log-tranformed looks pretty damn normal
qqnorm(log(wrangled_shrimp$CPUE_nom+1)) #pretty good

##by mgmt unit?
ggplot(wrangled_shrimp) + aes(x=log(CPUE_nom)) + geom_density() + facet_wrap(~Management_unit) 


##by analysis area?



names(mgmt_u_District_7)
ggplot(mgmt_u_District_7) + aes(x=Analysis.Area, y=CPUE_nom) + geom_boxplot()
#what stat areas does this include?
### Bradfield canal, lower E sound, Upper E sound, Zimovia strait


#PHIL STUFF BELOW
## 3) Examine variability in the variables that may affect cpue:
##    a) Variables as a function of TIME: 
##### by vessel ID
##### by fishing area
##### fishing area by stat area
# Time variables: jdate, Season, month, week 

#by stat area (see phil code line 353 of Jig_BRF_CPUE.R)

#CPUE AS A FUNCTION OF TIME - see line 378 of phil code
##vessel ID
## # vessels
##other covariates and ranefs
##jdate
##by fishing area
##by stat area

#CHECK COR BETWEEN VARIABLES - PHIL CODE 443

#MAKE DATA FACTORS THAT NEED TO BE FACTORS?


###wragnle Q's:
# do I care about stray stat areas not included? Dist 15 not mentioned ?

#variables that I care about
##season ref (year) - a factor
##vessel count (how many vessels)- not a factor, but an integer
##ADFG number (vessel ID) - a factor
##jdate - a smoothed (cyclical?) variable
##analysis area - a random effect (Analysis.Area)
##mgmt unit aka fishery - split up by this, and in the region-wide analysis, a random effect

#look at some global graphs
## by time
ggplot(wrangled_shrimp) + aes(x=factor(Season.Ref), y= vessel_count) + geom_point() #fuck, is vessel count per area
ggplot(mgmt_u_District_1) + aes(x=factor(Season.Ref), y= vessel_count) + geom_point() #yes, yes it is.
mgmt_u_District_1 %>% filter(Analysis.Area=="Portland Canal") %>%
  ggplot(aes(x=factor(Season.Ref), y= vessel_count)) + geom_point() #ok. need to add up vessel count by analysis area and year, to get the total vessel count by district in a year

#vessel count over each mgmt unit by uear. SOMETHING IS WRONG>
wrangled_shrimp_2 <- wrangled_shrimp %>%
  group_by(Season.Ref, Management_unit) %>% #group by this ok?
  mutate(vessel_count_by_mgmt_unit = sum(vessel_count)) %>%
  ungroup() 
##let's check if that worked
wrangled_shrimp_2 %>% filter(Management_unit == "District 1") %>% #there we go, looks much better. Need to QC that wrangle tho. There were not 6000 fishing bessels in a year right? something is still uip
   ggplot(aes(x=factor(Season.Ref), y= vessel_count_by_mgmt_unit)) + geom_point()

## correlations

#split by unit
##look by analysis area

###########################################################################################
###############################################################################################
###################################################################################################
#Analysis
library(mgcv)
##do I need to incorporate survey info, or is it already incorporated?

#for ENTIRE REGION 1 (including stray areas?) (are there stray areas?)  
#does it makes sense to look at these things at the region level? #kind of makes more sense to look at things at the mangemtn unit level, I would think
###oh a random effects model with nested random effecs (where did I do this before??) - other shrimp CPUE stuff
###random effects: area nested within management unit
###glmm, probably
#gamm with nested random effects (maybe look at tyler code again tho)


#by Management Unit
##start here
##Wait. This will be a mixed effects model, with the random effects being the analysis area.
### I do not think I will weigh each analysis area. I think it is not relevant, the way I calculate things. The model will know which area has more fishing/more entries
### glmm, probably
#gamm with: something like this: gam(log(CPUE_nom) ~ vessel_count + Season.Ref + ADFG.Number + s(jdate, k=4), data=(corr_spot))
##but I need to see examples. Season ref is temporally autocorrelated, or should be at least. How to account for that.
#gamm with temporal autocorrealtion. Also, analysis areas (wihin mgmt units) should be a ranef
#formula gloval mod like this: glob_mod ~ gam(log(CPUE_nom+1) ~ vessel_count + ADFG.Number(the vessel ID) + SOME YEAR INDICATOR + random effect(Analysis.Area(do I need to smooth? try out random intercept and slope)) +s(jdate); cor=AR1 (account for seasonal autocorrelation somehow))
##Gam() arguments to check out: offset (should use instead of cpue_nom+1?) #reminds me of Matt cpue paper, I should have it on this compiter
####family. Can I use a log link instead of log(CPUE)? Does it matter?
#####select - lets gam remove terms from the model?H 
##### H is the argument for a coeff matrix, but something about a penalty

## 6) Calculate standardized CPUE based on: FROM PHIL's CODE
##    a) loggam: gam using log(set_ppm+0.1)
##      i) Run null models (CPUE ~ Year + one variable at a time)
##      ii) Run Global models (CPUE ~ YEAR + all variables) (drop one variable at a time)
##      iii) Run intermediate models to arrive at choice for standardizing model
##           NOTE: Chose to favor BIC over AIC to be more conservative in deciding
##                 which variables to include.  Overfitting will smooth out the 
##                 indices and remove contrast.
##      iv) Standardize CPUE index, compare to nominal

#null model with one idditional facgtor by year (line 518 of phil code)
m0 <- gam(log(CPUE_nom + 1) ~ factor(Batch.Year), data=wrangled_shrimp) #use batch year or season? Which season does batch year correspond to?



#consider trying out a glmm also. But gamm would be good for jdate


#by Area
##See tyler code. Create a function that will do it for me




############################################################################################


#QUESTIONS
###which districts/areas use coons insetad of spot shrimp for stock assessment purposes #district 15 uses coons. #is this dist.... 115?
###what is the VALUE column in the raw data?
###what is the spot shrimp and coon code? Spot is 965, coon is 964