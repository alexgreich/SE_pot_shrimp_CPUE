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




#m_extra_nesting


#play around with nesting: stat area within analysis area
#DOES MAX MAKE DECSIONS BY ANALYSIS AREA OR BY MGMT UNIT??


#aside about looking at the correlation of factors
?model.matrix
model.matrix(wrangled_shrimp %>% select(Season.Ref, ADFG.Number) )

library(ggcorrplot)
?ggcorrplot


###phtest
library(plm)
?plm
?phtest
#fix <- 
 # ran <- 
  
  

#  I chose model 2, which has year(season.ref) as the fixed effect and ADFG Number, Analysis areas as random effects.
#- maybe try adding temporal autocorrelation to this one??
#  ```{r}
#plot(mod_g_2)

#what if I did all analysis areas separatly? Was it appropriate to group them together in a managment unit?

#predicted results
predicted <- predict(mod_g_2)
plot(predicted)

#predicted results after a grid method where boats are averaged or something (see Ernest sound code. DO this, make big graph, make graphs for individual areas, them compare to spot shrimp ernest sound)


Analysis and model testing: district 7 GAM with random effects
GAMM with ar1 autocorrelation (is this too complciated?)
```{r}
#null model
m0 <- gam(log(CPUE_nom + 0.001) ~ factor(Season.Ref) ,data= D7 ) #how to add temporal autocorrealtion to this tho?

#intermediate models


#global model
m_glob_r <- gam(log(CPUE_nom + 0.001) ~ Season.Ref + ADFG.Number+ s(vessel_count_mgmt_u, k=4) + s(jdate, k=4) + s(Analysis.Area, bs="re", k=4), data=D7 ) #remove jdate? #crap should ADFG.Number be a\random effect too?
##do I need to make this a factor somewhere else?


##how to alter the random effect to have a random global slope? See that paper.
#maybe try the GI, GS, G, I, S method like in the Pederson paper.


#first decide if ranef or not, via global model?


#https://stackoverflow.com/questions/47586110/autocorrelation-in-generalized-additive-models-gam  #how to add ar1 to my gam
##is this necessary or too complicated?
##gamm() with correllation = corAR1(form = ~time) where time is the var giving me ordering in time of the evently spaced observations
##or bam() and specify rho, the ar1 parameter
###check model residuals tho to see if we NEED ar1
```



The analysis above gives me:
  Analysis area probs matters as a ranef
ADFG ID matters as a ranef (or fixef, what did Phil and Tyler do?)
Can include number of vessels but I think it will get cut out. Unsure if smooth or linear, try both.

I do not think I should use jdate.


Alright round 2 mod selection after thinking about it a bit, following logic above.
log(CPUE+0.001) ~ Season.Ref + s(vessel_count_aa, k=4 or 3) + s(Analysis.Area, bs="re") + s(ADFG ID, bs="re") and int effects?
  int effects between Season ref and analysis area
int effects between season ref and ADFG ID
```{r}
mod_g_new <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "ML", data=D7)

summary(mod_g_new)

#(do I test temporal int effects before adding temporal autocorrelation??)
#is there temporal autocorrelation

acf(residuals(mod_g_new),main="raw residual ACF") #yep. AR1?
pacf(residuals(mod_g_new),main="raw residual ACF") 
#based on these graphs, try 2nd, 3rd, or 4th order autocorrelation

gamm_corr_3 <- gamm(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7, correlation = corARMA(form = ~1|Season.Ref, p=4))

acf(residuals(gamm_corr_3$gam),main="raw residual ACF") #yep. AR1?
pacf(residuals(gamm_corr_3$gam),main="raw residual ACF") 


#add correlation structure
mod_g_new_gamm <- gamm(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7) #no autocorr
summary(mod_g_new_gamm$gam)

gamm_corr <- gamm(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7, correlation = corAR1(0.4,form = ~1|Batch.Year)) #yes autocorr 
summary(gamm_corr$gam)

gamm_corr_0.5 <- gamm(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7, correlation = corAR1(0.4,form = ~Season.Ref|Batch.Year))
#form = ~ Season.Ref had to delete from inside the corAR1 argument
#I do not know if this is right
?corAR1

gamm_corr_2 <- gamm(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7, correlation = corAR1(form = ~1|Season.Ref))

summary(gamm_corr$gam)
acf(residuals(gamm_corr$gam),main="raw residual ACF") #well that did not work
acf(residuals(gamm_corr_2$gam),main="raw residual ACF")

AIC(gamm_corr, mod_g_new_gamm) #mgith have to go $gam

#try second, third

#adding autocorrelation strucutre makes everything worse

```


Testing fixed effects: 04/26/24
```{r}
mod_g_new <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7)
summary(mod_g_new)

mod_g_2 <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7)

mod_g_3 <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7)
summary(mod_g_new)

mod_g_1 <- gam(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "REML", data=D7)

AIC(mod_g_new, mod_g_2, mod_g_3, mod_g_1)

#says can get rid of vessel count
summary(mod_g_2)

mod_g_2.1 <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(Analysis.Area, by=Season.Ref, bs="re") + s(ADFG.Number, by=Season.Ref, bs="re"), method = "REML", data=D7) #shit how to add interaction effects
##this is certainly taking forever to run
##maybe I am overcomplicating and should stick with mod_g_2
##I dont think phil does any of this by= crap
##ok that takes way too long

#AIC(mod_g_2, Mod_g_2.1)

#try dropping ADFG.Number, see what happens?

mod_g_2.2 <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(Analysis.Area, bs="re"), method = "REML", data=D7)

mod_g_2.3 <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(ADFG.Number, bs="re"), method = "REML", data=D7)

AIC(mod_g_2, mod_g_2.2, mod_g_2.3)

```


Model selection with Tyler's function
```{r}
library(MASS)
stepAIC()
```

