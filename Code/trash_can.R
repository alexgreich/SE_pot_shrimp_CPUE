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




Model diagnostics

```{r}
#wil tyler's funciton work for me?
f_diagnostics <- function(x) {
  
  # qq plot
  ggplot()+
    stat_qq_line(aes(sample = resid(x, type = "deviance")), color = 2)+
    stat_qq(aes(sample = resid(x, type = "deviance")))+
    labs(x = "Theorical Quantiles", y = "Deviance Residuals", title = "Q-Q plot") -> p1
  
  # historgam
  ggplot()+
    geom_histogram(aes(x =  resid(x, type = "deviance")))+
    labs(x = "Residuals", y = "Count", title = "Histogram of residuals") -> p2
  
  # resid
  ggplot()+
    geom_point(aes(x = x$linear.predictors, y = resid(x, type = "deviance")))+
    geom_hline(yintercept = 0, linetype = 2)+
    labs(x = "Linear Predictor", y = "Residuals", title = "Resids. vs linear pred.") -> p3
  
  # resid vs fitted 
  ggplot()+
    geom_point(aes(x = x$fitted.values, y = x$model$cpue))+
    labs(x = "Fitted Values", y = "Response Values", title = "Response vs. Fitted") -> p4
  
  
  (p1 + p2) / (p3 + p4)
  
}
#end tyler's function

f_diagnostics(m_glob6)
```


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

#DISTRICT 1
Could use bam and bam syntax to cut down run time
https://m-clark.github.io/posts/2019-10-20-big-mixed-models/
```{r}
mod_b_1 <- bam(log(CPUE_nom+0.001) ~ Season.Ref + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "ML", data=D1)
mod_b_2 <- bam(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re")+
                 s(Season.Ref, Analysis.Area, bs="re") + s(Season.Ref, ADFG.Number, bs="re"), method = "ML", data=D1, discrete=T, nthreads = 16)
```

Run these models overnight- DO NOT RUN, 
too many singularities
```{r}
M15 <- lm(log(CPUE_nom+0.001) ~ Season.Ref * ADFG.Number * Analysis.Area, data=D1)
#M16 <- lm(log(CPUE_nom+0.001) ~ Season.Ref + ADFG.Number + Analysis.Area + ADFG.Number:Analysis.Area, data=D1
#stats::step(M12) #test
stats::step(M15)
summary(M15)

AIC(M12, M15)

```

#DISTRICT 7




Some wild experimentation below,
delete later and keep what is useful
```{r}
#null model
m0 <- gam(log(CPUE_nom + 0.001) ~ factor(Season.Ref) ,data= D7, method="ML" ) #how to add temporal autocorrealtion to this tho?

m_glob_r <- gam(log(CPUE_nom + 0.001) ~ Season.Ref + s(ADFG.Number, bs="re")+ s(vessel_count_mgmt_u, k=4) + s(jdate, k=4) + s(Analysis.Area, bs="re", k=4), data=D7 , method="ML") 

m_glob_r_2 <- gam(log(CPUE_nom + 0.001) ~ Season.Ref+ ADFG.Number + s(vessel_count_mgmt_u, k=4) + s(jdate, k=4) + s(Analysis.Area, bs="re", k=4), data=D7, method="ML" )

m_glob_nr <- gam(log(CPUE_nom + 0.001) ~ Season.Ref + ADFG.Number+ s(vessel_count_mgmt_u, k=4) + s(jdate, k=4), data=D7, method="ML") 

#do I need random effects in the model, and which random effects?
AIC(m_glob_r, m_glob_r_2, m_glob_nr, m0) #m glob r wins, but not by much


#get rid some variables
summary(m_glob_r)
m_3 <- gam(log(CPUE_nom + 0.001) ~ Season.Ref + s(ADFG.Number, bs="re")+ s(jdate, k=4) + s(Analysis.Area, bs="re", k=4), data=D7, method="ML" ) 

AIC(m_glob_r, m_3) #same, meaning drop vessel count
summary(m_3)

#oh shit tho, let me check temporal autocorrelation, maybe I should check for interactions
?acf
acf(residuals(m_glob_r),main="raw residual ACF") #yeah, some autocorrelation

#so...what do I do?

#try the global model with interaction effects, see if autocorrelation still exists
#https://r.qcbs.ca/workshop08/book-en/gam-with-interaction-terms.html
#m_glob_r_int <- gam(log(CPUE_nom + 0.001) ~ Season.Ref + s(ADFG.Number, bs="re")+ s(vessel_count_mgmt_u, k=4) + s(jdate, k=4) + s(Analysis.Area, bs="re", k=4), data=D7 , method="ML") +
 #s() #interaction effects

#ok ummm, maybe do smaller levels first to chekc for int
mod_int_1 <- gam(log(CPUE_nom + 0.001) ~ Season.Ref + s(ADFG.Number, bs="re")+ s(vessel_count_mgmt_u, k=4) + s(vessel_count_mgmt_u, by=Season.Ref, k=4) + s(Analysis.Area, bs="re", k=4), data=D7 , method="REML") #

summary(mod_int_1)
summary(m_glob_r)


```


But wait do I even, like, need a GAM?
m_glob_r <- gam(log(CPUE_nom + 0.001) ~ Season.Ref + s(ADFG.Number, bs="re")+ s(vessel_count_mgmt_u, k=4) + s(jdate, k=4) + s(Analysis.Area, bs="re", k=4), data=D7 , method="ML") 
```{r}
#remove outleir
max(D7$CPUE_nom)
D7_no_o <- D7 %>% filter(CPUE_nom < 100)

#log cpue vs. ADFG number (ok thats a radom effect)
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=ADFG.Number) + geom_boxplot()
###oh maybe this should be a special random effect. I forget
##this: Random factor smooth interactions
#bs="fs" A special smoother class (see smooth.construct.fs.smooth.spec) is available for the case in which a smooth is required at each of a large number of factor levels (for example a smooth for each patient in a study), and each smooth should have the same smoothing parameter. The "fs" smoothers are set up to be efficient when used with gamm, and have penalties on each null sapce component (i.e. they are fully ‘random effects’). from: https://stat.ethz.ch/R-manual/R-patched/RHOME/library/mgcv/html/smooth.terms.html
#or just a regular random effect. can try both ways. why not

#log cpue vs. season ref (is this smoothed in phil's code??)
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Season.Ref) + geom_boxplot(outliers=F) #totally autocorrelated

#log cpue vs. Analysis area (also a random effect)
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Analysis.Area) + geom_boxplot() #ok they're not... that different
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Analysis.Area) + geom_boxplot(outliers=F)
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Analysis.Area) + geom_violin()

ggplot(D7) + aes(y= CPUE_nom, x=Analysis.Area) + geom_boxplot(outliers=F)
ggplot(D7) + aes(y= CPUE_nom, x=Analysis.Area) + geom_violin()

#log cpue vs jdate (is this worth including, because the season might not be so important to look at, migjt give uneven results since fishing season recently shifted)
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=jdate) + geom_smooth() #i think maybe jdate isnt necessary because the fishing season is a specific window.

#log cpue vs. vessel count
##vessel count by year and analysis area
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=vessel_count_aa) + geom_point() + geom_smooth()
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=vessel_count_aa) + geom_smooth() + geom_point() #ok that puts thigs in perspective
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=vessel_count_aa) + geom_smooth(method = "lm")# + geom_point()
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=vessel_count_aa) + geom_smooth(method = "lm") + geom_point()
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=vessel_count_aa) + geom_point()

ggplot(D7_no_o) + aes(y= CPUE_nom, x=vessel_count_aa) +geom_point()+ geom_smooth()
ggplot(D7_no_o) + aes(y= CPUE_nom, x=vessel_count_aa, group=vessel_count_aa) +geom_boxplot()+ geom_smooth()
ggplot(D7_no_o) + aes(y= CPUE_nom, x=vessel_count_aa, color=Season.Ref) +geom_point()
ggplot(D7) + aes(y= CPUE_nom, x=vessel_count_aa) + geom_smooth(method = "lm") #ok teh change in cpue is super small. Like, 1 pound. Idbutk if that matters 
ggplot(D7_no_o) + aes(y= CPUE_nom, x=vessel_count_aa) + geom_smooth(method = "lm") + geom_point()

##vessel count by year and mgmt u
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=vessel_count_mgmt_u) + geom_smooth()
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=vessel_count_mgmt_u) + geom_point()


#investigate relationships for random effects
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Season.Ref) + geom_point()+ facet_wrap(~Analysis.Area)
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Season.Ref, color=Analysis.Area) + geom_boxplot() + theme_cowplot() # i would like to do a geom smooth plot of this too
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Season.Ref, color=Analysis.Area) + geom_violin() + theme_cowplot()
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Season.Ref, color=ADFG.Number) + geom_boxplot() + theme_cowplot() + theme(legend.position= "none")

##your experience depends on what group you are inb

#what is an average fishing vessel that is in all locations
ggplot(D7) + aes(y= log(CPUE_nom+0.001), x=Analysis.Area, color=ADFG.Number) + geom_boxplot() + theme_cowplot() #+ theme(legend.position= "none")


```




So, I wanted a model without smoothed effects. I probs should not use the gam function, but nlme or lme4 or something
Test if ranefs are necessary, please
It should be a glmm I think its a glmm. 4/29/24
- shoule ADFG.number be random or fixed effect????
  - year should be fixed effect
- area should be random effect
- do year and area have interaction effects? Do year and vessel have interaction effects? do vessel and year have interaction effects?
  --what is the best ranef structure??
  - correlation matrix include??
  - test vessel count (aa) as fixef too.
- maybe add week instead of jdate??
  read the highlighted refs in the mauder and punt paper
```{r}
library(lme4)
library(lmerTest)


mod_g_new <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(vessel_count_aa, k=4) + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "ML", data=D7)
summary(mod_g_new)
#the smoothed model, for funzies

mod_g_2 <- gam(log(CPUE_nom+0.001) ~ Season.Ref + s(Analysis.Area, bs="re") + s(ADFG.Number, bs="re"), method = "ML", data=D7)

AIC(mod_g_new, mod_g_2) #the more complex one wins(with vessel cound as a smoother). I do not undersand tho, why we would want to make vessel count smooth


#does vessel count matter? I do not think so.
#do area and year have interaction effects
#test just with lm for above
#do area and vessel have interaction effects

#global model - writing these, need sun
#m_interactions
#m_glob <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + (1|ADFG.Number) + (1+ Season.Ref|Analysis.Area), data=D7, REML=F) #not run, takes forevet
#m_glob_0.1 <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + (1 + Season.Ref|ADFG.Number) + (1+ Season.Ref|Analysis.Area), data=D7, REML=F) #not run, takes forever
#takes forever

m_glob2 <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + ADFG.Number + (1|Analysis.Area), data=D7, REML=F)
m_glob3 <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + (1|ADFG.Number) + (1|Analysis.Area), data=D7, REML=F)
m_glob4 <- lm(log(CPUE_nom+0.001) ~ Season.Ref + ADFG.Number + Analysis.Area, data=D7)
m_glob5 <- lm(log(CPUE_nom+0.001) ~ Season.Ref + ADFG.Number + Analysis.Area + vessel_count_aa, data=D7)
#m_glob5.2 <- lm(log(CPUE_nom+0.001) ~ Season.Ref * ADFG.Number * Analysis.Area * vessel_count_aa, data=D7) #takes forever
m_glob6 <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + (1|ADFG.Number) + (1|Analysis.Area), data=D7, REML=F) #gonna want to graph correlations
m_glob6_I <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + Season.Ref:vessel_count_aa + (1|ADFG.Number) + (1|Analysis.Area), data=D7, REML=F)
m_glob7 <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + ADFG.Number + (1|Analysis.Area), data=D7, REML=F)
#m_glob7_I <- lmer(log(CPUE_nom+0.001) ~ Season.Ref * vessel_count_aa * ADFG.Number + (1|Analysis.Area), data=D7, REML=F) #takes forever
m_glob8 <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + (1|ADFG.Number) + Analysis.Area, data=D7, REML=F)

AIC(m_glob2, m_glob3, m_glob4,m_glob5,  m_glob6, m_glob6_I, m_glob7, m_glob8) #says glob 5. Let's chgeck for correlation tho. 6 is better than 3, if we choose ranef
summary(m_glob2)
summary(m_glob4)
summary(m_glob5) #the AIC prefers the linear model
summary(m_glob6) #but conceptually, I think we should go with this model
summary(m_glob6_I)
#vcov(m_glob6_I)

#based on theory, this is what I think the model should be:
##m_glob6, m_glob6_I, or m_glob7. 
AIC(m_glob6, m_glob6_I, m_glob7) #m_glob7_I appears to take forever
BIC(m_glob6, m_glob6_I, m_glob7, m_glob5) #syas 7 and 5 are close to each other
library(MuMIn)
AICc(m_glob6)
AICc(m_glob6_I)
AICc(m_glob7)
AICc(m_glob5) #need to account for those interaction effects tho


#REML the chosen model before mocing on
##m_glob6_I makes sense to me theoretically, even tho it does not win by model selection numbers. I should consider making ADFG number a fixed effect tho

m_glob6_IR <- lmer(log(CPUE_nom+0.001) ~ Season.Ref + vessel_count_aa + Season.Ref:vessel_count_aa + (1|ADFG.Number) + (1|Analysis.Area), data=D7, REML=T) #season ref could be year. Would it make a difference?



```