##D7 R code

#alex REich 8/1/24


#load libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)
library(RColorBrewer)
library(mgcv)


#Load the data and select for D7
wrangled_shrimp <- read.csv("Data/wrangled shrimp focus years.csv")
D7 <- wrangled_shrimp %>% filter(Management_unit == "District 7") %>%
  mutate(Analysis.Area = factor(Analysis.Area),
         ADFG.Number = factor(ADFG.Number),
         Season.Ref = factor(Season.Ref)
  )#make things factors for the gams

unique(D7$Analysis.Area)

#############################################################
#exploratory plots
ggplot(D7) + aes(x=CPUE_nom) + geom_density()
ggplot(D7) + aes(x=log(CPUE_nom+0.001)) + geom_density()
qqnorm(log(D7$CPUE_nom)) #that kind of looks bad. Maybe somehting other than log-trans?
qqnorm(D7$CPUE_nom)
#ggplot(D7) + aes(x=CPUE_nom) + geom_density()+facet_wrap(~Analysis.Area)
ggplot(D7) + aes(x=log(CPUE_nom+0.001)) + geom_density()+facet_wrap(~Analysis.Area)

#cpue by year
#ggplot(D7) + aes(x=factor(Season.Ref), y=CPUE_nom) + geom_point() #ther is a big outlier
ggplot(D7) + aes(x=factor(Season.Ref), y=log(CPUE_nom)) + geom_boxplot(outliers = F)

ggplot(D7) + aes(x=factor(Season.Ref), y=log(CPUE_nom)) + geom_boxplot(outliers = F) +  facet_wrap(~Analysis.Area) #cpue varies... cyclically with time. Is season a fixed effect? I want to estimate it, so probs not. its def not linear.

#catch by year
ggplot(D7) + aes(x=factor(Season.Ref), y=total_weight) + geom_boxplot() #this is total weight per fish ticket.
ggplot(D7) + aes(x=factor(Season.Ref), y=max_pots_2) + geom_boxplot()
ggplot(D7) + aes(x=factor(Season.Ref), y=max_pots_2) + geom_boxplot(outliers=F)

ggplot(D7) + aes(x=factor(Season.Ref), y=total_weight) + geom_boxplot() + facet_wrap(~Analysis.Area) #max pots per fish ticket
ggplot(D7) + aes(x=factor(Season.Ref), y=max_pots_2) + geom_boxplot(outliers=F) + facet_wrap(~Analysis.Area)


#cpue and vessel count
ggplot(D7) + aes(x=factor(vessel_count_mgmt_u), y=log(CPUE_nom)) + geom_boxplot()
ggplot(D7) + aes(x=factor(vessel_count_mgmt_u), y=log(CPUE_nom)) + geom_boxplot(outliers=F)
ggplot(D7) + aes(x=factor(vessel_count_mgmt_u), y=log(CPUE_nom)) + geom_boxplot(outliers=F) + facet_wrap(~Analysis.Area)
############################################################################################################################



#drop the outlier for now- a data entry error?
D7 <- D7 %>% filter(ADFG.Number!= 99999)
unique(D7$ADFG.Number)

#Remove ADFG vessel from consideration (add this to shrimp prep function later)
D7 <- D7 %>% filter(ADFG.Number!= 99999)
unique(D7$ADFG.Number)



###########################################################################################
#Imputation: Last Observation Carried Forwards (LOCF)
D7$logCPUE <- log(D7$CPUE_nom) #I do not need the +0.001 because everything is positive/ not 0

temp2 <- expand.grid(Season.Ref = unique(D7$Season.Ref),
                     Analysis.Area = unique(D7$Analysis.Area)
)
D7_sub <- D7 %>% select(Season.Ref, Analysis.Area, ADFG.Number, logCPUE)
D7_sub2 <- full_join(D7_sub, temp2)
#make missing values for ADFG.Number the mode
which.max(table(D7_sub2$ADFG.Number)) #52131

mode_boat <- which.max(table(D7$ADFG.Number))

mode_val <- names(mode_boat)

D7_sub3 <- D7_sub2 %>%
  mutate(ADFG.Number = replace_na(ADFG.Number, mode_val)) #give the missing value the mode fishing vessel

############################################
#filling in the missing year value with the mean of the last year
##############################################
# Step 1: Calculate the mean logCPUE for each season
season_means <- D7_sub3 %>%
  group_by(Season.Ref, Analysis.Area) %>%
  summarise(mean_logCPUE = mean(logCPUE, na.rm = TRUE)) %>%
  arrange(Season.Ref, Analysis.Area)

# Step 2: Join the season means back to the original dat
D7_filled_2 <- D7_sub3 %>%
  left_join(season_means)

# Step 3: Fill the NA values with the mean from the previous season
D7_filled_2 <- D7_filled_2 %>%
  arrange(Season.Ref) %>%
  group_by(Analysis.Area) %>%
  mutate(logCPUE = if_else(is.na(logCPUE),
                           dplyr::lag(mean_logCPUE),
                           logCPUE)) %>%
  ungroup()

# Drop the mean_logCPUE column as it's no longer needed
D7_filled_2 <- D7_filled_2 %>%
  select(-mean_logCPUE)

ggplot(D7_filled_2) + aes(x=Season.Ref, y=logCPUE) + geom_point() + facet_wrap(~Analysis.Area)
############################################################################################################################


#############################################################################################################
#Model Selection after imputation

#D7_filled_2 is my dataframe
#import areas of .... stat areas
areas_raw <- read.csv("Data/Southeast_Shrimp_StatArea_SqMiles.csv")
names(areas_raw)
areas_D7 <- areas_raw %>% #from the messy dataset
  select(DISTRICT_CODE, STAT_AREA_NAME, STAT_AREA, Area_SqMiles) %>% #choose the columns I care about
  filter(DISTRICT_CODE == 107) #%>% #I only care about District 7 right now
#group_by(S)

#View(areas_D7)

U_ern_sound <- areas_D7 %>% filter(STAT_AREA == 10720) %>%
  summarise(area=sum(Area_SqMiles))
L_ern_sound <- areas_D7 %>% filter(STAT_AREA == 10710) %>%
  summarise(area=sum(Area_SqMiles))
Zim_st <- areas_D7 %>% filter(STAT_AREA == 10730|STAT_AREA == 10735) %>%
  summarise(area=sum(Area_SqMiles))
Brad_Can <- areas_D7 %>% filter(STAT_AREA == 10740|STAT_AREA == 10745) %>%
  summarise(area=sum(Area_SqMiles))

df_temp <- data.frame(Analysis.Area= c("Upper Ernest Sound", "Lower Ernest Sound", "Bradfield Canal", "Zimovia Strait"),
                      area_sqmi = c(U_ern_sound$area, L_ern_sound$area, Zim_st$area, Brad_Can$area))
#ugh that was tedious
#8/1/24 - update: area df now has stat areas as a column, I do not need to do this step for other districts

D7_filled_3 <- left_join(D7_filled_2, df_temp) #joined by analysis areas, now I have areas that I can use to weigh

#generate a model
M3_2 <- lm(logCPUE~ Season.Ref + ADFG.Number + Analysis.Area + Season.Ref:Analysis.Area, data=D7_filled_3) 
summary(M3_2)
M_alt1 <- lm(logCPUE~ Season.Ref + ADFG.Number + Analysis.Area, data=D7_filled_3)
M_alt2 <- lm(logCPUE~ Season.Ref + Analysis.Area + Season.Ref:Analysis.Area, data=D7_filled_3) 

AIC(M3_2, M_alt1, M_alt2) #M3_2 wins

#plot(M3_2)
################################################################################################################################

#mode boat - for the predict function below
Ernest_U <- D7 %>% filter(Analysis.Area == "Upper Ernest Sound")
Ernest_L <- D7_filled_3 %>% filter(Analysis.Area == "Lower Ernest Sound")
Bradfield <- D7_filled_3 %>% filter(Analysis.Area == "Bradfield Canal")
Zim <- D7_filled_3 %>% filter(Analysis.Area == "Zimovia Strait")

mode_boat <- which.max(table(D7$ADFG.Number))

mode_val <- names(mode_boat)

EL_mode_boat <- names(which.max(table(Ernest_L$ADFG.Number)))
brad_mode_boat <- names(which.max(table(Bradfield$ADFG.Number)))
zim_mode_boat<- names(which.max(table(Zim$ADFG.Number)))

###########################################################################################################################
#generating predictions/the standardized CPUE:


##predict function
D7_pred_2 <- function(DATA, ANALYSIS_AREA, MODE_VESSEL, MODEL){
  std_dat<- expand.grid(Season.Ref = unique(DATA$Season.Ref),
                        Analysis.Area = ANALYSIS_AREA, 
                        ADFG.Number = factor(MODE_VESSEL) #maybe pick mode vessel for this area. This is the mode vessel for D7 overall
  )
  pred_logcpue <- predict(MODEL, std_dat, type = "response", se = TRUE) 
  ln_mu = pred_logcpue$fit
  ln_sigma = pred_logcpue$se.fit
  
  std_dat %>% 
    mutate(
      ln_mu=ln_mu,
      mu = exp(ln_mu + (ln_sigma^2)/2),
      sigma = sqrt((exp(ln_sigma^2) - 1) * exp(2*ln_mu + ln_sigma^2)), #make sure is right
      upper= mu + 2*sigma,
      lower = mu - 2*sigma
    ) -> pred_dat_biascorrected
  
  return(pred_dat_biascorrected)
  
}

ernU_pred <- D7_pred_2(MODEL = M3_2, ANALYSIS_AREA = "Upper Ernest Sound", MODE_VESSEL = 52131, DATA=D7_filled_3)
ernL_pred <- D7_pred_2(MODEL = M3_2, ANALYSIS_AREA = "Lower Ernest Sound", MODE_VESSEL = EL_mode_boat, DATA=D7_filled_3)
brad_pred <- D7_pred_2(MODEL = M3_2, ANALYSIS_AREA = "Bradfield Canal", MODE_VESSEL = brad_mode_boat, DATA=D7_filled_3)
zim_pred <- D7_pred_2(MODEL = M3_2, ANALYSIS_AREA = "Zimovia Strait", MODE_VESSEL = zim_mode_boat, DATA=D7_filled_3)

D7_pred_comb <- rbind(ernU_pred, ernL_pred, brad_pred, zim_pred)

D7_pred_comb <- left_join(D7_pred_comb, df_temp)
############################################################################################################################################################

#######################################################################
#graphs of model predictions

ggplot(D7_pred_comb) + aes(x=Season.Ref, y=mu) + geom_errorbar(aes(ymin=lower, ymax=upper)) + geom_point() + geom_line(aes(group="Analysis.Area"))+
  facet_wrap(~Analysis.Area)
##################################################################33


#################################################################################
#Results graphs

#make teh function (move to functions page later)
make.plots <- function(DATA_PRED, AREA_NAME, DATA_RAW){ #data pred is the
  (E1<-ggplot(data=DATA_PRED) + aes(x=Season.Ref, y=mu) + 
     geom_point(size=3)+
     geom_errorbar( aes(ymin=lower, ymax=upper)) +
     geom_line(aes(group=1))+ #that works+
     #geom_hline(aes(yintercept=mean(std_dat_ranef_lim$bt_cpue)), linetype="dashed")+
     theme_cowplot()+
     labs(y="Standardized CPUE (lbs/pots)", x=NULL, title = AREA_NAME )+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))#+
   #  scale_y_continuous(breaks = c(1,2,3,4,5,6), expand=c(0,0))+
   # ylim(1.8, 5)
  )
  
  raw_totals <- DATA_RAW %>%
    group_by(Season.Ref, Analysis.Area) %>%
    dplyr::summarize(Harvest = sum(total_weight),
                     Effort = sum(max_pots_2))
  raw_totals %>% filter(Analysis.Area == AREA_NAME) %>%
    ggplot() + aes(x=Season.Ref) +
    geom_point(aes(y=Harvest), shape=0, size=3)+ #harvest graph
    geom_line(aes(y=Harvest, group=1))+
    geom_point(aes(y=Effort), shape=15, size=3) +
    geom_line(aes(y=Effort, group=1), linetype="dashed") +
    theme_cowplot()+
    labs(x="Season", y="Harvest (lbs) or Effort (pots)")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  E2 <- last_plot()
  
  (Plot <- E1/E2)
  return(Plot)
}

#upper ERnest
Upper_ernest_plot <- make.plots(DATA_PRED = ernU_pred, AREA_NAME = "Upper Ernest Sound", DATA_RAW=D7)
Lower_ernest_plot <- make.plots(DATA_PRED = ernL_pred, AREA_NAME = "Lower Ernest Sound", DATA_RAW=D7)
Brad_plot <- make.plots(DATA_PRED = brad_pred, AREA_NAME = "Bradfield Canal", DATA_RAW=D7)
Zim_plot <- make.plots(DATA_PRED = zim_pred, AREA_NAME = "Zimovia Strait", DATA_RAW=D7)

Upper_ernest_plot
Lower_ernest_plot
Brad_plot
Zim_plot

##ggsave these...

###################################################################################

#TAKE A LOOK AT BELOW ALEX!!
#####################################################################################
#D7 district-wide graphs
#Average of analysis area cpue's, weighted by area
#Weighted and unweighted graphs
#D7 df's
##combine values
##Looking at p 111 of campbell 2015
D7_results_area_weighted <- D7_pred_comb %>%
  group_by(Season.Ref) %>% #I want the sum by year
  summarise(CPUE_avg = weighted.mean(x=mu, w=area_sqmi),
            CPUE_sd = sqrt(weighted.mean(x=sigma^2, w=area_sqmi))
  )%>%
  mutate(upper = CPUE_avg + 2*CPUE_sd,
         lower = CPUE_avg - 2*CPUE_sd)
# area = max(area_sqmi)) %>% 
#mutate(index= CPUE_sum*area)


#6/4/24
##get mean of sd
D7_pred_comb$sigma #my standard deviations
var_D7<-D7_pred_comb$sigma^2 #my variances
sqrt(mean(var_D7))
mean(D7_pred_comb$sigma) 

#model:M3_2
##summary(M3_2)

D7_results_not_weighted <- D7_pred_comb %>%
  group_by(Season.Ref) %>% #I want the sum by year
  summarise(CPUE_avg = mean(mu), #average the cpue's
            #CPUE_sum = sum(mu), #I do not think I need this
            CPUE_sd = sqrt(mean(sigma^2))) %>%
  mutate(upper = CPUE_avg + 2*CPUE_sd,
         lower = CPUE_avg - 2*CPUE_sd)#get the sd's by taking the mean of the variances, then converting back to sd.


#total D7 plot
raw_totals_D7 <- D7 %>%
  group_by(Season.Ref) %>%
  dplyr::summarize(Harvest = sum(total_weight),
                   Effort = sum(max_pots_2))
raw_D7_plot <-raw_totals_D7 %>%# filter(Analysis.Area == AREA_NAME) %>%
  ggplot() + aes(x=Season.Ref) +
  geom_point(aes(y=Harvest), shape=0, size=3)+ #harvest graph
  geom_line(aes(y=Harvest, group=1))+
  geom_point(aes(y=Effort), shape=15, size=3) +
  geom_line(aes(y=Effort, group=1), linetype="dashed") +
  theme_cowplot()+
  labs(x="Season", y="Harvest (lbs) or Effort (pots)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#D7 graphs unweighted (soo... weighted (or biased, depending on your perspective) by catch)
#D7_unweighed <- make.plots(DATA_PRED = D7_pred_comb,AREA_NAME = NULL, DATA_RAW = D7 ) #no area name, will it work? #NOPE. graph by hand
D7_no_weight <- ggplot(D7_results_not_weighted)  + aes(x=Season.Ref, y=CPUE_avg) + 
  geom_point(size=3)+
  geom_errorbar( aes(ymin=lower, ymax=upper)) +
  geom_line(aes(group=1))+ #that works+
  #geom_hline(aes(yintercept=mean(std_dat_ranef_lim$bt_cpue)), linetype="dashed")+
  theme_cowplot()+
  labs(y="Standardized CPUE (lbs/pots)", x=NULL, title = "District 7" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # scale_y_continuous(breaks = c(1,2,3,4,5,6), expand=c(0,0))+
  ylim(0, 8)

D7_plot <- D7_no_weight/raw_D7_plot

#D7 graphs weighted by area
D7_yes_weight <- ggplot(D7_results_area_weighted)  + aes(x=Season.Ref, y=CPUE_avg) + 
  geom_point(size=3)+
  geom_errorbar( aes(ymin=lower, ymax=upper)) +
  geom_line(aes(group=1))+ #that works+
  #geom_hline(aes(yintercept=mean(std_dat_ranef_lim$bt_cpue)), linetype="dashed")+
  theme_cowplot()+
  labs(y="Standardized CPUE (lbs/pots)", x=NULL, title = "District 7" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # scale_y_continuous(breaks = c(1,2,3,4,5,6), expand=c(0,0))+
  ylim(0, 8)

D7_results_area_weighted
D7_results_not_weighted

D7_plot
(D7_plot_weight <- D7_yes_weight/raw_D7_plot)

