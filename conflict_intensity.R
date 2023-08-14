
####################################################################################################
### Democratic/Autocratic Peace Replication F/U - Deadliness of Wars by Dyad Type
####################################################################################################

### Set wd
setwd("C:/Users/David/Documents/Data Projects/Democratic Peace")

### Libraries
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(reshape2)
library(lubridate)
library(peacesciencer)
library(lfe)
library(dotwhisker)

###################################
### Get COW Directed Dyad Data
###################################

### Download Dat from http://svmiller.com/R/peacesciencer
download_extdata()

### Create Dataset
dat<-create_dyadyears(system = "cow", mry = F, directed = F) %>% # set of dyads
  add_contiguity() %>% # contiguity
  add_gml_mids() %>% # MIDs
  add_nmc() %>% # capability
  add_democracy() %>% # regime type
  add_cow_alliance() %>% # alliances
  add_sdp_gdp() %>% # GDP
  add_cow_majors() %>% # major power status
  add_capital_distance() %>% # distance
  add_strategic_rivalries() %>% # rivalries
  add_cow_trade() # trade


### Clean and Add additional variables
dat_clean <- dat %>%
  mutate(
    dyad_id=paste(ccode1,ccode2,sep=""),
    
    polity_democracy1=ifelse(polity21>=6,1,0),
    polity_democracy2=ifelse(polity22>=6,1,0),
    polity_autocracy1=ifelse(polity21<=-6,1,0),
    polity_autocracy2=ifelse(polity22<=-6,1,0),
    polity_anocracy1= ifelse(polity21 %in% c(-5:5),1,0),
    polity_anocracy2= ifelse(polity22 %in% c(-5:5),1,0),
    
    joint_democracy=ifelse(polity_democracy1==1 & polity_democracy2==1,1,0),
    joint_autocracy = ifelse(polity_autocracy1==1 & polity_autocracy2==1,1,0),
    joint_anocracy = ifelse(polity_anocracy1==1 & polity_anocracy2==1,1,0),
    
    cinc_share1=cinc1/(cinc1+cinc2),
    cinc_avg=(cinc1+cinc2)/2,
    
    cow_alliance=ifelse(cow_defense+cow_neutral+cow_nonagg+cow_entente > 0,1,0),
    
    bilateral_trade=(flow1+flow2)/(wbgdp2011est1+wbgdp2011est2),
    
    gdp_pc1=wbgdp2011est1/wbpopest1,
    gdp_pc2=wbgdp2011est2/wbpopest2,
    gdp_pc_avg=(gdp_pc1+gdp_pc2)/2,
    gdp_pc_share1=gdp_pc1/(gdp_pc1+gdp_pc2),
    
    init1 = ifelse(!is.na(init1),init1,0),
    gmlmidongoing = ifelse(!is.na(gmlmidongoing),gmlmidongoing,0),
    gmlmidonset = ifelse(!is.na(gmlmidonset),gmlmidonset,0)
  ) %>%
  add_spells()

### Filter to just onset year
dat_onset = dat_clean %>%
  filter(gmlmidonset == 1)

###################################
### Baseline Descriptive Analysis
###################################

### Avg Hostility Level
dat_onset %>%
  mutate(dyad_type = case_when(
    joint_democracy == 1 ~ "Democratic Dyad",
    joint_autocracy == 1 ~ "Autocratic Dyad",
    joint_anocracy == 1 ~ "Anocratic Dyad",
    T ~ "Mixed Dyad"
  )) %>%
  group_by(dyad_type) %>%
  summarise(
    count = n(),
    avg_hostility_level = mean(hostlev, na.rm = T)
  ) %>%
  mutate(avg_hostility_level = round(avg_hostility_level,4)) %>%
  ggplot(aes(dyad_type,avg_hostility_level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = avg_hostility_level),color = "white",vjust = 1.5,size = 5) +
  ggtitle("Average Hostility Level by Dyad Type, 1800-2016") +
  xlab("") + ylab("Hostility Level") +
  labs(caption = "Hostility Level ranges from 2 to 5, from threat to use force to war.")

### Fatality Level
dat_onset %>%
  filter(fatalpre1 != -9,
         fatalpre2 != -9) %>%
  mutate(dyad_type = case_when(
    joint_democracy == 1 ~ "Democratic Dyad",
    joint_autocracy == 1 ~ "Autocratic Dyad",
    joint_anocracy == 1 ~ "Anocratic Dyad",
    T ~ "Mixed Dyad"),
    total_fatalities = fatalpre1 + fatalpre2
  ) %>%
  group_by(dyad_type) %>%
  summarise(
    count = n(),
    avg_total_fatalities = mean(total_fatalities, na.rm = T),
    median_total_fatalities = median(total_fatalities, na.rm = T)
  ) %>%
  mutate(avg_total_fatalities = round(avg_total_fatalities,1)) %>%
  ggplot(aes(dyad_type,avg_total_fatalities)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = avg_total_fatalities),color = "white",vjust = 1.5,size = 5) +
  ggtitle("Average Number of Fatalities per Conflict by Dyad Type, 1800-2016") +
  xlab("") + ylab("Fatalities") 

### Fatality Level among just fatal MIDs
dat_onset %>%
  filter(fatalpre1 != -9,
         fatalpre2 != -9) %>%
  mutate(dyad_type = case_when(
    joint_democracy == 1 ~ "Democratic Dyad",
    joint_autocracy == 1 ~ "Autocratic Dyad",
    joint_anocracy == 1 ~ "Anocratic Dyad",
    T ~ "Mixed Dyad"),
    total_fatalities = fatalpre1 + fatalpre2
  ) %>%
  filter(total_fatalities > 0) %>%
  group_by(dyad_type) %>%
  summarise(
    count = n(),
    avg_total_fatalities = mean(total_fatalities, na.rm = T),
    median_total_fatalities = median(total_fatalities, na.rm = T)
  ) %>%
  mutate(avg_total_fatalities = round(avg_total_fatalities,1)) %>%
  ggplot(aes(dyad_type,avg_total_fatalities)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = avg_total_fatalities),color = "black",vjust = -.7,size = 3.5) +
  ggtitle("Average Number of Fatalities per Conflict by Dyad Type, 1800-2016") +
  xlab("") + ylab("Fatalities") +
  labs(caption = "Only MIDs with fatalities are included.") +
  theme(plot.title = element_text(size = 12))
ggsave("conflict_intensity_barplot.png")

###################################
### Regression Analysis
###################################

##############
### Set up data set
##############

dat_onset_final <- dat_onset %>%
  filter(fatalpre1 != -9,
         fatalpre2 != -9) %>%
  mutate(dyad_type = case_when(
    joint_democracy == 1 ~ "Democratic Dyad",
    joint_autocracy == 1 ~ "Autocratic Dyad",
    joint_anocracy == 1 ~ "Anocratic Dyad",
    T ~ "Mixed Dyad"),
    total_fatalities = fatalpre1 + fatalpre2
  ) %>%
  filter(total_fatalities > 0) 

##############
### Regressions
##############

### Baseline
m1 <- felm(total_fatalities ~ 
             joint_democracy + joint_autocracy + joint_anocracy
           | 0 | 0 | dyad_id, dat_onset_final)
summary(m1)

### With Controls
m2 <- felm(total_fatalities ~ 
             joint_democracy + joint_autocracy + joint_anocracy +
             I(conttype == 1) + capdist +
             cinc_share1 + cinc_avg + 
             cowmaj1 + cowmaj2 +
             ongoingrivalry + cow_alliance +
             gdp_pc_avg + gdp_pc_share1 +
             bilateral_trade
           | 0 | 0 | dyad_id, dat_onset_final)
summary(m2)

###################################
### Dot and Whisker Plot
###################################

dwplot(list(m1,m2),
       vars_order = c("joint_democracy","joint_autocracy","joint_anocracy"),
       model_order = c("Model 1","Model 2"),
) %>%
  relabel_predictors(
    c(
      joint_democracy = "Joint Democracy",
      joint_autocracy = "Joint Autocracy",
      joint_anocracy = "Joint Anocracy"
    )
  ) +
  xlab("Coefficient Estimate") + ylab("") +
  geom_vline(
    xintercept = 0,
    linetype = 2
  ) +
  ggtitle("Same Regime Type on Conflict Intensity to Mixed Dyads") +
  labs(color = "Model") +
  scale_color_discrete(labels = c("Controls","Baseline")) +
  theme(title = element_text(8),
        legend.position="bottom")
ggsave("coefficient_plot_conflict_intensity.pdf")


###################################
### Save Data
###################################

write.csv(dat_onset_final,"conflict_intensity_data.csv",row.names = T)
