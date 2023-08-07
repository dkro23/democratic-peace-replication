
####################################################################################################
### Democratic/Autocratic Peace Replication
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
library(extrafont)
loadfonts(device = "win")


###################################
### Get COW Directed Dyad Data
###################################

### Download Dat from http://svmiller.com/R/peacesciencer
download_extdata()

### Create Dataset
dat<-create_dyadyears(system = "cow", mry = F, directed = T) %>% # set of directed dyads
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
    polity_anocracy1= ifelse(polity21 %in% c(-6:6),1,0),
    polity_anocracy2= ifelse(polity22 %in% c(-6:6),1,0),
    
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
  mutate(init45=ifelse(init1*gmlmidonset > 0 & hostlev %in% c(4:5),1,0), # Use of Force and War

         init5=ifelse(init1*gmlmidonset > 0 & hostlev %in% c(5),1,0), # Just war

         init345=ifelse(init1*gmlmidonset > 0 & hostlev %in% c(3:5),1,0), # Display or Use of Force and War

         init2345=ifelse(init1*gmlmidonset > 0 & hostlev %in% c(2:5),1,0), # Threat, Display or Use of Force and War

         init123=ifelse(init1*gmlmidonset > 0 & hostlev %in% c(1:3),1,0), # No militarized action or threat or display

         init12=ifelse(init1*gmlmidonset > 0 & hostlev %in% c(1:2),1,0), # No militarized action or threat 

         init_fatal=ifelse(init1*gmlmidonset > 0 & fatality > 0,1,0), # At least 1 fatality 

         init_very_fatal=ifelse(init1*gmlmidonset > 0 & fatality > 3,1,0), # At least 250 fatality 
  ) %>%
  add_spells()

###################################
### Baseline Descriptive Analysis
###################################

dat_clean %>%
  mutate(dyad_type = case_when(
    joint_democracy == 1 ~ "Democratic Dyad",
    joint_autocracy == 1 ~ "Autocratic Dyad",
    joint_anocracy == 1 ~ "Anocratic Dyad",
    T ~ "Mixed Dyad"
  )) %>%
  group_by(dyad_type) %>%
  summarise(
    count = n(),
    avg_init1 = mean(init1, na.rm = T)
  ) %>%
  mutate(avg_init1 = round(avg_init1,4)) %>%
  ggplot(aes(dyad_type,avg_init1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = avg_init1),color = "white",vjust = 1.5,size = 5) +
  ggtitle("MID Initiation Rate, 1800-2016") +
  xlab("") + ylab("Rate (%)")
  



###################################
### Regression Analysis
###################################

### Baseline
m1 <- felm(gmlmidonset ~ 
             joint_democracy + joint_autocracy + joint_anocracy
             | ccode1 + ccode2 + year | 0 | dyad_id, dat_clean)
summary(m1)

### With Controls
m2 <- felm(gmlmidonset ~ 
             joint_democracy + joint_autocracy + joint_anocracy +
             I(conttype == 1) + capdist +
             poly(gmlmidspell,3) +
             cinc_share1 + cinc_avg + 
             cowmaj1 + cowmaj2 +
             ongoingrivalry + cow_alliance +
             gdp_pc_avg + gdp_pc_share1 +
             bilateral_trade
           | ccode1 + ccode2 + year | 0 | dyad_id, dat_clean)
summary(m2)

### Just Display or Use of Force and War
m3 <- felm(init345 ~ 
             joint_democracy + joint_autocracy + joint_anocracy +
             I(conttype == 1) + capdist +
             poly(gmlmidspell,3) +
             cinc_share1 + cinc_avg + 
             cowmaj1 + cowmaj2 +
             ongoingrivalry + cow_alliance +
             gdp_pc_avg + gdp_pc_share1 +
             bilateral_trade
           | ccode1 + ccode2 + year | 0 | dyad_id, dat_clean)


###################################
### Dot and Whisker Plot
###################################

dwplot(list(m1,m2,m3),
       vars_order = c("joint_democracy","joint_autocracy","joint_anocracy"),
       model_order = c("Model 1","Model 2","Model 3"),
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
  ggtitle("Same Regime Type on Conflict Relative to Mixed Dyads") +
  labs(color = "Model") +
  scale_color_discrete(labels = c("Display/Use of Force and War","Controls","Baseline")) +
  theme(title = element_text(8),
        legend.position="bottom")
ggsave("coefficient_plot.pdf")


###################################
### Save Data
###################################

write.csv(dat_clean,"democratic_peace_data.csv",row.names = T)


