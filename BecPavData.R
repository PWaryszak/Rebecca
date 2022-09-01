#LOAD  Data:=========
library(tidyverse)
library(gridExtra)
library(readxl)

library(lme4)
library(lmerTest)
library(nlme)
library(sjPlot)
library(sjmisc)

bb <- read_xlsx( "RStL_SwanBay_restoration.xlsx", sheet = 5)
names(bb)


#Compute carbon stock and accretion rate============
#Correct Soil data for compaction and #Compute corrected C-stock (Off Bulk Density):
NewDATA <- bb
NewDATA$C.percent <- ifelse(NewDATA$C.percent == 0, 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models if any


NewDATA$SliceLength.cm <- (NewDATA$Sample_upper_depth - NewDATA$Sample_lower_depth) #cm

NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm

NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume

NewDATA$Core_in.cm <- NewDATA$Core_Length  - NewDATA$Core_In_Measurement_cm #Compaction in cm, Core Length is Pipe Length
NewDATA$Pipe_in.cm <- NewDATA$Core_Length  - NewDATA$Core_Out_Measurement_cm
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.cm / NewDATA$Pipe_in.cm

NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Compaction_Correction_Value
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C.percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )



#SOIL C-stock (Depth_to_rehab_cm):========
#Compute Carbon Accretion Rate at Rehabilitated sites (per each site):

NewDATA2 <- NewDATA %>%
  
  filter(Treatment != "Natural") %>% # Natural sites come from Pawels Pn210 age-dating. Join later
  
  mutate(KeepThrow = ifelse(Depth_to_rehab_cm >= Sample_upper_depth, "keep", "throw")) %>% #keep slices data are > Sample_upper_depth
  
  filter(KeepThrow=="keep") %>% #keep the "keep"
  
  transform (DepthAtRehab_cm = ifelse(Sample_lower_depth <= Depth_to_rehab_cm,  #Cut to the length of Depth_to_rehab_cm
                                      Sample_lower_depth, Depth_to_rehab_cm)) %>%
  
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Sample_upper_depth) %>% #lenght of slice at cores up to Depth_to_rehab_cm
  
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) %>%  #Soil C stock in cores till Depth_to_rehab_cm

  mutate (Stock = "Soil") %>% #Assigning additional category, indicating it is soil stock
  
  group_by(Site_Core, Treatment,Rehab_age) %>% #grouping by core till 100 cm
  
  summarise(TotalCarbonStockPerCore_TillRehab = sum(CarbonStockTillRehab_Mgha)) %>%  #Add-up all slices per core at each rehab site
  
  mutate(CAR = TotalCarbonStockPerCore_TillRehab / Rehab_age) %>%
  
  select(-TotalCarbonStockPerCore_TillRehab) %>%  #Remove to match PAwels dataset on CAR
 
  select(- Rehab_age) #Remove to match PAwels dataset on CAR

NewDATA2


#Get Natural and Grazed Data=======
#Get Age Data from Natural sites from Pawel:
nat <- read.csv("Pawels_Natural_CAR.csv")

#Get Made-up Grazed Data from Pawel (as per convo with Paul):
grazed   <- read.csv("Grazed.csv") #Add grazed

#Join all data:
Full_CAR_Data <- rbind(grazed, nat, NewDATA2 ) %>%
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_",remove = F) 

Full_CAR_Data



#LMER=========
#Turn Grazed into intercept in the model:
unique(Full_CAR_Data$Treatment)#"Grazed"    "Natural"   "20+year"   "10-15year"
Full_CAR_Data$Treatment <- as.factor(as.character(Full_CAR_Data$Treatment))
Full_CAR_Data$Treatment <- factor(Full_CAR_Data$Treatment, levels = c("Grazed","10-15year", "20+year","Natural"))

CAR_MODEL <- lmer(CAR ~ Treatment + (1|Site), data = Full_CAR_Data)
summary(CAR_MODEL)
tab_model(CAR_MODEL)


#PLOT CAR per TREATMENT:==========
#Soil till rehab 
#Compute mean+_ SE per treatment:
soil_till_rehab <- Full_CAR_Data %>%
  
  group_by(Treatment) %>% #grouping by core till 100 cm
  summarise(AV = round(mean(CAR, na.rm = T),1),
            SD = round(sd(CAR, na.rm = T),1),
            N = length(CAR),
            SE = round(SD / sqrt(N),1)) 

soil_till_rehab 


ggplot(soil_till_rehab, aes(x= Treatment, y= AV, color=Treatment )) +
  geom_point(size = 6) +
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.6,size=1.2)+
  scale_y_continuous(limits = c(0, 1))+
  scale_color_manual(values = c("brown", "lightblue", "darkblue","darkgreen"))+
  labs(x= "", shape = "Site Type: ",
       y = bquote('Soil Carbon Accretion Rate  ' (Mg*~ha^-1*~year^-1)))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))



