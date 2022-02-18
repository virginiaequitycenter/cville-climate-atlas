library(tidyverse)

## Reading in data, renaming variables, and merging together
edat <- read.csv('cville_region_collection/data/lodes_employment_cville_tracts.csv')
edat <- edat %>%
  rename(GEOID = w_tract)

popdat <- read.csv("cville_region_collection/data/acs_tract_cville.csv")

healthdat <- read.csv("cville_region_collection/data/cdcplaces_cville_tract.csv")
healthdat <- healthdat %>%
  rename(GEOID = locationname)

dat <- popdat %>%
  left_join(edat) %>%
  left_join(healthdat)

str(dat)

## Creating tract ranks based on tercile groupings based on socio-economic variables 
### The higher the rank, the higher the average--i.e. povrank of 1 corresponds to lower % people living in poverty
dat$povrank <- as.character(ntile(dat$povrateE, 3))
aggregate(dat$povrateE ~ dat$povrank, FUN = mean)
table(dat$povrank)

# ...........................
# mpc added
dat <- dat %>% 
  mutate(povrank2 = factor(povrank, labels = c("low", "mid", "high")))
# ...........................

dat$lowwage_jobsrank <- as.character(ntile(dat$lowwage_p, 3)) # high % low-wage jobs = higher rank
dat$hhincrank <- as.character(ntile(dat$hhincE, 3)) # lower household income = lower rank
dat$insurancerank <- as.character(ntile(dat$hlthinsE, 3)) # lower % with health insurance = lower rank


## Example plots 
mentalhealthplot <- dat %>%  
  dplyr::select(Mental_Health2018, povrank) %>%
  filter(!is.na(Mental_Health2018), !is.na(povrank)) %>% #get rid of those tracts with missing data -- will need to think about
  # whether we want to do this every time. For poverty rank, it gets rid of the UVA tract.  
  gather(Variable, Mental_Health2018, -povrank) %>%
  ggplot(aes(povrank, Mental_Health2018, fill=povrank)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean", na.rm = T) + 
  scale_fill_manual(labels = c("low-poverty", "mid-poverty", "high-poverty"),
                    values = c('coral2', 'cornflowerblue', 'plum4')) + 
  labs(x = "Poverty rank", y = "% of population with poor mental health") # I'm assuming we can use the paste function 
# or something here to generate labels based on whatever variables are selected in the shiny app but 
# I'm not exactly sure how you do that 

# ...........................
# mpc added
dat2 <- dat %>%  
  dplyr::select(totalpopE, Mental_Health2018, povrank2) %>%
  filter(!is.na(Mental_Health2018), !is.na(povrank2)) %>% 
  mutate(mh_num = totalpopE*(Mental_Health2018/100)) %>% 
  group_by(povrank2) %>% 
  summarize(mh_num = sum(mh_num),
            num = sum(totalpopE),
            mh_per = round((mh_num/num)*100,1))

mentalhealthplot2 <- dat2 %>% 
  ggplot(aes(povrank2, mh_per, fill=povrank2)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label = paste0(mh_per, "%")), nudge_y = -.4, color = c("white", "white", "black")) +
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Poverty rank", y = "% of population with poor mental health") # I'm assuming we can use the paste function 
# ...........................



## The following plots are just additional examples that I think are interesting 

smokingplot <- dat %>%  
  dplyr::select(Current_Smoking2018, lowwage_jobsrank) %>%
  filter(!is.na(Current_Smoking2018)) %>% 
  gather(Variable, Current_Smoking2018, -lowwage_jobsrank) %>%
  ggplot(aes(lowwage_jobsrank, Current_Smoking2018, fill=lowwage_jobsrank)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  scale_fill_manual(values = c('coral2', 'cornflowerblue', 'plum4')) + 
  labs(x = "% of jobs that are low-wage rank", y = "% of population who smoke") 

asthmaplot <- dat %>%  
  dplyr::select(Current_Asthma2018, hhincrank) %>%
  filter(!is.na(Current_Asthma2018), !is.na(hhincrank)) %>% 
  gather(Variable, Current_Asthma2018, -hhincrank) %>%
  ggplot(aes(hhincrank, Current_Asthma2018, fill=hhincrank)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  scale_fill_manual(values = c('coral2', 'cornflowerblue', 'plum4')) + 
  labs(x = "household income rank", y = "% of population with asthma")

diabetesplot <- dat %>%  
  dplyr::select(Diabetes2018, insurancerank) %>%
  filter(!is.na(Diabetes2018), !is.na(insurancerank)) %>% 
  gather(Variable, Diabetes2018, -insurancerank) %>%
  ggplot(aes(insurancerank, Diabetes2018, fill=insurancerank)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  scale_fill_manual(values = c('coral2', 'cornflowerblue', 'plum4')) + 
  labs(x = "insurance rank", y = "% of population with diabetes")


