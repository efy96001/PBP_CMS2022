library(tidyverse)
library(readr)

pbp_b13_other_services <- read_delim("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_b13_other_services.txt", 
                                          delim = "\t", escape_double = FALSE, 
                                          trim_ws = TRUE)

p <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')

s <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')%>%
  select(pbp_a_hnumber,pbp_b13c_meal_type_chk,pbp_b13c_bendesc_service,
         pbp_b13c_meal_type_chk
         ,pbp_b13c_maxplan_amt
         ,pbp_b13c_maxplan_per)

s <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')
s1 <- as_data_frame(t(s))
write.table(s, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

pbp_b6_home_health <- read_delim("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_b6_home_health.txt", 
  delim = "\t", escape_double = FALSE, 
  trim_ws = TRUE)

hh <- pbp_b6_home_health %>% filter(pbp_a_hnumber=='H0169')
write.table(hh, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

#Preventative
pbp_b14_preventive <- read_delim("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_b14_preventive.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 trim_ws = TRUE)

prev <- pbp_b14_preventive %>%
  filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')

prevTest <- prev %>% select(pbp_b14c_rp_bendesc_ehc,
                            pbp_b14c_rp_bendesc_other,
                            pbp_b14c_rp_days,
                            pbp_b14c_rp_max_meals)

mealFields <- pbp_b14_preventive %>% select(pbp_a_hnumber,pbp_a_plan_identifier,
  pbp_b14c_rp_bendesc_ehc,
                                            pbp_b14c_rp_bendesc_other,
                                            pbp_b14c_rp_days,
                                            pbp_b14c_rp_max_meals)

pbp_Section_A <- read_delim("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_Section_A.txt", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

secAslice <- pbp_Section_A %>%
  filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')

