library(tidyverse)
library(readr)

pbp_b13_other_services <- read_delim("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_b13_other_services.txt", 
                                          delim = "\t", escape_double = FALSE, 
                                          trim_ws = TRUE)

p <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')

##Meal Benefits
s <- pbp_b13_other_services %>% #filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')%>%
  select(pbp_a_hnumber,pbp_a_plan_identifier,segment_id,pbp_b13c_meal_type_chk,pbp_b13c_bendesc_service,
         pbp_b13c_meal_type_chk
         ,pbp_b13c_maxplan_amt,pbp_b13c_maxplan_yn,
         pbp_b13c_maxplan_per)%>%
  mutate(CPBP=paste0(pbp_a_hnumber,"_",pbp_a_plan_identifier,"_",segment_id),
         Meals=if_else(pbp_b13c_bendesc_service==1,"Yes","No"),
         Type=case_when(
           pbp_b13c_meal_type_chk=='001'~"Chronic Illness",#93
           pbp_b13c_meal_type_chk=='010'~"Post Discharge",#2,032
           pbp_b13c_meal_type_chk=='011'~"PD and Chronic",#574
           pbp_b13c_meal_type_chk=='100'~"Medical Condition requiring home stay",#76
           pbp_b13c_meal_type_chk=='101'~"Medical Condition or Chronic",#13
           pbp_b13c_meal_type_chk=='110'~"Medical Condition or PD",#72
           pbp_b13c_meal_type_chk=='111'~"Medical Condition_PD_CC"#177
         ))

countSUM <- s %>%
  group_by(CPBP)%>%
  summarise(N=n())%>%
  arrange(-N)

mealSUM <- s %>%
  group_by(Meals)%>%
  summarize(N=n())

jj <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')%>%
  select(pbp_a_hnumber,pbp_b13c_meal_type_chk,
        #pbp_b13c_days,
        pbp_b13c_max_meals)
jjTransform <- as_data_frame(t(jj))
write.table(jjTransform, "clipboard", sep="\t", row.names=T, col.names=TRUE)

s2 <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')
s3 <- as_data_frame(t(s2))
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
                            delim = "\t", escape_double = FALSE, col_types = cols(pbp_a_last_data_entry_date = col_date(format = "%m/%d/%Y"), pbp_a_plan_identifier = col_character()), 
                            trim_ws = TRUE)%>%  select(pbp_a_hnumber
                                                       ,pbp_a_plan_identifier...2
                                                       ,pbp_a_contract_number
                                                       ,pbp_a_segment_id
                                                       ,pbp_a_contract_period
                                                       ,pbp_a_plan_geog_name
                                                       ,pbp_a_segment_name
                                                       ,segment_id
                                                       ,pbp_a_plan_name
                                                       ,pbp_a_plan_type...5
                                                       ,pbp_a_special_need_flag
                                                       ,pbp_a_special_need_plan_type
                                                       ,pbp_a_dsnp_zerodollar
                                                       ,pbp_a_snp_cond
                                                       ,pbp_a_snp_state_cvg_yn)

pbp_Section_A.clean <- pbp_Section_A %>%
  mutate(TYPE=case_when(
    pbp_a_plan_type...5 %in% c("01","02","03","04","05","07","08","09",
                               "31","32","42","43","44","45","48","49")~"MA",#93
    pbp_a_plan_type...5=='29'~"Drug Plan",
    pbp_a_plan_type...5 %in% c('18','19')~"Cost Plans",
    pbp_a_plan_type...5=='20'~"National Pace",
    pbp_a_plan_type...5=='30'~"Employer_Union Direct Contact PDP",
    pbp_a_plan_type...5=='40'~"Employer_Union Direct Contact FFS",
    pbp_a_plan_type...5=='47'~"Employer Direct PPO"))


secAslice <- pbp_Section_A %>%
  filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')

