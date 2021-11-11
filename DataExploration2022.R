library(tidyverse)
library(readr)

pbp_b13_other_services <- read_delim("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_b13_other_services.txt", 
                                          delim = "\t", escape_double = FALSE, 
                                          trim_ws = TRUE)

p <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')


pbp_b13ALLCHARACTER <- data.frame(data.table::fread("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_b13_other_services.txt", 
                          colClasses = 'character', stringsAsFactors = FALSE))

allcharacter <- pbp_b13ALLCHARACTER %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')

# %>%
#   select(pbp_a_hnumber,pbp_a_plan_identifier,segment_id,pbp_b13c_meal_type_chk,pbp_b13c_bendesc_service,
#          pbp_b13c_meal_type_chk
#          ,pbp_b13c_maxplan_amt,pbp_b13c_maxplan_yn,
#          pbp_b13c_maxplan_per)

allCharT <- as_data_frame(t(allcharacter))
write.table(allCharT, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)


##Meal Benefits 
s <- pbp_b13_other_services %>% filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')%>%
  select(pbp_a_hnumber,pbp_a_plan_identifier,segment_id,pbp_b13c_meal_type_chk,
         pbp_b13c_bendesc_service,pbp_b13c_bendesc_amo,
         pbp_b13c_meal_type_chk
         ,pbp_b13c_maxplan_amt,pbp_b13c_maxplan_yn,
         pbp_b13c_maxplan_per)%>%
  mutate(CPBP=paste0(pbp_a_hnumber,"_",pbp_a_plan_identifier,"_",segment_id),
         Meals=if_else(pbp_b13c_bendesc_service==1,"Yes","No"),
         Option=if_else(pbp_b13c_bendesc_amo==2,"Mandatory","Optional"),
         Type=case_when(
           pbp_b13c_meal_type_chk=='001'~"Chronic Illness",#93
           pbp_b13c_meal_type_chk=='010'~"Post Discharge",#2,032
           pbp_b13c_meal_type_chk=='011'~"PD and Chronic",#574
           pbp_b13c_meal_type_chk=='100'~"Medical Condition requiring home stay",#76
           pbp_b13c_meal_type_chk=='101'~"Medical Condition or Chronic",#13
           pbp_b13c_meal_type_chk=='110'~"Medical Condition or PD",#72
           pbp_b13c_meal_type_chk=='111'~"Medical Condition_PD_CC"#177
         ))

sWIthMeals <- s %>% filter(Meals=='Yes')
table(sWIthMeals$Type)


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
                                                       ,pbp_a_snp_cond,
                                                       pbp_a_snp_institutional_type,pbp_a_est_memb

                                                       ,pbp_a_snp_state_cvg_yn)

pbp_Section_A.clean <- pbp_Section_A %>%
  mutate(SNP=if_else(pbp_a_special_need_flag==1,"Yes","No"),
         SNPType=case_when(pbp_a_special_need_plan_type==1~"Institutional",
                           pbp_a_special_need_plan_type==3~"Dual-Eligible",
                           pbp_a_special_need_plan_type==4~"Chronic or Disabling Condition"),
         TYPE=case_when(
    pbp_a_plan_type...5 %in% c("01","02","03","04","05","07","08","09",
                               "31","32","42","43","44","45","48","49")~"MA",#93
    pbp_a_plan_type...5=='29'~"Drug Plan",
    pbp_a_plan_type...5 %in% c('18','19')~"Cost Plans",
    pbp_a_plan_type...5=='20'~"National Pace",
    pbp_a_plan_type...5=='30'~"Employer_Union Direct Contact PDP",
    pbp_a_plan_type...5=='40'~"Employer_Union Direct Contact FFS",
    pbp_a_plan_type...5=='47'~"Employer Direct PPO"),#20 DIGITS EACH
    ChronicConditions=case_when(pbp_a_snp_cond=="00000000000000000001"~"Cardiovascular Disorders and/or Stroke", #20
                                pbp_a_snp_cond=="00000000000000000010"~"Cardiovascular Disorders, Chronic Heart Failure, and/or Diabetes", #19
                                pbp_a_snp_cond=="00000000000000000100"~"Chronic Heart Failure and/or Diabetes",#18
                                pbp_a_snp_cond=="00000000000000001000"~"Cardiovascular Disorders and/or Diabetes",#17
                                pbp_a_snp_cond=="00000000000000010000"~"Cardiovascular Disorders and/or Chronic Heart Failure",#16
                                pbp_a_snp_cond=="00000000000000100000"~"Stroke",#15
                                pbp_a_snp_cond=="00000000000001000000"~"Neurologic disorders",#14
                                pbp_a_snp_cond=="00000000000010000000"~"Chronic and disabling mental health conditions",#13
                                pbp_a_snp_cond=="00000000000100000000"~"Chronic lung disorders",#12
                                pbp_a_snp_cond=="00000000001000000000"~"HIV/AIDS",#11
                                pbp_a_snp_cond=="00000000010000000000"~"Severe hematologic disorders",#10
                                pbp_a_snp_cond=="00000000100000000000"~"Dialysis Services requiring dialysis (any mode of dialysis)",#9
                                pbp_a_snp_cond=="00000001000000000000"~"End-stage liver disease",#8
                                pbp_a_snp_cond=="00000010000000000000"~"Diabetes mellitus",#7
                                pbp_a_snp_cond=="00000100000000000000"~"Dementia",#6
                                pbp_a_snp_cond=="00001000000000000000"~"Chronic heart failure",#5
                                pbp_a_snp_cond=="00010000000000000000"~"Cardiovascular disorders",#4
                                pbp_a_snp_cond=="00100000000000000000"~"Cancer excluding pre-cancer conditions or in-situ status",#3
                                pbp_a_snp_cond=="01000000000000000000"~"Autoimmune disorders",#2
                                pbp_a_snp_cond=="100000000000000000000"~"Chronic alcohol and other drug dependence",#1,
                                pbp_a_snp_cond=="00011010000000000000"~"Cardiovascular_Chronic Heart Failure_Diabetes",))%>%
  select(pbp_a_hnumber,pbp_a_plan_identifier...2,pbp_a_contract_number,pbp_a_segment_id,
         pbp_a_plan_geog_name,pbp_a_plan_name,pbp_a_est_memb,
         SNP,SNPType,TYPE,ChronicConditions)

Insti <- pbp_Section_A.clean %>% #TARGET FOR PEOPLE LIVING IN INSTITUTIONS 
  filter(pbp_a_snp_institutional_type==2)


secAslice <- pbp_Section_A %>%
  filter(pbp_a_hnumber=='H0169'|pbp_a_hnumber=='R1548')


#Section C
pbp_Section_C <- read_delim("C:/Users/eyankowski/OneDrive - momsmeals.com/MM/CMS/PBPData/2022/PBP_Benefits_2022/pbp_Section_C.txt", 
          delim = "\t", escape_double = FALSE, 
          trim_ws = TRUE)

OONTest <- pbp_Section_C %>% select(pbp_c_oon_nmc_bendesc_cats)
