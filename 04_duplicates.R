library(tidyverse)

icd10_lookup = read_csv("icd10_lookup.csv")

cause_hierarchy = read_csv("data/IHME_GBD_2016_CAUSE_HIERARCHY_Y2017M10D02.csv")

level2 = read_csv("gbd_icd10_lookup_level2_long.csv") %>% 
  rename(cause_id.level2 = cause_id) %>% 
  left_join(icd10_lookup)


level3 = read_csv("gbd_icd10_lookup_level3_long.csv") %>% 
  rename(cause_id.level3 = cause_id) %>% 
  left_join(icd10_lookup)

dups = level2$icd10_name[duplicated(level2$icd10_name)]

level2.duplicates = level2 %>% 
  filter(icd10_name %in% dups)

dups = level3$icd10_name[duplicated(level3$icd10_name)]

level3.duplicates = level3 %>% 
  filter(icd10_name %in% dups) %>% 
  left_join(select(cause_hierarchy, cause_id, cause_name), by = c("cause_id.level3" = "cause_id")) %>% 
  mutate(dummy = 1) %>% 
  group_by(icd10) %>% 
  mutate(group_n = cumsum(dummy)) %>% 
  mutate(group = paste0("group", group_n)) %>% 
  ungroup() %>% 
  mutate(gbd = paste0(cause_name, " (", cause_id.level3, ")")) %>% 
  select(disease = icd10_name, icd10, group, gbd) %>% 
  spread(group, gbd) %>% 
  arrange(icd10)


take_other_mental = level3.duplicates %>% 
  filter(group2 == "Other mental and substance use disorders (585)")

take_other_mental %>% 
  write_csv("duplicated_other_mental.csv")

not_mental =  level3.duplicates %>% 
  filter(group2 != "Other mental and substance use disorders (585)")

not_mental %>% 
  write_csv("duplicated_not_mental.csv")
