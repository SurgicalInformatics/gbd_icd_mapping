library(tidyverse)

icd10_lookup = read_csv("icd10_lookup.csv")

cause_hierarchy = read_csv("data/IHME_GBD_2016_CAUSE_HIERARCHY_Y2017M10D02.csv")

level2 = read_csv("gbd_icd10_lookup_level2_long.csv") %>% 
  rename(cause_id.level2 = cause_id)

level3 = read_csv("gbd_icd10_lookup_level3_long.csv") %>% 
  rename(cause_id.level3 = cause_id)


# check if everything in level 3 was also in level 2
check_3in2 = level3 %>% 
  left_join(level2)

in_level3_not_in_level2 = check_3in2 %>% 
  filter(is.na(cause_id.level2)) %>% 
  left_join(icd10_lookup)

missing2.distinct = in_level3_not_in_level2 %>% 
  distinct(icd10_name, .keep_all = TRUE)

dups = in_level3_not_in_level2$icd10_name[duplicated(in_level3_not_in_level2$icd10_name)]

missing2.duplicates = in_level3_not_in_level2 %>% 
  filter(icd10_name %in% dups)


# check if everything in level 2 shows up in level 3
check_2in3 = level2 %>% 
  left_join(level3) %>% 
  left_join(select(cause_hierarchy, cause_id, cause_name), by= c("cause_id.level2" = "cause_id")) %>% 
  arrange(icd10) %>% 
  select(cause_id.level2, cause_name, icd10, cause_id.level3)

in_level2_not_in_level3 = check_2in3 %>% 
  filter(is.na(cause_id.level3))

# 111/12067 = 1% of the causes do not


level3_icd10 = read_csv("gbd_icd10_lookup_level3_for_checking.csv")

potential_children = cause_hierarchy %>% 
  filter(parent_id %in% in_level2_not_in_level3$cause_id.level2) %>% 
  select(cause_id.level2 = parent_id, cause_id.level3 = cause_id, cause_name.level3 = cause_name) %>% 
  left_join(level3_icd10, by = c("cause_id.level3" = "cause_id")) %>% 
  select(-cause_name)

write_csv(in_level2_not_in_level3, "in_level2_but_missing_in_level3.csv", na = "")
write_csv(potential_children, "potentially_missing_children.csv", na = "")




