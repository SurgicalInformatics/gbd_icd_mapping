library(tidyverse)

icd10_lookup = read_csv("icd10_lookup.csv")

cause_hierarchy = read_csv("data/IHME_GBD_2016_CAUSE_HIERARCHY_Y2017M10D02.csv")

level3 = read_csv("gbd_icd10_lookup_level3_long.csv") %>% 
  rename(cause_id.level2 = cause_id)

level4 = read_csv("gbd_icd10_lookup_level4_long.csv") %>% 
  rename(cause_id.level3 = cause_id)


# check if everything in level 3 was also in level 2
check_4in3 = level4 %>% 
  left_join(level3)

in_level4_not_in_level3 = check_4in3 %>% 
  filter(is.na(cause_id.level3)) %>% 
  left_join(icd10_lookup)