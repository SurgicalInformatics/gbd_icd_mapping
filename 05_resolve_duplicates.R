library(tidyverse)

icd10_lookup = read_csv("icd10_lookup.csv")

cause_hierarchy = read_csv("original_data/IHME_GBD_2016_CAUSE_HIERARCHY_Y2017M10D02.csv") %>% 
  select(cause_id, cause_name)

level3 = read_csv("interim_checks/gbd_icd10_lookup_level3_long.csv") %>% 
  rename(cause_id.level3 = cause_id) %>% 
  left_join(icd10_lookup)

duplicate_decisions = read_csv("original_data/duplicated_ICD_to_GBD_mappings.csv") %>% 
  rename(icd10_name = disease, decision = `decision (1 or 2)`) %>% 
  mutate(group1_id = str_extract(group1, "[:digit:]{3}")) %>% 
  mutate(group2_id = str_extract(group2, "[:digit:]{3}")) %>% 
  mutate(cause_id.level3 = ifelse(decision == 1, group1_id, group2_id) %>% as.integer())


resolved_duplicates = duplicate_decisions %>% 
  select(cause_id.level3, icd10, icd10_name)


icd10_to_gdb_level3 = level3 %>% 
  filter(! icd10 %in% resolved_duplicates$icd10) %>% 
  bind_rows(resolved_duplicates) %>% 
  left_join(cause_hierarchy, by = c("cause_id.level3" = "cause_id")) %>% 
  select(cause_id.level3, cause_name, icd10, icd10_name) %>% 
  arrange(cause_id.level3)

# nrow(level3) - nrow(icd10_to_gdb_level3)

icd10_to_gdb_level3 %>% 
  write_csv("formatted_data/icd10_to_gdb_level3.csv")
