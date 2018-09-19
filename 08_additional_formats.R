library(tidyverse)

main_format  = read_csv("formatted_data/icd10_to_gdb_level3.csv")

main_format %>% 
  mutate(icd10 = str_remove(icd10, "\\.")) %>% 
  write_csv("formatted_data/icd10_to_gdb_level3_nodots.csv")


# main_format %>% 
#   group_by(cause_id.level3) %>% 
#   summarise(icd10 = paste0(icd10, collapse = ",")) %>% 
#   ungroup()
