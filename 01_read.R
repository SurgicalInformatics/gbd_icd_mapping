library(tidyverse)
library(modelr)

mapping_raw = read_csv("data/IHME_GBD_2016_ICD_CAUSE_MAP_CAUSES_OF_DEATH_Y2017M09D14.csv", skip = 1) %>% 
  select(cause_name = Cause, icd10 = ICD10)

cause_hierarchy = read_csv("data/IHME_GBD_2016_CAUSE_HIERARCHY_Y2017M10D02.csv") %>% 
  select(-cause_outline, -sort_order) # the outline looks confusingly/coincidentally similar to ICD but it's not

alldata = mapping_raw %>% 
  full_join(cause_hierarchy)

level2 = alldata %>% 
  filter(level == 2)

level3 = alldata %>% 
  filter(level == 3)

level4 = alldata %>% 
  filter(level == 4)


# checking if the missing level3s have information in thei child 4s (no they don't)
# level3_missing = level3%>% 
#   filter(is.na(icd10))
# level4 %>% 
#   filter(parent_id %in% level3_missing$cause_id)


# testing that ranges are one letter only
# mutate(icd10_letter_both = str_extract_all(icd10, "[A-Z]")) %>% 
expand = level3 %>% 
  select(cause_id, icd10) %>% 
  na.omit() %>% 
  separate(icd10, into = paste0("group", 1:50), ",") %>% 
  gather(group, icd10, -cause_id) %>% 
  mutate(icd10 = str_trim(icd10)) %>% 
  select(-group) %>% 
  na.omit() %>% 
  mutate(icd10_letter1 = str_extract(icd10, "[A-Z]")) %>% 
  mutate(icd_range = str_replace(icd10, "[A-Z]", "")) %>% 
  mutate(icd10_letter2 = str_extract(icd_range, "[A-Z]")) %>% 
  mutate(icd_range = str_replace(icd_range, "[A-Z]", "")) %>% 
  mutate(icd10_letter3 = str_extract(icd_range, "[A-Z]")) %>% 
  filter(icd10 != "C47-C4A") %>%  # we'll deal with this one later
  select(-icd10_letter3) %>% 
  separate(icd_range, into = c("start", "end"), "-") %>% 
  mutate(start = as.numeric(start), end = as.numeric(end))




single_codes_per_cause = expand %>% 
  filter(is.na(end))

range_of_codes = expand %>% 
  filter(!is.na(end))

seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to"))
c(seq2(from = range_of_codes$start, to = range_of_codes$end, by = 0.1))

range_of_codes %>% 
  filter(end < start)


for (i in 1:nrow(range_of_codes)){
  print(i)
  mystart = range_of_codes$
  expanded_range = seq()
  
}








