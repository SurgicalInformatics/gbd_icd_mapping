library(tidyverse)


mapping_raw = read_csv("data/IHME_GBD_2016_ICD_CAUSE_MAP_CAUSES_OF_DEATH_Y2017M09D14.csv", skip = 1) %>% 
  select(cause_name = Cause, icd10 = ICD10)

cause_hierarchy = read_csv("data/IHME_GBD_2016_CAUSE_HIERARCHY_Y2017M10D02.csv") %>% 
  select(-cause_outline, -sort_order) # the outline looks confusingly/coincidentally similar to ICD but it's not

alldata = mapping_raw %>% 
  full_join(cause_hierarchy)

level2 = alldata %>% 
  filter(level == 2)


expand = level2 %>% 
  mutate(icd10 = str_replace(icd10, "X66-Y08.9", "X66-X99, Y00-Y08.9")) %>% # split a two-letter range
  mutate(icd10 = str_replace(icd10, "W99-X06.9", "W99, X00-Y06.9")) %>% # split a two-letter range
  select(cause_id, icd10) %>% 
  na.omit() %>% 
  #mutate(icd10_original = icd10) %>% 
  separate(icd10, into = paste0("group", 1:100), ",") %>% 
  #gather(group, icd10, -cause_id, -icd10_original) %>% 
  gather(group, icd10, -cause_id) %>% 
  mutate(icd10 = str_trim(icd10)) %>% 
  select(-group) %>% 
  na.omit() %>% 
  mutate(icd10 = ifelse(icd10 == "C47-C4A", "C47-C48", icd10)) %>% # typo in the lookup, 4A -> 48
  mutate(icd10_letter1 = str_extract(icd10, "[A-Z]")) %>% 
  mutate(icd_range = str_replace(icd10, "[A-Z]", "")) %>% 
  mutate(icd10_letter2 = str_extract(icd_range, "[A-Z]")) %>% 
  mutate(icd_range = str_replace(icd_range, "[A-Z]", "")) %>% 
  separate(icd_range, into = c("start", "end"), "-") %>% 
  mutate(end = ifelse(is.na(end), start, end)) %>% 
  mutate(is_end_with_decimal = str_detect(end, "\\.")) %>% 
  mutate(end_decimal = ifelse(is_end_with_decimal, end, paste0(end, ".9"))) %>% 
  mutate(start_numeric = as.numeric(start), end_numeric = as.numeric(end_decimal))


# if this is zero all ranges only cover one letter
expand %>% 
  filter(icd10_letter1 != icd10_letter2)

# this needs to be empty (end can't be before start)
expand %>% 
  filter(end_numeric < start_numeric)

seq.vectorized = Vectorize(seq.default, vectorize.args = c("from", "to"))

expand_range = expand %>% 
  group_by(icd10) %>% 
  mutate(range = seq.vectorized(from = start_numeric,
                                to   = end_numeric,
                                by   = 0.1) %>% paste(collapse = ",")) %>% 
  select(-start, -end, -icd10_letter2, -is_end_with_decimal, -end_decimal, -start_numeric, -end_numeric)

gather_range = expand_range %>% 
  separate(range, into = paste0("code-", 1:1500), ",") %>% 
  gather(number, value, -cause_id, -icd10, -icd10_letter1) %>% 
  na.omit() %>% 
  mutate(value_formatted = format(value %>% as.numeric(), nsmall = 1, trim = TRUE) %>% str_pad(4, side = "left", pad = "0")) %>% 
  mutate(icd10_expanded = paste0(icd10_letter1, value_formatted)) %>% 
  ungroup()

gather_range %>% 
  select(cause_id, icd10 = icd10_expanded) %>% 
  write_csv("gbd_icd10_lookup_level2_long.csv")



