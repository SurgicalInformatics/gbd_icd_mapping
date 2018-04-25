library(tidyverse)


# TODO: do not remove X60
icd10_lookup = read_csv("icd10_lookup.csv")

mapping_raw = read_csv("data/IHME_GBD_2016_ICD_CAUSE_MAP_CAUSES_OF_DEATH_Y2017M09D14.csv", skip = 1) %>% 
  select(cause_name = Cause, icd10 = ICD10)

mapping_missing = read_csv("data/GBD_map_missing_surginf_17042018.csv", skip = 1) %>% 
  select(cause_name = Cause, icd10 = ICD10)

mapping_raw = bind_rows(mapping_raw, mapping_missing) %>% 
  na.omit()

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


# checking if the missing level3s have information in their child 4s (no they don't)
# level3_missing = level3%>% 
#   filter(is.na(icd10))
# level4 %>% 
#   filter(parent_id %in% level3_missing$cause_id)



# testing that ranges are one letter only
# mutate(icd10_letter_both = str_extract_all(icd10, "[A-Z]")) %>% 
expand = level3 %>% 
  mutate(icd10 = str_replace(icd10, "X85-Y08.9", "X85-X99, Y00-Y08.9")) %>% # split a two-letter range
  select(cause_id, icd10) %>% 
  na.omit() %>% 
  #mutate(icd10_original = icd10) %>% 
  separate(icd10, into = paste0("group", 1:50), ",") %>% 
  #gather(group, icd10, -cause_id, -icd10_original) %>% 
  gather(group, icd10, -cause_id) %>% 
  mutate(icd10 = str_trim(icd10)) %>% 
  select(-group) %>% 
  na.omit() %>% 
  mutate(icd10 = ifelse(icd10 == "C47-C4A", "C47-C48", icd10)) %>% # typo in the lookup, 4A -> 48
  bind_rows(data_frame(cause_id = 488, icd10 = "C49")) %>% #add in C49 (present in level2 but missing in level3)
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
  ungroup() %>% 
  mutate(value_formatted = format(value %>% as.numeric(), nsmall = 1, trim = TRUE) %>% str_pad(4, side = "left", pad = "0")) %>% 
  mutate(icd10_expanded = paste0(icd10_letter1, value_formatted)) %>% 
  ungroup() %>% 
  filter(icd10_expanded %in% icd10_lookup$icd10) %>% 
  distinct(cause_id, icd10_expanded, .keep_all = TRUE)

gather_range %>% 
  select(cause_id, icd10 = icd10_expanded) %>% 
  left_join(icd10_lookup) %>% 
  arrange(icd10_name, icd10) %>% 
  write_csv("gbd_icd10_lookup_level3_long.csv")

for_checking = gather_range %>% 
  group_by(icd10, cause_id) %>% 
  summarise(expanded = paste0(icd10_expanded, collapse = ",")) %>% 
  group_by(cause_id) %>% 
  summarise(expanded = paste0(expanded, collapse = ",")) %>% 
  ungroup() %>% 
  full_join(level3) %>% 
  select(cause_id, cause_name, icd10_lookup_provided = icd10, icd10_ranges_expanded = expanded)

write_csv(for_checking, "gbd_icd10_lookup_level3_for_checking.csv")




