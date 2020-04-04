# script to extract and clean 2018-2019 att 8 data

# libs --------------------------------------
library(tidyverse)
library(rvest)

# scrape list of partially selective schools -------------------

path <- "//*[@id='mw-content-text']/div/table[3]"

url <- "https://en.wikipedia.org/wiki/Partially_selective_school_(England)"

page <- xml2::read_html(url)

partially_selective_schools <- page %>% 
  html_node(xpath = path) %>% 
  html_table() %>% 
  mutate(LEA = str_remove_all(LEA, "[0-9]")) %>% 
  mutate(LEA = str_remove(LEA, "\\[]")) %>% 
  mutate(School = str_remove_all(School, "[0-9]")) %>% 
  mutate(School = str_remove(School, "\\[]")) %>% 
  rename(school = "School")

# data ----------------------------------------------------------

# list of schools from partially selective table with diff names than they appear on main df
unmatched_schools <- read_csv("data/2018-2019/unmatched_schools.csv")

special_schools <- c("CYS", "FDS", "NMSS", "ACS", "FS", "INDSPEC", "ACCS", "INDSPS", "SS")

na_values <- c("NA", "SUPP", "NE", "NP", "SP", "RE", "LOWCOV", "NEW", "")

# 2017-2018 gcse data
gcses_17 <- read_csv("data/2017-2018/2017-2018_england_ks4final.csv") %>% 
  select(urn = "URN",
         att8_17 = "ATT8SCR")

# 2018-2019 gcse data
gcses <- read_csv("data/2018-2019/england_ks4revised.csv", na = na_values) 

# cleaning ------------------------------------------------------

gcses_cleaned <- gcses %>% 
  select(la = "LEA",
         urn = "URN",
         school = "SCHNAME",
         school_description = "NFTYPE",
         adm_policy = "ADMPOL",
         num_pupils = "TPUP",
         perc_boys = "PBPUP",
         perc_girls = "PGPUP",
         perc_fsm = "PTFSM6CLA1A",
         perc_sec_lang = "PTEALGRP2",
         perc_sen = "PSEN_ALL",
         att8 = "ATT8SCR",
         att8_eng = "ATT8SCRENG",
         att8_maths = "ATT8SCRMAT",
         att8_fsm = "ATT8SCR_FSM6CLA1A",
         att8_nonfsm = "ATT8SCR_NFSM6CLA1A") %>% 
  mutate_at(c("perc_boys", "perc_girls", "perc_fsm", "perc_sec_lang", "perc_sen"), ~str_remove(., "\\%")) %>% 
  mutate_at(c("perc_boys", "perc_girls", "perc_fsm", "perc_sec_lang", "perc_sen"), as.numeric) %>% 
  mutate(school_type = case_when(
    school_description %in% special_schools ~ "Special School",
    school_description == "IND" ~ "Independent School",
    adm_policy == "SEL" ~ "Grammar School",
    school %in% partially_selective_schools$school  | school %in% unmatched_schools$school ~ "Partially Selective School",
    TRUE ~ "State School"
  )) %>% 
  mutate(gender = case_when(
    perc_girls == 100 ~ "Girls",
    perc_boys == 100 ~ "Boys",
    TRUE ~ "Mixed"
  )) %>% 
  filter(!(is.na(urn))) %>% 
  filter(!(is.na(att8))) %>% 
  filter(school_type != "Independent School" & school_type != "Partially Selective School") %>% 
  left_join(gcses_17, by = "urn")

write.csv(gcses, "data/gcses-cleaned.csv", row.names = FALSE)