# libs ---------------------------------------------------------
library(tidyverse)
library(olsrr)
library(rvest)
extrafont::loadfonts(device = "win")

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
gcses <- read_csv("data/2018-2019/england_ks4revised.csv", na = na_values) %>% 
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

write.csv(gcses, "data/gcses-cleaned.csv")
  

# visualisation -------------------------------------------

  # histogram of just state schools attainment 8
gcses %>% 
  filter(school_type == "State School") %>% 
  ggplot() +
  geom_density(aes(att8, fill = school_type, colour = school_type), alpha = 0.5, size = 1.2) +
  theme_minimal() +
  scale_colour_manual(values = c("#A2CD5D")) +
  scale_fill_manual(values = c("#d3e8b0")) +
  labs(x = "Average Attainment 8",
       y = "Density",
       fill = "",
       colour = "") +
  theme(legend.position = "none",
        text = element_text(family = "Arial Nova"),
        panel.grid.minor = element_blank())

  # histogram of all school types
gcses %>% 
  ggplot() +
  geom_density(aes(att8, fill = school_type, colour = school_type), alpha = 0.5, size = 1.2) +
  theme_minimal() +
  scale_colour_manual(values = c("#EF6351", "#F9BD56", "#A2CD5D")) +
  scale_fill_manual(values = c("#f6a79d", "#fcdda6", "#d3e8b0")) +
  labs(x = "Attainment 8 Score",
       y = "Density",
       fill = "",
       colour = "") +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial Nova"),
        panel.grid.minor = element_blank())

p1 <- gcses %>% 
  filter(school_type != "Special School") %>% 
  ggplot() +
  geom_point(aes(perc_fsm, att8, colour = school_type), alpha = 0.2, size = 1.5) +
  labs(x = "% FSM",
       y = "Att 8 Score",
       colour = "School Type") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova")) +
  scale_colour_manual(values = c("#EF6351", "#A2CD5D"))

# correlation between eal and att8 score
p2 <- gcses %>% 
  filter(school_type != "Special School") %>% 
  ggplot() +
  geom_point(aes(perc_sec_lang, att8, colour = school_type), alpha = 0.4) +
  labs(x = "% EAL",
       y = "Att 8 Score",
       colour = "School Type") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom") +
  scale_colour_manual(values = c("#EF6351", "#A2CD5D"))

  # relationship between gender policy and att 8
gcses %>% 
  filter(school_type != "Special School") %>% 
  ggplot() +
  geom_boxplot(aes(gender, att8, fill = gender), alpha = 0.8) +
  facet_wrap(~school_type) +
  labs(x = "",
       y = "Att 8 Score",
       fill = "") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom") +
  scale_fill_manual(values = c("#EF6351", "#F9BD56", "#A2CD5D"))
 
  # correlate att 8 and pupil number  
gcses %>% 
  filter(school_type != "Special School") %>% 
  ggplot() + 
  geom_point(aes(num_pupils, att8, colour = school_type), alpha = 0.4) +
  labs(x = "Number of Pupils",
       y = "Att 8 Score",
       colour = "School Type") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom") +
  scale_colour_manual(values = c("#EF6351", "#A2CD5D"))

  # correlate att8 and % sen

gcses %>% 
  filter(school_type != "Special School") %>%
  filter(perc_sen != 100) %>% 
  ggplot() + 
  geom_point(aes(perc_sen, att8, colour = school_type), alpha = 0.4) +
  labs(x = "% SEN",
       y = "Att 8 Score",
       colour = "School Type") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom") +
  scale_colour_manual(values = c("#EF6351", "#A2CD5D"))

  # correlation with 2017-18 grades
gcses %>% 
  filter(school_type != "Special School") %>% 
  ggplot() +
  geom_point(aes(att8_17, att8, colour = school_type), alpha = 0.5) +
  labs(x = "2017-2018 Attainment 8",
       y = "2018-2019 Attainment 8",
       colour = "School Type") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom") +
  scale_colour_manual(values = c("#EF6351", "#A2CD5D"))

# regression analysis ---------------------------------------

  # additive model
gcses_cleaned <- gcses %>% 
  filter(school_type != "Special School")

model.a <- rq(att8 ~ school_type + perc_fsm + perc_sec_lang + gender + perc_sen + num_pupils,
              data = gcses_cleaned,
              tau = 0.5)

summary(model.a)

model.a.null = rq(att8 ~ 1,
                data = gcses_cleaned,
                tau = 0.5)

anova(model.a, model.a.null)

  # interactive model
model.i <- rq(att8 ~ school_type + num_pupils:school_type + gender + gender:school_type + perc_sec_lang 
              + perc_sec_lang + perc_fsm + perc_fsm:school_type + perc_sen,
              data = gcse_cleaned,
              tau = 0.5) # seems to be some interactions missing...

summary(model.i)

model.i.null = rq(att8 ~ 1,
                  data = gcses_cleaned,
                  tau = 0.5)

anova(model.i, model.i.null)

 # just state schools

gcses_state <- gcses %>% filter(school_type == "State School")

model.s <- rq(att8 ~ att8_17 + perc_fsm + gender + perc_sec_lang + perc_sen, data = gcses_state, tau = 0.5)

summary(model.s)

model.s.null = rq(att8 ~ 1, data = gcses_state, tau = 0.5)

anova(model.s, model.s.null)

goodfit(model.s, model.s.null, 0.5)

# fsm disparity analysis ----------------------------------

gcse_state %>% 
  mutate(fsm_diff_att8 = att8_nonfsm - att8_fsm) %>% 
  ggplot() +
  geom_point(aes(perc_fsm, fsm_diff_att8), alpha = 0.25, colour = "#1A237E") +
  labs(x = "FSM Students (%)",
       y = "FSM/non-FSM Disparity") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom")


# non-FSM score as a function of % fsm
gcse_state %>% 
  mutate(fsm_diff_att8 = att8_nonfsm - att8_fsm) %>% 
  ggplot() +
  geom_point(aes(fsm_diff_att8, att8_nonfsm), alpha = 0.2, colour = "#1A237E") +
  labs(x = "FSM/non-FSM Score Disparity",
       y = "non-FSM att8") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom")

# FSM score as a function of % fsm
gcse_state %>% 
  mutate(fsm_diff_att8 = att8_nonfsm - att8_fsm) %>% 
  ggplot() +
  geom_point(aes(fsm_diff_att8, att8_fsm), alpha = 0.2, colour = "#1A237E") +
  labs(x = "FSM/non-FSM Score Disparity",
       y = "FSM att8") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Nova"),
        legend.position = "bottom")



