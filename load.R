library(tidyverse)
library(readxl)
library(janitor)

pipsc_positions_raw <- read_excel("data/source/ati-tbs/A-2022-00110 Release package.xlsx") %>% clean_names()

pipsc_positions <- pipsc_positions_raw %>%
  select(-degree_1:-points_16) %>% # TODO: recode various fields based on A-2021-00256 legend
  extract(position_classification_code, into = c("group", "level"), regex = "([A-Z]*)[^[:alnum:]]*([0-9]*)", remove = FALSE, convert = TRUE) %>%
  extract(supervisors_position_classification_code, into = c("supervisor_group", "supervisor_level"), regex = "([A-Z]*)[^[:alnum:]]*([0-9]*)", remove = FALSE, convert = TRUE)

psac_positions_raw <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", sheet = "Position Data") %>% clean_names() %>%
  select(organization_code:rejected_position)

psac_positions <- psac_positions_raw %>%
  select(-degree_1:-points_16) %>%
  extract(position_classification_code, into = c("group", "level"), regex = "([A-Z]*)[^[:alnum:]]*([0-9]*)", remove = FALSE, convert = TRUE)

organization_codes <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!A10:A86", col_names = c("legend")) %>%
  separate(legend, c("organization_code", "organization"), sep = " - ")

reasons_for_classification_decision <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!B10:B22", col_names = c("legend")) %>%
  separate(legend, c("reason_for_classification_decision", "reason_for_classification_decision_full"), sep = " - ")

intended_incumbency_type <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!C10:C22", col_names = c("legend")) %>%
  separate(legend, c("intended_incumbency_type", "intended_incumbency_type_full"), sep = " - ")
