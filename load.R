library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

count_prop <- function(df, ...) {
  df %>%
    count(...) %>%
    mutate(prop = round(n / sum(n), 2))
}

pipsc_positions_raw <- read_excel("data/source/ati-tbs/A-2022-00110 Release package.xlsx") %>%
  clean_names() %>%
  mutate(
    across(
      c(
        position_number,
        geographic_location_code,
        supervisors_position_number,
        evaluation_process,
        position_classification_authorization,
        national_occupational_classification_noc_code
      ),
      as.character
    ),
    across(
      starts_with("degree"),
      as.character
    ),
    across(
      starts_with("points"),
      as.character
    ),
    across(
      contains("date"),
      as_date
    )
  )
  
psac_positions_raw <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", sheet = "Position Data") %>% clean_names() %>%
  select(organization_code:rejected_position) %>% # remove long tail of empty columns
  mutate(
    across(
      contains("date"),
      as_date
    )
  )


organization_codes <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!A10:A86", col_names = c("legend")) %>%
  separate(legend, c("organization_code", "organization"), sep = " - ", extra = "merge")

reasons_for_classification_decision <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!B10:B22", col_names = c("legend")) %>%
  separate(legend, c("reason_for_classification_decision", "reason_for_classification_decision_full"), sep = " - ", extra = "merge")

intended_incumbency_type <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!C10:C22", col_names = c("legend")) %>%
  separate(legend, c("intended_incumbency_type", "intended_incumbency_type_full"), sep = " - ", extra = "merge") %>%
  mutate(# fix for typo in source data legend
    intended_incumbency_type_full = if_else(is.na(intended_incumbency_type_full) & str_detect(intended_incumbency_type, "Full-time indeterminate"), "Full-time indeterminate employee", intended_incumbency_type_full),
    intended_incumbency_type = if_else(intended_incumbency_type_full == "Full-time indeterminate employee", "A", intended_incumbency_type)
  )

evaluation_process <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!D10:D64", col_names = c("legend")) %>%
  separate(legend, c("evaluation_process", "evaluation_process_full"), sep = " - ", extra = "merge")

position_classification_authorizations <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!E10:E19", col_names = c("legend")) %>%
  separate(legend, c("position_classification_authorization", "position_classification_authorization_full"), sep = " - ", extra = "merge")

position_statuses <- read_excel("data/source/ati-tbs/A-2021-00256 Release package.xlsx", range = "Legend!F10:F11", col_names = c("legend")) %>%
  separate(legend, c("position_status", "position_status_full"), sep = " - ", extra = "merge") %>%
  mutate(position_status_full = if_else(position_status_full == "Vaccant", "Vacant", position_status_full))


positions <- bind_rows(
  list(
    "pipsc (A-2022-00110)" = pipsc_positions_raw,
    "psac (A-2021-00256)" = psac_positions_raw
  ),
  .id = "source"
) %>%
  select(-degree_1:-points_16) %>% # TODO: recode various fields based on A-2021-00256 legend
  left_join(organization_codes) %>%
  mutate(
    position_status = str_replace_all(position_status, position_statuses %>% deframe)
  ) %>%
  extract(position_classification_code, into = c("group", "level"), regex = "([A-Z]*)[^[:alnum:]]*([0-9]*)", remove = FALSE, convert = TRUE) %>%
  extract(supervisors_position_classification_code, into = c("supervisor_group", "supervisor_level"), regex = "([A-Z]*)[^[:alnum:]]*([0-9]*)", remove = FALSE, convert = TRUE)
