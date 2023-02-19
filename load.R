library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(arrow)

count_prop <- function(df, ...) {
  df %>%
    count(...) %>%
    mutate(prop = round(n / sum(n), 2))
}

# TODO: when loading from full, manually set column types in read_excel, erring on "character" to preserve leading zeroes etc (especially for variables in legend)

positions_raw <- read_excel("data/source/ati-tbs/A-2022-01431 records.xlsx") %>%
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

positions_raw %>% write_parquet("data/out/positions-raw.parquet")


organization_codes <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!A10:A78", col_names = c("legend")) %>%
  separate(legend, c("organization_code", "organization"), sep = " - ", extra = "merge")

reasons_for_classification_decision <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!D10:D22", col_names = c("legend")) %>%
  separate(legend, c("reason_for_classification_decision", "reason_for_classification_decision_full"), sep = " - ", extra = "merge")

intended_incumbency_type <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!E10:E22", col_names = c("legend")) %>%
  separate(legend, c("intended_incumbency_type", "intended_incumbency_type_full"), sep = " - ", extra = "merge") %>%
  mutate(# fix for typo in source data legend
    intended_incumbency_type_full = if_else(is.na(intended_incumbency_type_full) & str_detect(intended_incumbency_type, "Full-time indeterminate"), "Full-time indeterminate employee", intended_incumbency_type_full),
    intended_incumbency_type = if_else(intended_incumbency_type_full == "Full-time indeterminate employee", "A", intended_incumbency_type)
  )

evaluation_process <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!G10:G64", col_names = c("legend")) %>%
  separate(legend, c("evaluation_process", "evaluation_process_full"), sep = " - ", extra = "merge")

position_classification_authorizations <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!H10:H19", col_names = c("legend")) %>%
  separate(legend, c("position_classification_authorization", "position_classification_authorization_full"), sep = " - ", extra = "merge")

position_statuses <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!K10:K11", col_names = c("legend")) %>%
  separate(legend, c("position_status", "position_status_full"), sep = " - ", extra = "merge") %>%
  mutate(position_status_full = if_else(position_status_full == "Vaccant", "Vacant", position_status_full))

group_labels <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!B10:B198", col_names = c("legend")) %>%
  separate(legend, c("group", "group_label"), sep = " - ", extra = "merge")

noc_2016 <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!I9:I508", col_names = c("legend")) %>%
  separate(legend, c("noc_2016", "noc_2016_full"), sep = " - ", extra = "merge")

noc_2021 <- read_excel("data/source/ati-tbs/A-2022-01430 Database schema of the Position Classification Information System.xlsx", range = "Legend!J9:J524", col_names = c("legend")) %>%
  separate(legend, c("noc_2021", "noc_2021_full"), sep = " - ", extra = "merge")

source("load/pay.R")

known_missing_positions <- read_csv("data/indexes/missing-positions.csv") %>%
  filter(! str_detect(position_title_english, fixed("?"))) %>% # removes missing positions that we're not bothering with
  select(-comments) %>%
  separate(position_gid, into = c("organization_code", "position_number"), remove = FALSE) %>%
  extract(supervisor_gid, into = c("supervisors_position_number"), regex = "([0-9]+)", remove = FALSE) %>%
  select(-position_gid, -supervisor_gid)

corrections_supervisor <- read_csv("data/indexes/supervisor-corrections.csv") %>%
  select(-comments) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(tmp_index = as.integer(tmp_index))

positions <- positions_raw %>%
  mutate(tmp_index = row_number()) %>%
  rows_update(
    corrections_supervisor,
    by = c("organization_code", "position_number", "tmp_index")
  ) %>%
  select(-tmp_index) %>%
  select(-degree_1:-points_16) %>% # TODO: recode various fields based on A-2021-00256 legend
  bind_rows(known_missing_positions) %>%
  left_join(organization_codes) %>%
  mutate(
    position_status = str_replace_all(position_status, position_statuses %>% deframe)
  ) %>%
  extract(position_classification_code, into = c("group", "level"), regex = "([A-Z]*)[^[:alnum:]]*([0-9]*)", remove = FALSE, convert = TRUE) %>%
  extract(supervisors_position_classification_code, into = c("supervisor_group", "supervisor_level"), regex = "([A-Z]*)[^[:alnum:]]*([0-9]*)", remove = FALSE, convert = TRUE) %>%
  left_join(group_labels) %>%
  left_join(intended_incumbency_type) %>%
  left_join(evaluation_process %>%
              mutate(evaluation_process = as.character(as.integer(evaluation_process)))
  ) %>%
  left_join(position_classification_authorizations) %>%
  left_join(reasons_for_classification_decision) %>%
  left_join(
    core_pay_rates %>%
      select(group, level, pay_max)
  )

positions %>% write_parquet("data/out/positions.parquet")

# TODO:
# - consider binning by n (i.e., "we want 5 bins, make them the size that works")
# - consider binning just on filled positions (how many vacant positions are... extra?)
# - consider adding a break at 5000
orgs_by_bin <- positions %>%
  count(organization, name = "org_n_positions") %>%
  mutate(
    all_positions_bin = cut(
      org_n_positions,
      breaks = c(0, 50, 250, 1000, 10000, Inf),
      labels = c("Micro (0-50)", "Small (51-250)", "Medium (251-1000)", "Large (1,001-5,000)", "Very Large (10,001+)")
    )
  )

positions_binned <- positions %>%
  select(organization_code:organization) %>%
  left_join(orgs_by_bin, by = c("organization"))

positions_binned %>% write_parquet("data/out/positions-binned.parquet")

positions <- read_parquet("data/out/positions.parquet")
positions_binned <- read_parquet("data/out/positions-binned.parquet")
