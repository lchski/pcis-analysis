core_pay_rates_raw <- read_csv("data/source/github.com/meetingcostcalculator/meeting-cost-calculator-data/ca/rates/core.csv")

core_pay_rates <- core_pay_rates_raw %>%
  extract(label, into = c("level"), regex = "([0-9]+)$", remove = FALSE) %>%
  mutate(
    group = str_remove(label, str_glue("-{level}")),
    group = str_remove_all(group, "-")
  ) %>%
  mutate(
    group = if_else(# fix bespoke format
      description == "Grain Inspection",
      "PICGC",
      group
    ),
    level = if_else(# fix bespoke format
      description == "Grain Inspection",
      str_extract(label, "([0-9]+)"),
      level
    ),
    group = if_else(# reflect usage in data
      description == "Printing Operations (Supervisory) Group",
      "PR",
      group
    ),
    group = if_else(# reflect update to collective agreement
      description == "Computer Systems",
      "IT",
      group
    )
  ) %>%
  mutate(
    level = as.integer(level)
  ) %>%
  select(
    group,
    group_label = description,
    level,
    pay_min = min,
    pay_max = max
  )
