source("load.R")

positions_rr <- positions %>%
  mutate(
    position_supervisor_group_match = group == supervisor_group,
    position_supervisor_match_or_supervisor_ex = (group == supervisor_group | supervisor_group == "EX")
  )

positions_rr %>%
  group_by(organization, group) %>%
  count_prop(position_supervisor_group_match) %>%

positions_rr %>%
  group_by(organization) %>%
  count_prop(position_supervisor_group_match) %>%
  arrange(position_supervisor_group_match, -prop)

positions_rr %>%
  group_by(organization, group) %>%
  count_prop(position_supervisor_match_or_supervisor_ex)

positions_rr %>%
  group_by(organization) %>%
  count_prop(position_supervisor_match_or_supervisor_ex) %>%
  arrange(position_supervisor_match_or_supervisor_ex, -prop)

positions_rr %>%
  filter(group == "IT") %>%
  group_by(organization) %>%
  count_prop(position_supervisor_group_match) %>%
  arrange(position_supervisor_group_match, -prop)

positions_rr %>%
  filter(group == "IT") %>%
  group_by(organization) %>%
  count_prop(position_supervisor_match_or_supervisor_ex) %>%
  arrange(position_supervisor_match_or_supervisor_ex, -prop)
