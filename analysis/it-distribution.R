source("load.R")

# ==== Helper variables ====

it_positions <- positions %>%
  filter(group == "IT") %>%
  mutate(organization = case_when(
    branch_directorate_division == "Canadian Digital Ser" ~ "TBS-CDS",
    organization == "Treasury Board of Canada Secretariat" & branch_directorate_division != "Canadian Digital Ser" ~ "Treasury Board of Canada Secretariat (excl. CDS)",
    TRUE ~ organization
  ))

orgs_by_bin <- positions %>%
  count(organization, name = "n_positions") %>%
  mutate(
    all_positions_bin = cut(# TODO: adjust these bin sizes
      n_positions,
      breaks = c(0, 50, 150, 500, Inf),
      labels = c("Micro (0-50)", "Small (51-150)", "Medium (151-500)", "Large (501+)")
    )
  )

orgs_by_it_bin <- it_positions %>%
  count(organization, name = "n_it_positions") %>%
  mutate(
    it_positions_bin = cut(
      n_it_positions,
      breaks = c(0, 50, 150, 500, Inf),
      labels = c("Micro (0-50)", "Small (51-150)", "Medium (151-500)", "Large (501+)")
    )
  )





# ==== Level distributions ====

it_distribution_by_dept <- it_positions %>%
  # filter(branch_directorate_division != "Canadian Digital Ser") %>%
  group_by(organization) %>%
  count_prop(level) %>%
  pivot_wider(id_cols = organization, names_from = level, values_from = n, values_fill = 0) %>% # pivot wider then longer to make sure we have levels 1:5 for every org
  pivot_longer(cols = c(-organization), names_to = "level", names_transform = as.integer, values_to = "n") %>%
  mutate(prop = round(n / sum(n), 2)) %>%
  ungroup %>%
  left_join(orgs_by_it_bin %>% select(organization, it_positions_bin))

it_distribution_gc_wide <- it_positions %>%
  # filter(branch_directorate_division != "Canadian Digital Ser") %>%
  count_prop(level) %>%
  mutate(
    organization = "GC-wide",
    it_positions_bin = organization
  )

it_distribution_cds <- it_positions %>%
  filter(branch_directorate_division == "Canadian Digital Ser") %>%
  count_prop(level) %>%
  mutate(
    organization = "TBS-CDS",
    it_positions_bin = organization
  )

it_distribution_by_dept %>%
  mutate(
    it_positions_bin = if_else(organization == "TBS-CDS", "TBS-CDS", as.character(it_positions_bin))
  ) %>%
  group_by(it_positions_bin, level) %>%
  summarize(avg_prop = mean(prop)) %>%
  bind_rows(
    it_distribution_gc_wide %>%
      select(it_positions_bin = organization, level, avg_prop = prop),
  ) %>%
  ggplot(aes(x = level, y = avg_prop, color = it_positions_bin, fill = it_positions_bin)) +
  geom_line() +
  ylim(c(0, 0.5)) +
  labs(
    title = "% of IT positions by level (average), by organization category",
    y = "% of positions (average)"
  )

bind_rows(
  it_distribution_by_dept,
  it_distribution_gc_wide,
  it_distribution_cds
) %>%
  ggplot(aes(x = level, y = prop, color = organization, fill = organization)) +
  geom_line() +
  ylim(c(0, 1))

bind_rows(
  it_distribution_by_dept,
  it_distribution_cds
) %>%
  filter(it_positions_bin != "Micro (0-50)") %>%
  left_join(
    it_distribution_cds %>%
      select(level, cds_prop = prop)
  ) %>%
  mutate(prop_vs_cds = prop - cds_prop)

bind_rows(
  it_distribution_by_dept,
  it_distribution_cds
) %>%
  filter(it_positions_bin != "Micro (0-50)") %>%
  left_join(
    it_distribution_cds %>%
      select(level, cds_prop = prop)
  ) %>%
  mutate(prop_vs_cds = prop - cds_prop) %>%
  group_by(level) %>%
  summarize(avg_prop_diff = round(mean(prop_vs_cds), 6))





# ==== Average levels ====

it_avg_level_by_dept <- it_positions %>%
  # filter(branch_directorate_division != "Canadian Digital Ser") %>% # allows measuring TBS without CDS's impact; average changes by ~ .15
  group_by(organization) %>%
  summarize(avg_level = mean(level)) %>%
  left_join(orgs_by_it_bin %>% select(organization, it_positions_bin))

it_avg_level_gc_wide <- it_positions %>%
  summarize(avg_level = mean(level)) %>%
  mutate(
    organization = "GC-wide",
    it_positions_bin = organization
  )

it_avg_level_cds <- it_positions %>%
  filter(branch_directorate_division == "Canadian Digital Ser") %>%
  summarize(avg_level = mean(level)) %>%
  mutate(
    organization = "TBS-CDS",
    it_positions_bin = organization
  )

bind_rows(
  it_avg_level_cds,
  it_avg_level_gc_wide,
  it_avg_level_by_dept
) %>%
  left_join(orgs_by_it_bin) %>%
  filter(it_positions_bin != "Micro (0-50)") %>%
  mutate(avg_level_bin = cut_interval(avg_level, n = 15)) %>%
  ggplot(aes(x = avg_level)) +
  geom_histogram(bins = 15)




it_positions %>%
  left_join(orgs_by_it_bin) %>%
  ggplot(aes(y = level, x = it_positions_bin)) +
  geom_boxplot()
