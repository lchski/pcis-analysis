library(tidygraph)

library(ggraph)
library(ggiraph)
library(visNetwork)

# NB: ATSSC, CSA (Space!), Stats, and Transport are all missing position IDs and supervisor position IDs (more or less)
#     This means they're not suitable for network analysis.
position_nodes_raw <- positions %>%
  filter(! is.na(position_number) & ! is.na(supervisors_position_number)) %>%
  mutate(
    position_gid = str_glue("{organization_code}-{position_number}"),
    supervisor_gid = str_glue("{organization_code}-{supervisors_position_number}")
  )

# NB: we do this to fill in the graph, but it's arguably relevant in other analyses and could go to `load.R`, even if missing various data
inferred_position_nodes <- position_nodes_raw %>%
  select(# TODO: left_join pay_max for the inferred positions
    supervisor_gid,
    supervisors_position_number,
    supervisors_position_classification_code,
    supervisor_group,
    supervisor_level,
    organization_code,
    organization,
    branch_directorate_division # TBD if there are any other fields we could include—even this is maybe a stretch
  ) %>%
  anti_join(
    position_nodes_raw %>%
      select(position_gid, position_classification_code),
    by = c(
      "supervisor_gid" = "position_gid"
      # "supervisors_position_classification_code" = "position_classification_code" # NB: this line creates a small number of duplicate positions, where the supervisor's position already exists but the recorded supervisor group/level differs from the existing position
    )
  ) %>%
  mutate(position_title_english = "Unknown") %>%
  rename(
    position_gid = supervisor_gid,
    position_number = supervisors_position_number,
    position_classification_code = supervisors_position_classification_code,
    group = supervisor_group,
    level = supervisor_level
  ) %>%
  distinct(position_gid, .keep_all = TRUE) # TODO: consider sorting by -pay_max prior to this (maybe include for sort, then drop), so we get the branch of the _most senior_ person reporting to this role

position_nodes <- position_nodes_raw %>%
  bind_rows(inferred_position_nodes) %>%
  mutate(
    node_id = row_number()
  ) %>%
  group_by(organization) %>%
  mutate(
    org_node_id = row_number()
  ) %>%
  ungroup

position_edges <- position_nodes %>%
  select(
    position_gid,
    supervisor_gid
  ) %>%
  left_join(
    position_nodes %>%
      select(position_gid, from = node_id)
  ) %>%
  left_join(
    position_nodes %>%
      select(position_gid, to = node_id),
    by = c("supervisor_gid" = "position_gid")
  )

positions_graph_raw <- tbl_graph(
  nodes = position_nodes,
  edges = position_edges %>%
    filter(! is.na(from) & ! is.na(to))
) %>%
  mutate(
    reports_total = local_size(order = nrow(position_nodes), mindist = 1, mode = "in"),
    reports_direct = local_size(mindist = 1, mode = "in"),
    reports_indirect = reports_total - reports_direct,
    ranks_from_top = node_eccentricity(mode = "out"),
    is_supervisor = reports_total > 0
  )

## NB: VERY expensive operation (maps over the connected nodes for every node), consider parallelizing by department or something
system.time({
  positions_graph_with_pay <- positions_graph_raw %>%
    # filter(organization_code == "GSS") %>%
    select(position_gid, pay_max) %>%
    activate(edges) %>%
    select(from, to) %>%
    activate(nodes) %>%
    mutate(
      supervised_salary_total = map_local_dbl(order = nrow(position_nodes), mindist = 1, mode = "in", .f = function(neighborhood, ...) {
        sum(as_tibble(neighborhood, active = "nodes")$pay_max, na.rm = TRUE)
      }),
      supervised_salary_direct = map_local_dbl(mindist = 1, mode = "in", .f = function(neighborhood, ...) {
        sum(as_tibble(neighborhood, active = "nodes")$pay_max, na.rm = TRUE)
      }),
      supervised_salary_indirect = supervised_salary_total - supervised_salary_direct
    )
})
# positions_graph_with_pay %>% write_rds("data/out/positions-graph-pay.RDS")
# positions_graph_with_pay <- read_rds("data/out/positions-graph-pay.RDS")

positions_graph <- positions_graph_raw %>%
  graph_join(
    positions_graph_with_pay %>% activate(edges) %>% filter(from == 0) %>% activate(nodes)
  )



## exploration

### understand how many different trees there are in an org
### more trees = likely more created / inferred positions, or broken reporting chains
### NB: makes sense that DND and RCMP are so many, since their positions would include RMs / CAF that aren't stored in PCIS
positions_graph %>%
  filter(ranks_from_top == 0) %>%
  as_tibble %>%
  count(organization, sort = TRUE)

## analysis

### study a particular organization
positions_graph %>%
  filter(organization_code == "IMC") %>%
  select(
    position_gid,
    grp_lvl = position_classification_code,
    branch_directorate_division,
    position_title_english,
    sup_pos_gid = supervisor_gid,
    sup_grp_lvl = supervisors_position_classification_code,
    position_status,
    reports_direct:last_col()
  ) %>%
  as_tibble %>% View

### study a position of interest
positions_graph %>%
  mutate(distance_to_position_of_interest = node_distance_to(position_gid == "TBD-2986")) %>%
  filter(position_status == "Occupied") %>%
  filter(distance_to_position_of_interest != Inf) %>%
  select(
    position_gid,
    grp_lvl = position_classification_code,
    branch_directorate_division,
    position_title_english,
    sup_pos_gid = supervisor_gid,
    sup_grp_lvl = supervisors_position_classification_code,
    position_status,
    reports_direct:last_col()
  ) %>%
  as_tibble %>% View()

### reporting responsibilities
positions_graph %>%
  as_tibble %>%
  group_by(
    # is_central_agency = organization_code %in% c("PCO", "TBD", "FIN"), # or other axis of interest, e.g., NCR vs not
    is_ncr = work_location_english %in% c("Ottawa", "Gatineau", "Hull"),
    grp_lvl = position_classification_code
  ) %>%
  summarize(
    count = n(),
    max_direct = max(reports_direct),
    min_direct = min(reports_direct),
    avg_direct = mean(reports_direct),
    max_indirect = max(reports_indirect),
    min_indirect = min(reports_indirect),
    avg_indirect = mean(reports_indirect)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  arrange(grp_lvl)

### accuracy of supervisory factor code, vs classified org chart?
### verdict: `supervisory_factor_code` seems barely used
positions_graph %>%
  as_tibble %>%
  group_by(is_supervisor) %>%
  count_prop(supervisory_factor_code)

### look at CDS (because it's what I know)
positions_graph %>%
  mutate(distance_to_position_of_interest = node_distance_to(position_gid == "TBD-2986")) %>%
  filter(distance_to_position_of_interest != Inf | branch_directorate_division == "Canadian Digital Ser")
#### what can we observe? that there are clearly some broken links in the chain: sometimes you can go by shared root node, but sometimes there are positions with the same branch/dir/div that are floating



## visualization
## NB: don't try to run on whole graph... unless you want to stress your computer
##     works best for orgs / clusters roughly <600
##     now if we could get a dynamic / zooming visualization...

positions_graph %>%
  filter(organization_code == "CIC") %>%
  plot()

(positions_graph %>%
    filter(organization_code == "GGS") %>%
  ggraph("igraph", algorithm = "kk") + # other layouts of note: stress tree igraph[algorithm = "nicely"] igraph[algorithm = "sugiyama"] igraph[algorithm = "fr"] igraph[algorithm = "kk"]    
  geom_edge_link(
    arrow = grid::arrow(type = "open", length = unit(9, "points"), angle = 7)
  ) +
  geom_node_point() +
  geom_point_interactive(aes(
    x = x,
    y = y,
    tooltip = str_glue("{position_title_english} ({group}-{level})\nBra/Dir/Div: {branch_directorate_division}\nPosition ID: {position_number}\nReports to: {supervisors_position_number}\nDirect reports: {reports_direct}\nIndirect reports: {reports_indirect}"),
    color = position_status
  ))
) %>%
  girafe(
    ggobj = .,
    options = list(opts_sizing(rescale = FALSE)),
    width_svg = 8,
    height_svg = 6
  )

vis_data <- positions_graph %>%
  filter(organization_code == "CIC") %>%
  toVisNetworkData()

visNetwork(
  nodes = vis_data$nodes %>%
    mutate(title = str_glue("{position_title_english} ({group}-{level})\nPosition ID: {position_number}\nReports to: {supervisors_position_number}")),
  edges = vis_data$edges,
  width = "100%"
) %>%
  visIgraphLayout() %>%
  visEdges(
    smooth = FALSE,
    arrows = "to"
  ) %>%
  visPhysics(stabilization = FALSE) %>%
  visInteraction(
    dragNodes = FALSE
  )

positions_graph %>%
  filter(organization_code == "CIC") %>%
  visIgraph()

