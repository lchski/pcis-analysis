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
  select(
    supervisor_gid,
    supervisors_position_number,
    supervisors_position_classification_code,
    supervisor_group,
    supervisor_level,
    organization_code,
    organization,
    branch_directorate_division # TBD if there are any other fields we could include—even this is maybe a stretch (wewe )
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
  distinct(position_gid, .keep_all = TRUE)

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

positions_graph <- tbl_graph(
  nodes = position_nodes,
  edges = position_edges %>%
    filter(! is.na(from) & ! is.na(to))
) %>%
  mutate(
    reports_direct = local_size(mindist = 1, mode = "in"),
    reports_indirect = local_size(order = nrow(position_nodes), mindist = 1, mode = "in"),
    ranks_from_top = node_eccentricity(mode = "out"),
    is_isolated = node_is_isolated()
  )


## analysis

### study a particular organization
positions_graph %>%
  filter(organization_code == "TBD") %>%
  select(
    position_gid,
    grp_lvl = position_classification_code,
    position_title_english,
    sup_pos_gid = supervisor_gid,
    sup_grp_lvl = supervisors_position_classification_code,
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
    position_title_english,
    sup_pos_gid = supervisor_gid,
    sup_grp_lvl = supervisors_position_classification_code,
    reports_direct:last_col()
  ) %>%
  as_tibble %>% View()


## visualization
## NB: don't try to run on whole graph... unless you want to stress your computer

positions_graph %>%
  filter(organization_code == "CIC") %>%
  plot()

(positions_graph %>%
  filter(organization_code == "CIC") %>%
  ggraph("igraph", algorithm = "kk") + # other layouts of note: stress tree igraph[algorithm = "nicely"] igraph[algorithm = "sugiyama"] igraph[algorithm = "fr"] igraph[algorithm = "kk"]    
  geom_edge_link(
    arrow = grid::arrow(type = "open", length = unit(9, "points"), angle = 7)
  ) +
  geom_node_point() +
  geom_point_interactive(aes(
    x = x,
    y = y,
    tooltip = str_glue("{position_title_english} ({group}-{level})\nPosition ID: {position_number}\nReports to: {supervisors_position_number}")
  ))
) %>%
  girafe(
    ggobj = .,
    options = list(opts_sizing(rescale = FALSE))
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

