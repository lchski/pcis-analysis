library(tidygraph)
library(ggraph)
library(ggiraph)
library(visNetwork)


# TODO: to run this on whole dataset, remap position_ids to be globally unique (prefix with department code)
cic_positions <- positions %>%
  filter(organization_code == "CIC") %>%
  mutate(node_id = row_number())

# TODO: where a supervisor's position doesn't exist (is.na(to)), create that position manually?
cic_position_edges <- cic_positions %>%
  select(
    position_number,
    supervisors_position_number
  ) %>%
  left_join(
    cic_positions %>%
      select(position_number, from = node_id)
  ) %>%
  left_join(
    cic_positions %>%
      select(position_number, to = node_id),
    by = c("supervisors_position_number" = "position_number")
  )

cic_positions_graph <- tbl_graph(
  nodes = cic_positions,
  edges = cic_position_edges %>%
    filter(! is.na(from) & ! is.na(to)) # need to account for situations where a supervisor ID doesn't correspond
)


## analysis

cic_positions_graph %>%
  activate(nodes) %>%
  mutate(
    reports_direct = local_size(mindist = 1, mode = "in"),
    reports_indirect = local_size(order = nrow(cic_positions), mindist = 1, mode = "in"),
    ranks_from_top = node_eccentricity(mode = "out")
  ) %>%
  select(position_number, sup_pos_num = supervisors_position_number, sup_grp_lvl = supervisors_position_classification_code, position_title_english, reports_direct:ranks_from_top) %>%
  as_tibble %>% View


## visualization

plot(cic_positions_graph)

(cic_positions_graph %>%
  ggraph("igraph", algorithm = "kk") + # stress tree igraph[algorithm = "nicely"] igraph[algorithm = "sugiyama"] igraph[algorithm = "fr"] igraph[algorithm = "kk"]    
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

vis_data <- toVisNetworkData(cic_positions_graph)

visNetwork(
  nodes = vis_data$nodes %>%
    mutate(title = str_glue("{position_title_english} ({group}-{level})\nPosition ID: {position_number}\nReports to: {supervisors_position_number}")),
  edges = vis_data$edges,
  main = cic_positions %>% distinct(organization) %>% pull,
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

cic_positions_graph %>%
  visIgraph()

