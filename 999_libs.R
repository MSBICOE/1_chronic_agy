library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(flextable)


get_gg_data <- function(df, cd, cd_use, d_use, d_type) {
    
  cd_graph_raw <- df %>% 
      complete(nesting(cd_from_to, cd_from), nesting(age_group, gender, data_use, develop_type)) %>% 
      mutate(cd_concern_1 = ifelse(cd_from %in% cd_use, 1, 0),
             cd_concern_2 = ifelse(cd_to %in% cd_use, 1, 0),
             cd_concern = cd_concern_1 + cd_concern_2
      ) %>% 
      filter(cd_concern == 2, cd_from == cd, data_use == d_use, develop_type == d_type) %>% 
      mutate(from = paste0(cd_from, gender),
             to = paste0(cd_to, gender)) %>% 
      select(from, to, instance, p_display, link_width, link_color, age_group, gender, cd_from, cd_to, cd_from_to, z_symbol)
    # filter(gender == "All Sex")
  
    
    
    ft_data <- cd_graph_raw %>% 
      filter(gender != "All Sex", age_group != "All Age")
    
    # age_gp <- c("Commorbidity", rep(as.character(sort(unique( ft_data $age_group))), 2))
    
    if (d_use == "Age Group") {
      cols <- c("F_A1", "F_A2", "F_A3", "M_A1", "M_A2", "M_A3")
      age_gp <- c("Commorbidity", rep(c("< 40", "40 to 60", "> 60"), 2))
    } else if (d_use == "Develop Years") {
      cols <- c("F_Y1", "F_Y2", "F_Y3", "M_Y1", "M_Y2", "M_Y3")
      age_gp <- c("Commorbidity", rep(c("<= 2 yrs", "<= 5 yrs", "<= 10 yrs"), 2))
    }
    
    
    ft_data <-  ft_data %>% 
      select(cd_from_to, age_group, gender, z_symbol) %>% 
      arrange(cd_from_to, gender, age_group) %>% 
      unite(sex_age, gender:age_group, sep = "_" ) %>% 
      pivot_wider(id_cols = cd_from_to, names_from = sex_age, values_from = z_symbol) %>% 
      select(cd_from_to, cols)
    
    
    colnames(ft_data) <- c("Disease", "c1", "c2", "c3", "c4", "c5", "c6")
    
  
    ft <- flextable(ft_data) 
    ft <- set_header_labels(ft, c1 = "Female", 
                            c2 = "Female", c3 = "Female",
                            c4 = "Male", c5 = "Male", c6 = "Male")
    ft <- merge_at(ft, i = 1, j = 2:4, part = "header")
    ft <- merge_at(ft, i = 1, j = 5:7, part = "header")
    
    ft <- add_header_row(ft, values = age_gp, top = FALSE)
    
    ft <- theme_box(ft)
    ft <- align(ft, j = 1, align = "left", part = "all")
    ft <- align(ft, j = 2:7, align = "center", part = "all")
    ft <- bold(ft, part = "header")
    ft <- fontsize(ft, part = "header", size = 9) %>% 
      fontsize(part = "body", size = 8) %>% autofit()
  
  # cd_df %>% filter(data_use == "Age Group") %>% View()
  # 
  #   
    node_from <- cd_graph_raw %>% 
      select(from, cd_from, gender, instance) %>% 
      group_by(from, cd_from, gender) %>% 
      summarise(instance = sum(instance))
    
    node_to <- cd_graph_raw %>% 
      select(from = to, cd_from = cd_to, gender, instance) %>% 
      group_by(from, cd_from, gender) %>% 
      summarise(instance = sum(instance))
    
    node <- bind_rows(node_from, node_to)
    
    cd_graph_raw.1 <- cd_graph_raw %>% 
      select(-gender, -cd_from, -cd_to, -cd_from_to, -z_symbol)
    
    color_index <- sort(4 - unique(cd_graph_raw.1$link_width))
    
    gg_cd <- as_tbl_graph(cd_graph_raw.1)
    
    gg_cd <- gg_cd %>% 
      activate(nodes) %>% 
      inner_join(node, by = c("name" = "from")) %>% 
      activate(edges) %>% 
      mutate(age_group = factor(age_group, 
                                levels = c( "All Age", "A1", "A2", "A3", "Y1",  "Y2",  "Y3"),
                                labels = c( "All Age", "< 40", "40 to 60", "> 60", "<= 2 years",  "<= 5 years",  "<= 10 years")))
    
    # labels = c( "All Age", "< 40", "40 to 60", "> 60", "Within 2 years",  "Within 5 years",  "Within 10 years")
    
    d_gg <- list()
    d_gg[[1]] <- gg_cd 
    d_gg[[2]] <- color_index
    d_gg[[3]] <- ft
    
    return(d_gg)
    # return(cd_graph_raw.1)

}


plot_gg <- function(g_df, cd, cd_name, data_use, develop_name) {
  
  tilte_name <- paste0(cd_name, " Developing Path Significance by Gender and ", data_use)
  
  labels <- c("***", "**", "*", "--")
  values <- c("#5CC96E", "#0571B0", "#F4A582", "white")
  
  edge_labels <- labels[g_df[[2]]]
  col_value <- values[g_df[[2]]]
  
  g_Plot <- g_df[[1]] %>% 
      ggraph(layout = "auto") +
      geom_node_point(aes(size = ifelse(cd_from == cd, 3, 2), color = ifelse(cd_from == cd, "a", "b")), alpha = 0.5) +
      geom_edge_link(aes(width = link_width, color = link_color, alpha = 0.6), 
                     arrow = arrow(length = unit(2, 'mm')), end_cap = circle(2, 'mm'),
                     show.legend = T) +
      geom_node_text(aes(label = cd_from, size = ifelse(cd_from == cd, 3, 2)), repel = T) +
      # scale_edge_width(range = c(0.5, 3)) +
      scale_edge_width(range = c(0.5, 1.5)) +
      # scale_radius(range = c(3, 5)) +
      scale_size(range = c(2, 3)) +
      scale_edge_color_manual(name = "Link Significance", labels = edge_labels, values = col_value) +
      scale_color_manual(name = "Node Color", labels = c("From Disease", "To Disease"), values = c("darkblue", "slategray3")) +
      guides(edge_width = "none", edge_alpha = "none", size = "none", color = "none") +
      # facet_edges(~ age_group) +
      facet_graph(age_group ~ gender, scale = "free", space = "free") +
      labs(title = tilte_name,
           subtitle = develop_name) + 
      th_foreground(foreground = 'grey80', border = TRUE) + 
      theme(plot.title = element_text(color = "darkblue", size = 10, hjust = 0.5),
            legend.title = element_text(color = "darkblue", size = 9),
            legend.text = element_text(color = "darkblue", size = 9),
            legend.position = "top",
            plot.subtitle = element_text(color = "darkblue", size = 9, hjust = 0.5),
            strip.text = element_text(face = "plain", size = 8))
  
  return(g_Plot)

}



## the below fond display is good for R visual
plot_gg_r <- function(g_df, cd, cd_name, data_use, develop_name) {
  
  tilte_name <- paste0(cd_name, " Developing Path Significance by Gender and ", data_use)
  
  g_Plot <- g_df %>% 
    ggraph(layout = "auto") +
    geom_node_point(aes(size = ifelse(cd_from == cd, 5, 3), color = ifelse(cd_from == cd, "a", "b")), alpha = 0.5) +
    geom_edge_link(aes(width = link_width, color = link_color, alpha = 0.6), 
                   arrow = arrow(length = unit(3, 'mm')), end_cap = circle(3, 'mm'),
                   show.legend = T) +
    geom_node_text(aes(label = cd_from, size = ifelse(cd_from == cd, 5, 3)), repel = T) +
    # scale_edge_width(range = c(0.5, 3)) +
    scale_edge_width(range = c(0.5, 3)) +
    # scale_radius(range = c(3, 5)) +
    scale_size(range = c(3, 5)) +
    scale_edge_color_manual(name = "Link Significance", labels = c("***", "**", "*", "+"), values = c("#5CC96E", "#0571B0", "#F4A582", "#CA0020")) +
    scale_color_manual(name = "Node Color", labels = c("From Disease", "To Disease"), values = c("darkblue", "slategray3")) +
    guides(edge_width = "none", edge_alpha = "none", size = "none", color = "none") +
    # facet_edges(~ age_group) +
    facet_graph(age_group ~ gender, scale = "free", space = "free") +
    labs(title = tilte_name,
         subtitle = develop_name) + 
    th_foreground(foreground = 'grey80', border = TRUE) + 
    theme(plot.title = element_text(color = "darkblue", size = 13, hjust = 0.5),
          legend.title = element_text(color = "darkblue", size = 12),
          legend.text = element_text(color = "darkblue", size = 13),
          legend.position = "top",
          plot.subtitle = element_text(color = "darkblue", size = 11, hjust = 0.5))
  
  return(g_Plot)
  
}



gg_plot <- function(gdata, title_name, edge_facet = "age_group", ratio_enlarge = 3) {
  f_facet <- as.formula(paste0("~ ", edge_facet))
  g <- ggraph(gdata) +
        geom_edge_link(aes(width = width, color = width_color, alpha = ifelse(width == 1, 0.01, 0.7)), 
                       arrow = arrow(length = unit(3, 'mm')), end_cap = circle(4, 'mm'),
                       show.legend = T) +
        geom_node_point(aes(size = value, color = color), alpha = 1) +
        geom_node_text(aes(label = name), size = 12, repel = T) +
        # geom_image(aes(x =x, y = y, image = image)) +
        # scale_edge_width(range = c(0.5, 3)) +
        scale_edge_width(range = c(1, 2)) +
        # scale_radius(range = c(3, 5)) +
        scale_size(range = c(3, 10)) +
        scale_edge_color_manual(name = "Link Significance", labels = edge_labels, values = col_value) +
        # scale_color_manual(name = "Node Color", labels = c("From Disease", "To Disease"), values = c("darkblue", "slategray3")) +
        guides(edge_width = "none", edge_alpha = "none", size = "none", color = "none") +
        facet_edges(f_facet) +
        # facet_graph(age_group ~ gender, scale = "free", space = "free") +
        labs(title = title_name) + 
        th_foreground(foreground = 'grey80', border = TRUE) + 
        theme(plot.title = element_text(color = "darkblue", size = 13 * ratio_enlarge, hjust = 0.5),
              legend.title = element_text(color = "darkblue", size = 10 * ratio_enlarge),
              legend.text = element_text(color = "darkblue", size = 10 * ratio_enlarge),
              legend.position = "top",
              # plot.subtitle = element_text(color = "darkblue", size = 30, hjust = 0.5),
              strip.text = element_text(face = "bold", color = "darkblue", size = 12 * ratio_enlarge),
              strip.background = element_blank())
  
  return(g)
}


gg_plot_1 <- function(gdata, title_name, edge_facet = "age_group", ratio_enlarge = 3) {
  f_facet <- as.formula(paste0("~ ", edge_facet))
  g <- ggraph(gdata) +
    geom_edge_link(aes(width = width, color = width_color, alpha = ifelse(width == 1, 0.01, 0.7)), 
                   arrow = arrow(length = unit(2, 'mm')), end_cap = circle(3, 'mm'),
                   show.legend = F) +
    geom_node_point(aes(size = value, color = color), alpha = 1) +
    geom_node_text(aes(label = name), size = 4 * ratio_enlarge, repel = T) +
    # geom_image(aes(x =x, y = y, image = image)) +
    # scale_edge_width(range = c(0.5, 3)) +
    scale_edge_width(range = c(0.5, 1.5)) +
    # scale_radius(range = c(3, 5)) +
    scale_size(range = c(2, 7)) +
    scale_edge_color_manual(name = "Link Significance", labels = edge_labels, values = col_value) +
    # scale_color_manual(name = "Node Color", labels = c("From Disease", "To Disease"), values = c("darkblue", "slategray3")) +
    guides(edge_width = "none", edge_alpha = "none", size = "none", color = "none") +
    facet_edges(f_facet) +
    # facet_graph(age_group ~ gender, scale = "free", space = "free") +
    # labs(title = title_name) + 
    th_foreground(foreground = 'grey80', border = TRUE) + 
    theme(plot.title = element_blank(), ### element_text(color = "darkblue", size = 36, hjust = 0.5),
          strip.text = element_text(face = "bold", color = "darkblue", size = 11 * ratio_enlarge),
          strip.background = element_blank())
  
  return(g)
}




gg_plot_2 <- function(gdata, title_name, edge_facet = "age_group", ratio_enlarge = 3) {
  f_facet <- as.formula(paste0("~ ", edge_facet))
  g <- ggraph(gdata) +
    geom_edge_link(aes(width = width, color = width_color, alpha = ifelse(width == 1, 0.01, 0.7)), 
                   arrow = arrow(length = unit(2, 'mm')), end_cap = circle(3, 'mm'),
                   show.legend = F) +
    geom_node_point(aes(size = value, color = color), alpha = 1) +
    geom_node_text(aes(label = name), size = 4 * ratio_enlarge, repel = T) +
    # geom_image(aes(x =x, y = y, image = image)) +
    # scale_edge_width(range = c(0.5, 3)) +
    scale_edge_width(range = c(0.5, 1)) +
    # scale_radius(range = c(3, 5)) +
    scale_size(range = c(2, 6)) +
    scale_edge_color_manual(name = "Link Significance", labels = edge_labels, values = col_value) +
    # scale_color_manual(name = "Node Color", labels = c("From Disease", "To Disease"), values = c("darkblue", "slategray3")) +
    guides(edge_width = "none", edge_alpha = "none", size = "none", color = "none") +
    facet_edges(f_facet) +
    # facet_graph(age_group ~ gender, scale = "free", space = "free") +
    # labs(title = title_name) + 
    th_foreground(foreground = 'grey80', border = TRUE) + 
    theme(plot.title = element_blank(),
          strip.text = element_text(face = "bold", color = "darkblue", size = 11 * ratio_enlarge),
          strip.background = element_blank())
  
  return(g)
}




# 
# 
# ft_data <- cd_data %>% 
#   mutate(cd_concern_1 = ifelse(cd_from %in% cd_use, 1, 0),
#          cd_concern_2 = ifelse(cd_to %in% cd_use, 1, 0),
#          cd_concern = cd_concern_1 + cd_concern_2
#   ) %>% 
#   filter(cd_concern == 2, cd_from == cd, data_use == d_use, develop_type == d_type) %>% 
#   mutate(from = paste0(cd_from, gender),
#          to = paste0(cd_to, gender)) %>% 
#   select(from, to, instance, p_display, link_width, link_color, age_group, gender, cd_from, cd_to, cd_from_to, z_symbol) %>% 
#   filter(gender != "All Sex", age_group != "All Age") %>% 
#   select(cd_from_to, age_group, gender, z_symbol) %>% 
#   unite(sex_age, gender:age_group, sep = "_" ) %>% 
#   pivot_wider(id_cols = cd_from_to, names_from = sex_age, values_from = z_symbol)
# 
# colnames(ft_data) <- c("Disease", "c1", "c2", "c3", "c4", "c5", "c6")
# 
# age_gp <- c("Commorbidity", rep(unique(xyz$age_group), 2))
# 
# ft <- flextable(ft_data) 
# 
# ft <- set_header_labels(ft, c1 = "Female", 
#                         c2 = "Female", c3 = "Female",
#                         c4 = "Male", c5 = "Male", c6 = "Male")
# 
# ft <- merge_at(ft, i = 1, j = 2:4, part = "header")
# ft <- merge_at(ft, i = 1, j = 5:7, part = "header")
# 
# ft <- add_header_row(ft, values = age_gp, top = FALSE)
#   
# ft <- theme_box(ft) %>% autofit()
#   
# ft <- align(ft, j = 1, align = "left", part = "all")
# 
# ft <- align(ft, j = 2:7, align = "center", part = "all")
# 
# ft <- bold(ft, part = "header")
# ft <- fontsize(ft, part = "header", size = 12) %>% autofit()



# cd_data %>% 
#   # select(cd_from_to, age_group, gender) %>% 
#   complete(cd_from_to, nesting(age_group, gender, data_use, develop_type)) %>% 
#   
#   View()
# 
# cols <- c("F_A1", "F_A2", "F_A3", "M_A1", "M_A2", "M_A3")
# cols <- c("F_Y1", "F_Y2", "F_Y3", "M_Y1", "M_Y2", "M_Y3")
# 
# cd_data %>% 
#   complete(nesting(cd_from_to, cd_from), nesting(age_group, gender, data_use, develop_type)) %>% 
#   mutate(cd_concern_1 = ifelse(cd_from %in% cd_use, 1, 0),
#          cd_concern_2 = ifelse(cd_to %in% cd_use, 1, 0),
#          cd_concern = cd_concern_1 + cd_concern_2
#   ) %>% 
#   filter(cd_concern == 2, cd_from == "C", data_use == "Develop Years",  develop_type == "overall") %>% 
#   mutate(from = paste0(cd_from, gender),
#          to = paste0(cd_to, gender)) %>% 
#   select(from, to, instance, p_display, link_width, link_color, age_group, gender, cd_from, cd_to, cd_from_to, z_symbol) %>% 
#   
#   
#   filter(gender != "All Sex", age_group != "All Age") %>% 
# 
#   select(cd_from_to, age_group, gender, z_symbol) %>% 
#   arrange(cd_from_to, gender, age_group) %>% 
#   unite(sex_age, gender:age_group, sep = "_" ) %>% 
#   pivot_wider(id_cols = cd_from_to, names_from = sex_age, values_from = z_symbol) %>% 
#   select(cd_from_to, cols)
#   