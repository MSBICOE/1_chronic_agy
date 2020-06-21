library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(broom)
library(officer)

library(tidygraph)
library(ggraph)

source("999_libs.r")

# set_graph_style() 

## get the data
cd_from <- read_csv("data/cd_from_name.csv") %>% 
  rename(image = image_from)


node_size <- cd_data %>% 
  group_by(cd_from) %>% 
  summarise(cases = sum(instance)) 

cd_from <- cd_from %>% 
  inner_join(node_size, by = c("cd_from_code" = "cd_from")) %>% 
  mutate(group = ifelse(selected_chronic == 1, "Main Chronic Disease", "Other Disease"),
         shape = ifelse(selected_chronic == 1, "circularImage", "circle")) %>% 
  select(id = ID, label = cd_from_code, group, title = cd_from_name,
         shape, value = cases, color = color_from, image)

write_csv(cd_from, "cd_vis_nodes.csv") 

########################################################

c4 <- brewer.pal(n = 4, name = "RdBu")

cd_node <- cd_from %>% 
  mutate(is_main = ifelse(group == "Main Chronic Disease", 1, 0)) %>% 
    select(cd = label, id, is_main)

cd_data <- read_csv("data/chronic_disease_with_variables_agegroup_2.csv")

cd_data.1 <- cd_data %>% 
  mutate(age_group_sort = case_when(age_group == "All Age" ~ 1,
                                    age_group == "< 40" ~ 2,
                                    age_group == "40 to 60" ~ 3,
                                    age_group == "> 60" ~ 4,
                                    TRUE ~ 7)) %>% 
  group_by(gender, age_group, develop_year) %>% 
  mutate(link = n(),
         ProbNorm = round(1 / (25 * 24) * 100, 3),
         cases = sum(instance),
         MU =  mean(instance),
         SD = sd(instance),
         prob = instance / cases * 100,
         z = (instance - MU) / SD,  ## (SD_overall / sqrt(link_overall))
         p = pt(z, df = link - 1, lower.tail = FALSE)
  ) %>% 
  ungroup() %>% 
mutate(prob = round(prob, 2),
       z = round(z, 3),
       p = round(p, 3),
       z_sig = case_when(p <= 0.001 ~ "Extremely Significant",
                         p <= 0.01 ~ "Very Significant",
                         p <= 0.05 ~ "Significant",
                         TRUE ~ "Not Significant"),
       z_order = case_when(p <= 0.001 ~ 1,
                           p <= 0.01 ~ 2,
                           p <= 0.05 ~ 3,
                           TRUE ~ 4),
       z_symbol = case_when(p <= 0.001 ~ "***",
                            p <= 0.01 ~ "**",
                            p <= 0.05 ~ "*",
                            TRUE ~ "--"),
       link_label = paste0("Prob: ", sprintf("%1.2f", prob), "% | p: ", sprintf("%1.3f", p), " | ", z_symbol),
       link_label_p = paste0("<p><b>", cd_from_to, "</b></p>","<p>P value: ", sprintf("%1.3f", p), " ", z_symbol, "</p>"),
       p_display = ifelse(p <= 0.001, 0.001, p),
       p_display_inverse = 1 / p_display,
       # link_width = case_when(z < 1.645 ~ 1,
       #                        z < 1.96 ~ 2,
       #                        z < 2.58 ~ 3,
       #                        TRUE ~ 4.5),
       link_width = case_when(p <= 0.001 ~ 4.5,
                              p <= 0.01 ~ 3,
                              p <= 0.05 ~ 1.5,
                              TRUE ~ 1),
       link_color = case_when(link_width == 1 ~ c4[1],   #"c4", # c4[1],
                              link_width == 1.5 ~ c4[2], ##"c3", #c4[2],
                              link_width == 3 ~ c4[3], ##"c2", # c4[3],
                              TRUE ~ c4[4]  #"c1" # c4[4]
                              # link_color = case_when(link_width == 0 ~ "c4", # c4[1],
                              #                        link_width == 1 ~ "c3", #c4[2],
                              #                        link_width == 2 ~ "c2", # c4[3],
                              #                        TRUE ~ "c1" # c4[4]
       )
) %>%
  filter(!is.nan(z)) %>%
  mutate(develop_year_order = case_when(develop_year == "All Years" ~ 1,
                                        develop_year == "Within 2 years" ~ 2,
                                        develop_year == "Within 5 years" ~ 3,
                                        TRUE ~ 7))



cd_links <- cd_data.1 %>% 
  inner_join(cd_node, by = c("cd_from" = "cd")) %>% 
  rename(from = id, node_1 = is_main) %>% 
  inner_join(cd_node, by = c("cd_to" = "cd")) %>% 
  rename(to = id, node_2 = is_main) %>% 
  mutate(main_node = node_1 + node_2,
         is_significant = ifelse(z_order < 4, 1, 0)) %>% 
  filter(main_node == 2) %>% 
  select(from, to,
         width = link_width,
         color = link_color,
         title = link_label_p,
         gender, age_group, develop_year,
         is_significant
           )



write_csv(cd_links, "cd_vis_links.csv") 



cd_gggraph <- cd_data.1 %>% 
  inner_join(cd_node, by = c("cd_from" = "cd")) %>% 
  rename(from = id, node_1 = is_main) %>% 
  inner_join(cd_node, by = c("cd_to" = "cd")) %>% 
  rename(to = id, node_2 = is_main) %>% 
  mutate(main_node = node_1 + node_2,
         is_significant = ifelse(z_order < 4, 1, 0)) %>% 
  filter(main_node == 2) %>% 
  mutate(width_color = case_when(link_width == 1 ~ "a",   #"c4", # c4[1],
                                link_width == 1.5 ~ "b", ##"c3", #c4[2],
                                link_width == 3 ~ "c", ##"c2", # c4[3],
                                TRUE ~ "d")) %>% 
  select(from = cd_from, to = cd_to,
         width = link_width,
         color = link_color,
         title = link_label_p,
         gender, age_group, develop_year,
         is_significant,
         cd_from_to,
         significance = z_sig,
         symbol = z_symbol,
         width_color
  )



write_csv(cd_gggraph, "cd_gggraph.csv") 
