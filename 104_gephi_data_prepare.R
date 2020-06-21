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
  rename(image = image_from_2)

# dev_type <- read_csv("data/developing_type.csv")
cd_data <- read_csv("data/chronic_disease_data_for_gephi.csv")

node_size <- cd_data %>% 
  group_by(cd_from) %>% 
  summarise(cases = sum(instance)) 


cols <- names(cd_data)
cols_use <- setdiff(cols, c("cd_from", "cd_to"))

cd_links <- cd_data %>% 
  inner_join(cd_from, by = c("cd_from" = "cd_from_code")) %>% 
  rename(source = ID) %>% 
  select(source, cd_to, cols_use) %>% 
  inner_join(cd_from, by = c("cd_to" = "cd_from_code")) %>% 
  rename(target = ID) %>% 
  select(source, target, cols_use) %>% 
  rename(Weight = instance)

cd_from <- cd_from %>% 
  inner_join(node_size, by = c("cd_from_code" = "cd_from"))



write_csv(cd_from, "cd_nodes_1.csv") 

write_csv(cd_links, "cd_links.csv") 
