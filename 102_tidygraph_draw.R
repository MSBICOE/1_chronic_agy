library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(broom)
library(officer)

library(tidygraph)
library(ggraph)

source("999_libs.r")

set_graph_style() 

## get the data
cd_from <- read_csv("data/cd_from_name.csv")
dev_type <- read_csv("data/developing_type.csv")
cd_data <- read_csv("data/chronic_disease_with_variables_agegroup_new.csv") %>% 
  mutate(age_group = factor(age_group,
                            levels = c( "All Age", "< 40", "40 to 60", "> 60", "Within 2 years",  "Within 5 years",  "Within 10 years"),
                            labels = c( "All Age", "A1", "A2", "A3", "Y1",  "Y2",  "Y3")))

# str(cd_data)
# unique(cd_data$age_group)





cd_df <- cd_from %>% 
  filter(selected_chronic == 1) %>% 
  select(cd_from_code, cd_from_name)

d_uses <- c("Age Group", "Develop Years")
d_types <- c("overall", "from")


var_df_all <- expand.grid(cd = cd_df$cd_from_code, data_use = d_uses, develop_type = d_types) %>% 
  as_tibble(stringsAsFactors = FALSE) %>% 
  inner_join(cd_df, by = c("cd" = "cd_from_code")) %>% 
  inner_join(dev_type, by = c("develop_type" = "Type_Name"))

# View(var_df_all)

cd_use <- cd_from %>% 
  filter(selected_chronic == 1) %>% 
  pull(cd_from_code)


#########################################
## start doc

roc_report <- read_docx() %>%
  body_add_par(value = "Table of content", style = "toc 1") %>%
  body_add_par(value = "", style  = "Normal") %>% 
  body_add_toc(level = 2) %>%
  body_add_break()


for (dt in d_types) {
  # dt = "overall"
  
  var_df.1 <- var_df_all %>% 
    filter(develop_type == dt)
  
  develop_name <- unique(var_df.1$Developing_type)
  develop_desc <- unique(var_df.1$Notes)
  
  roc_report <- roc_report %>% 
    body_add_par(value = develop_name, style = "heading 1") %>% 
    body_add_par(value = "", style  = "Normal") %>% 
    body_add_par(value = develop_desc, style  = "Normal")

  
  for (du in d_uses) {
    # du = "Age Group"
    var_df <- var_df.1 %>% 
      filter(data_use == du)
    len <- dim(var_df)[1]
    
    head_2 <- paste("Difference on ", du, " by Gender")
    
    roc_report <- roc_report %>% 
      body_add_par(value = head_2, style = "heading 2") %>% 
      body_add_par(value = "", style  = "Normal")
    
    for (i in seq_along(1:len)) {
      # i = 1
      cd <- var_df$cd[i]
      d_use <- var_df$data_use[i] %>% as.character()
      d_type <- var_df$develop_type[i]
      
      cd_name <- ifelse(cd == "COPD", "COPD", var_df$cd_from_name[i])
      develop_name <- var_df$Developing_type[i]
      develop_desc <- var_df$Notes[i]
      
      g_df <- get_gg_data(cd_data, cd, cd_use, d_use, d_type)
      
      gg <- plot_gg(g_df, cd, cd_name, d_use, develop_name)    
      
      roc_report <- roc_report %>% 
        body_add_gg(value = gg, style = "centered") %>% 
        body_add_par(value = "", style  = "Normal") %>% 
        body_add_flextable(value = g_df[[3]])
    }
    
  }
}



print(roc_report, target = "disease_prograssion_differences_14.docx")

