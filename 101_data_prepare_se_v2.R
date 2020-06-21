library(tidyverse)

library(RColorBrewer)

library(ggplot2)

library(gtools)


#############################
cd_selected <- c("AR", "AS", "BP", "C", "CVD", "COPD", "D", "MHC") 

cd_raw <- read_csv("data/chronic_disease_name.csv")

cd_selected <- c("AR", "AS", "BP", "C", "CVD", "COPD", "D", "MHC") 

cd_from <- cd_raw %>% 
  rename(cd_from_code = Chronic_Disease_Abbr,
         cd_from_name = Chronic_Disease) %>% 
  mutate(selected_chronic = ifelse(cd_from_code %in% cd_selected, 1, 0))

filter(cd_from, is_main_chronic == 1) %>% pull(cd_from_code)

# c("Arthritis", "Asthma", "Back Pain", "Cancer", "Cardiovascular Disease", "Chronic Kidney Disease", "Chronic Obstructive Pulmonary Disease", "Diabetes", "Mental Health Conditions", "Osteoporosis")        
# c("AR", "AS", "BP", "C", "CVD", "CKD", "COPD", "D", "MHC", "Osteo")


cd_selected <- c("AR", "AS", "BP", "C", "CVD", "COPD", "D", "MHC")      ## https://www.health.gov.au/health-topics/chronic-conditions/about-chronic-conditions

cd_to <- cd_raw %>% 
  rename(cd_to_code = Chronic_Disease_Abbr,
         cd_to_name = Chronic_Disease) %>% 
  mutate(selected_chronic = ifelse(cd_to_code %in% cd_selected, 1, 0))


# (arthritis, asthma, back pain, cancer, cardiovascular disease, chronic obstructive pulmonary disease, diabetes and mental health conditions)


display.brewer.all()

display.brewer.pal(n = 10, name = "RdBu")

col1 <- brewer.pal(n = 10, name = "RdBu")

display.brewer.pal(n = 10, name = "PiYG")

col2 <- brewer.pal(n = 10, name = "PiYG")

display.brewer.pal(n = 4, name = "RdBu")

link_from_color <- c(col1[1:5], col2[1:5], rep("#1E41B3", 15))

link_to_color <- c(col1[6:10], col2[6:10], rep("#B6C2E3", 15))


cd_from <- cd_from %>% 
  mutate(color_from = link_from_color,
         image_from = paste0("https://www.aiinsights.solutions.iqvia.com/images/disease/", cd_from_code, ".jpg"),
         image_from_2 = paste0("C:/Users/Steven Wang/OneDrive/Ph_Study/2_chronic_agegroup/image/", cd_from_code, ".jpg"))

write_csv(cd_from, "data/cd_from_name.csv")

cd_to <- cd_to %>% 
  mutate(color_to = link_to_color,
         image_to = paste0("https://www.aiinsights.solutions.iqvia.com/images/disease/", cd_to_code, ".jpg"),
         image_to_2 = paste0("C:/Users/Steven Wang/OneDrive/Ph_Study/2_chronic_agegroup/image/", cd_to_code, ".jpg"))

write_csv(cd_to, "data/cd_to_name.csv")

#################################################

cd_data <- read_csv("data/chronic_disease_with_variables_agegroup_2.csv")

# cd_data.1 <- filter(cd_data, age_group == "40 to 60", gender == "M", instance > 10)
# 
# cd_data.1 <- filter(cd_data, instance >= 1)
# 
# ggplot(cd_data.1, aes(x = sqrt(log2(instance)))) + geom_density() + facet_grid(age_group ~ gender)
# 
# ggplot(cd_data.1, aes(x = log2(instance))) + geom_density() + facet_grid(age_group ~ gender)
# 
# ggplot(cd_data.1, aes(x = log(log(instance)))) + geom_density() + facet_grid(age_group ~ gender)
# 
# 
# cd_data %>% 
#   # filter(instance >= 1) %>%
#   mutate(inst_log = log(instance),
#          inst_sqrt = sqrt(instance),
#          inst_log_sqrt = sqrt(inst_log)) %>%
#   # filter(inst_log > 0.5) %>%
#   group_by(gender, age_group) %>% 
#   nest() %>% 
#   mutate(normality = map(data, ~ shapiro.test(.x$instance)),
#          normality_tidy = map(normality, tidy)) %>% 
#   unnest(normality_tidy, .drop = TRUE)


# xx["data"][[1]][1] %>% 
#   filter(inst_log_sqrt > 0) %>% 

c4 <- brewer.pal(n = 4, name = "RdBu")

display.brewer.pal(n = 4, name = "RdBu")
# c4[4]


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
         # p_overall = 2 * (1 - pnorm(MU_overall, mean=instance, sd= SD_overall/ sqrt(link_overall))),
         # p_overall = 2 * pt(-abs(z_overall), df = link_overall - 1)
         p = pt(z, df = link - 1, lower.tail = FALSE)
         
         # inst_log = log(instance),
         # L_MU_overall =  mean(inst_log),
         # L_SD_overall = sd(inst_log),
         # L_z_overall = (inst_log - L_MU_overall) / L_SD_overall,
         # # p_overall = 2 * (1 - pnorm(MU_overall, mean=instance, sd= SD_overall/ sqrt(link_overall))),
         # p_overall = 2 * pt(-abs(L_z_overall), df = link_overall - 1)
  ) %>% 
  ungroup() %>% 
  # group_by(cd_from, gender, age_group, data_use) %>% 
  # mutate(link_from = n(),
  #        ProbNorm_from = round(1 / 25 * 100, 3),
  #        cases_from = sum(instance),
  #        MU_from =  mean(instance),
  #        SD_from = sd(instance),
  #        prob_from = instance / cases_from * 100,
  #        z_from = (instance - MU_from) / (SD_from / sqrt(link_from)), ## (SD_from / sqrt(link_from))
  #        # p_from = 2 * pt(-abs(z_from), df = link_from - 1)
  #        p_from = pt(z_from, df = link_from - 1, lower.tail = FALSE)
  # ) %>% 
  # ungroup() %>% 
  # pivot_longer(link_overall:p_from) %>% 
  # separate(name, into = c("metrics", "develop_type")) %>% 
  # pivot_wider(names_from = metrics, values_from = value) %>% 
  mutate(prob = round(prob, 2),
         z = round(z, 3),
         p = round(p, 3),
         odds = case_when(z < -1.96 ~ "Very Likely",
                          z > 1.96 ~ "Very Unlikely",
                          TRUE ~ "Neutral"),
         odds_order = case_when(z < -1.96 ~ 1,
                                z > 1.96 ~ 3,
                                TRUE ~ 2),
         odds_symbol = case_when(z < -1.96 ~ "+",
                                 z > 1.96 ~ "-",
                                 TRUE ~ "o"),
         # z_sig = case_when(z < 1.645 ~ "Not Significant",
         #                   z < 1.96 ~ "Significant",
         #                   z < 2.58 ~ "Very Significant",
         #                   TRUE ~ "Extremely Significant"),
         # z_order = case_when(z < 1.645 ~ 1,
         #                     z < 1.96 ~ 2,
         #                     z < 2.58 ~ 3,
         #                     TRUE ~ 4),
         # z_symbol = case_when(z < 1.645 ~ "ns",
         #                      z < 1.96 ~ "*",
         #                      z < 2.58 ~ "**",
         #                      TRUE ~ "***"),
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
         link_label = paste0("Prob: ", sprintf("%1.2f", prob), "% | p: ", sprintf("%1.3f", p), " | ", odds_symbol),
         link_label_p = paste0("p value: ", sprintf("%1.3f", p), " ", odds),
         d_type_order = 1,  # ifelse(develop_type == "overall", 1, 2),
         image_from = paste0("https://www.aiinsights.solutions.iqvia.com/images/disease/", cd_from, ".jpg"),
         image_to = paste0("https://www.aiinsights.solutions.iqvia.com/images/disease/", cd_to, ".jpg"),
         p_display = ifelse(p <= 0.001, 0.001, p),
         p_display_inverse = 1 / p_display,
         # link_width = case_when(z < 1.645 ~ 1,
         #                        z < 1.96 ~ 2,
         #                        z < 2.58 ~ 3,
         #                        TRUE ~ 4.5),
         link_width = case_when(p <= 0.001 ~ 3,
                                p <= 0.01 ~ 2,
                                p <= 0.05 ~ 1,
                                TRUE ~ 0),
         link_color = case_when(link_width == 0 ~ c4[1],   #"c4", # c4[1],
                                link_width == 1 ~ c4[2], ##"c3", #c4[2],
                                link_width == 2 ~ c4[3], ##"c2", # c4[3],
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
#   filter(data_use == "Age Group")
#  
# dim(cd_data.1)

# link_color = case_when(odds_order == 1 ~ "#255C11",
#                        odds_order == 3 ~ "#941212",
#                        TRUE ~ "#BFBA1B"

# View(cd_data.1)

write_csv(cd_data.1, "data/chronic_disease_with_variables_agegroup_new.csv")


# write_csv(cd_data.1, "data/chronic_disease_with_variables_agegroup_new_w.csv") # for officer use









